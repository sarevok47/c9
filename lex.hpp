#pragma once

#include "meta.hpp"
#include "token.hpp"
#include "driver.hpp"
#include "char-set.hpp"

namespace c9 {
namespace pp {
enum class cond_flags {
  expr   = 1 << 0,
  def    = 1 << 1,
  neg    = 1 << 2,
  if_    = 1 << 3,
  else_  = 1 << 4,
  endif_ = 1 << 5
};

constexpr bool operator&(cond_flags lhs, cond_flags rhs) { return  (size_t) lhs & (size_t) rhs; }
constexpr cond_flags operator|(cond_flags lhs, cond_flags rhs) { return cond_flags((size_t) lhs | (size_t) rhs); }

struct cond_entry {
  lex::token cond_tok;
  cond_flags flags;


  bool skip       = false;
  bool true_entry = false;
};

}

namespace lex {
inline flags operator|(flags lhs, flags rhs) { return flags((size_t) lhs | (size_t) rhs); }
inline bool  operator&(flags lhs, flags rhs) { return (size_t) lhs & (size_t) rhs; }

static const char *search_end_of_phase2_line(const char *s, const char *end) {
  auto N = 16_c;

  using vec [[gnu::vector_size(N())]] = char;

  return make_seq(N)([&](auto ...i) -> const char * {
    if(s >= end)
      return nullptr;

    static vec bslash = {((void) i, '\\')...};
    static vec pline  = {((void) i, '\n')...};

    for(; s + N() < end; s += N()) {
      vec data = { s[i()]... };
      vec mask = (data == pline) | (data == bslash);
#ifdef __SSE2__
       size_t found = __builtin_ia32_pmovmskb128(mask);
       found &= (size_t) -1;
       if(found)
         return s + __builtin_ctz(found);
#else
      for(size_t i = 0; i < N(); ++i)
        if(mask[i]) return s + i;
#endif
    }

    while(s < end && *s != '\n' && *s != '\\')
      ++s;
    return s;
  });
}


class lexer {
public:
  const char *cur = "";

  file &src_file;
  driver &d;

  string file_name;
  size_t line = 0;

  std::string fresh_line;

  std::vector<pp::cond_entry> conditions;
public:
  const char *buf;
  const char *physical_start; // Physical line is every line following after \n symbol, including backslash newlines.

  const char *phase2_start;   // Each fresh line is a phase 2 line instace
  const char *limit;

  size_t result_token_count{};
  opt<string> miopt_name;
   // Curr align incremented by backslash newlines used to mapping pointer in real source position.
  size_t align;

  // Positions of backslashes in buf
  std::vector<size_t> line_notes;
  std::vector<size_t>::iterator cur_note = line_notes.end();

  driver &get_driver() { return d; }

  bool clean_line();
  size_t process_line_notes(const char *pos);
  lex::token lex_number(lex::token &tok);

  location_t map_instance(flags flags, const char *start, const char *finish);
  bool next_line();
  token lex_token(flags flags);
  // In macro expansion we don't give any errors about unterminated string literals
  template<class ...T> void invalid_tok(location_t loc, flags flags, std::format_string<T...> fmt, T&& ...args) {
    flags & flags::warn_on_invalid_tok
    ? report(flags, loc, "warning"_s, fmt, (decltype(args)) args...)
    : report(flags, loc, "error"_s, fmt, (decltype(args)) args...);
  }
  template<class lit> void lex_string(flags flags, auto &&tok, auto start) {
    c9_assert(!__is_same(angled_string, lit) || *cur == '<');

    for(char terminator = *cur == '<' ? '>' : *cur; *++cur != terminator; ) {
      if(!*cur) {
        location_t loc = map_instance(flags, start, cur);
        invalid_tok(loc, flags,
                    "unterminated {}", terminator == '\"'
        ? "string" : terminator == '\'' ? "char constant"
        : "angled include name");
        tok = undefined{{start, cur}};
        return;
      }

      if(*cur == '\\' && !__is_same(lit, angled_string))
        ++cur;
    }
    ++cur;
    if(validate_start(cur))
      while(validate_cont(cur));
      tok = lit{{start, cur}};
  }

  template<class ...T> void report(flags flags, location_t loc, message_type mtype, std::format_string<T...> fmt, T&& ...args) {
    if(!(flags & flags::disable_loc))
      report(flags, loc, mtype, fmt, (decltype(args)) args...);
  }
  lexer(driver &d, file &src_file, auto buf, auto limit)
    : d{d}, src_file{src_file}, buf{buf}, limit{limit}, file_name{src_file.path.c_str()} {
      d.loc_tab.next_line({
        file_name, line, src_file, physical_start
      });
    }

  lexer(driver &d, file &src_file)
    : d{d}, src_file{src_file}, buf{src_file.buf}, limit{src_file.limit}, file_name{src_file.path.c_str()} {
      d.loc_tab.next_line({
        file_name, line, src_file, physical_start
      });
    }
};
}}

