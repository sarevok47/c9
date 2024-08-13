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
enum class flags : uint8_t {
  discard_next_line   = 1 << 0,
  warn_on_invalid_tok = 1 << 1,
  angled_string       = 1 << 2,
  pragma_string       = 1 << 3,
  disable_loc         = 1 << 4,
  paste_tokens        = 1 << 5
};
inline flags operator|(flags lhs, flags rhs) { return flags((size_t) lhs | (size_t) rhs); }
inline bool  operator&(flags lhs, flags rhs) { return (size_t) lhs & (size_t) rhs; }

const char *search_end_of_phase2_line(const char *s, const char *end) {
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


  // Copy next line concateneted with lines after backslashes
  bool clean_line() {
    for(; cur_note != line_notes.end(); ++cur_note)
      ++line;

    line_notes.clear();

    auto eol = physical_start = phase2_start = buf;

    align = 0;

    fresh_line.clear();

    if(buf >= limit) [[unlikely]]
      return false;
    else
      while((eol = search_end_of_phase2_line(eol, limit))) {
        if(eol == limit || *eol == '\n') {
          d.loc_tab.next_line({
            file_name, line, src_file, physical_start
          });
          fresh_line.append(buf, eol);
          buf =  ++eol;
          break;
        } else if(*eol == '\\') {
          auto bl_process = [&] {
            fresh_line.append(buf, eol);
            line_notes.push_back(eol - phase2_start);
          };

          if(eol + 1 == limit || eol[1] == '\n') {
            bl_process();
            buf = eol += 2;
            continue;
          }
          if(isspace(eol[1])) [[unlikely]]
            for(auto cur = eol + 1; ; ++cur) {
              if(cur == limit || *cur == '\n') {
                bl_process();
                buf =  eol = cur + 1 ;
                goto continue_;
              } else if(!isspace(*cur))
                break;
            }

            ++eol;

        continue_:
          continue;
        } else
          break;
      }


    cur = fresh_line.c_str();
    cur_note = line_notes.begin();
    return true;
  }

  /* Process the notes created by add_line_note as far as the current
   * location. */
  size_t process_line_notes(const char *pos) {
    size_t idx = pos - fresh_line.c_str();

    for(; cur_note != line_notes.end() && idx + align >= *cur_note; ++cur_note) {
      auto slash = phase2_start + *cur_note + 1;
      if(*slash != '\n') [[unlikely]]
        for(; slash != limit && *slash != '\n'; ++slash)
          ++align;
      ++line;
      physical_start = slash + 1;
      align += 2; // \\\n
      d.loc_tab.next_line({file_name, line, src_file, physical_start});
    }

    return phase2_start + idx + align - physical_start;
  }


  auto lex_number(auto &&tok) {
    auto start = cur;

    bool floating{}, exponent{};
    for(;;) {
      if(validate_cont(cur))
        ;
      else if(((*cur == '+' || *cur == '-')
          && (cur[-1] == 'e'
            || cur[-1] == 'E'
            || cur[-1] == 'p'
            || cur[-1] == 'P'))
      ) {
        floating = exponent = true;
        ++cur;
      } else if(*cur == '.' && (cur - 1 == start || cur[-1] != '.')) {
        floating = true;
        ++cur;
      } else if(*cur == '\'')
        ++cur;
      else
        break;
    }

    tok = numeric_constant{ {start, cur}, floating, exponent};
    return tok;
  }

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


  location_t map_instance(flags flags, const char *start, const char *finish) {
    if(!(flags & flags::disable_loc))
      return d.loc_tab[location{d.loc_tab.line_maps.size() - 1, process_line_notes(start), process_line_notes(finish)}];
    return {};
  }


public:
  bool next_line() {
    ++line;

    for(; buf < limit && *buf == '\n'; ++buf)
      ++line;

    return clean_line();
  }

  token lex_token(flags flags = {}) {
    token tok;
  start:
    tok.start_of_line |= cur == fresh_line.c_str();
    for(; isspace(*cur); tok.prev_space = true)
      ++cur;

    auto start = cur;

    switch(*cur) {
      case '\0':
        if(flags & flags::discard_next_line || !next_line()) {
          tok.loc = map_instance(flags, cur, cur + 1);
          tok = eof{};
          return tok;
        }
        goto start;
      case '.':
        if(!isdigit(cur[1])) {
          tok = scan(cur);
          break;
        }
        [[fallthrough]];
      case '0' ... '9':
        lex_number(tok);
        break;
      case '/':
        if(!(flags & flags::paste_tokens) && cur[1] == '/') {
          while(*cur && *cur != '\n')
            ++cur;
          goto start;
        }
        if(!(flags & flags::paste_tokens) && cur[1] == '*') {
          location_t start_comment_loc = map_instance(flags, cur, cur + 2);

          for(;;)
            if(*cur == '*' && cur[1] == '/') {
              cur += 2;
              break;
            } else if(*cur) {
              ++cur;
            } else if(!next_line()) {
              report(flags, start_comment_loc, "error"_s, "unterminated /* comment");
              break;
            }
          goto start;
        }

        tok = scan(cur);

        break;
      case '<':
        if(flags & flags::angled_string) [[unlikely]] {
          lex_string<angled_string>(flags, tok, start);
          return tok;
        }

        tok = scan(cur);
        break;

      default:
        variant_t<""_s, "U"_s, "u"_s, "u8"_s, "L"_s, "L"_s> prefix;

        auto s = cur;
        scan_impl(cur, prefix, variant_types(prefix), 0_c, ""_s);

        if(*cur == '\"') {
          lex_string<string_literal>(flags, tok, start);
          break;
        } else if(*cur == '\'') {
          lex_string<char_literal>(flags, tok, start);
          break;
        } else
          cur = s;


        if(validate_start(cur)) {
         while(validate_cont(cur))
            ;
          tok = identifier{{start, cur}};
          break;
        }
        tok = scan(cur);
        if(tok == eof{}) {
          ++cur;
          tok = undefined{{start, cur}};
          break;
        }
      break;
    }

    tok.loc = map_instance(flags, start, cur);
    return tok;
  }


  driver &get_driver() { return d; }


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

