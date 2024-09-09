#pragma once

#include <deque>

#include "lex.hpp"
#include "lex-conv.hpp"
#include <unordered_map>
#include <ranges>
#include <list>


namespace c9 { namespace pp {
using lex::flags;
struct tokenbuf : std::vector<lex::token> {
  tokenbuf &operator<<(lex::token tok) {
    this->emplace_back(mov(tok));
    return *this;
  }
};
inline fs::path find_include_file(sv s, const auto& ...dirs) {
  std::array p{rv::single(dirs)...};

  for(auto &path : p | rv::join | rv::join) {
    if(fs::is_regular_file(path/s))
      return path/s;
  }

  return {};
}

enum class builtin_macro_type {
  none,
  predefined,
  bmt__FILE__,
  bmt__LINE__,
  bmt__has_include,
  bmt__has_builtin
};
struct macro {
  bool is_funlike  ;
  bool is_variadic ;
  builtin_macro_type builtin;

  size_t noparams{};

  tokenbuf rlist;

  string name;

  bool is_varag(lex::macro_arg a) {
    return is_variadic && a.index == noparams - 1;
  }
};

using macro_p = shared_ptr<macro>;

static macro_p make_macro(macro m) { return mov(m); }


struct range_stream {
  refw<tokenbuf> buf;
  tokenbuf::iterator cur = buf.get().begin();

  auto begin() { return buf.get().begin(); }
  auto end() { return buf.get().end(); }
};
struct objlike_macro_stream {
  lex::token invoker;

  macro_p macro;

  tokenbuf::iterator cur = macro->rlist.begin();

  auto begin() { return macro->rlist.begin(); }
  auto end() { return macro->rlist.end(); }
};
struct funlike_macro_stream {
  lex::token invoker;
  source_range invoker_range;

  macro_p macro;


  // share buffer between preprocessor and location table
  shared_ptr<tokenbuf> buf;
  tokenbuf::iterator cur = buf->begin();

  auto begin() { return buf->begin(); }
  auto end() {   return buf->end(); }
};




using stream =  variant<objlike_macro_stream, funlike_macro_stream, range_stream>;


struct pp_num {
  uint64_t value{};
  bool unsigned_{};

  pp_num &operator=(uint64_t v) { value = v; return *this; }
  operator uint64_t() { return value; }
};

class preprocessor {
  std::vector<std::unique_ptr<lex::lexer>> lexers;
  std::vector<stream> streams;

  std::unordered_map<string, macro_p, string::hash> macro_table;

  driver &d;

  lex::lexer *lexer;

  lex::token tok, temp;


  void copy_flags(lex::token &tok, auto cur, auto &ctx) {
    if(cur == ctx.begin()) {
      tok.prev_space    = ctx.invoke.prev_space;
      tok.start_of_line = ctx.invoke.start_of_line;
    }
    if(cur + 1 == ctx.end())
      tok.paste_left = ctx.invoke.paste_left;
  }

  lex::string_literal stringify(tokenbuf &buf);
  lex::string_literal stringify(auto &&next)  {
    std::string buf = "\"";
    lex::token tok;
    size_t cnt = 0 ;
    for(; next(tok); ++cnt) {
      if(cnt && (tok.prev_space || tok.start_of_line))
        buf += " ";
      buf += tok.spelling();
    }
    buf += "\"";
    return lex::string_literal{{buf.c_str()}};
  }

  tokenbuf expand_arg(tokenbuf &unexpanded);

  auto macro_invoker(lex::token tok);

  lex::token &get_token_nodirect(lex::flags lexer_flags) {
    if(temp) [[unlikely]] {
      tok = temp;
      temp.clear();
    } else
      return tok = lexer->lex_token(lexer_flags);
  };



  lex::token get_token_from_stream_1(auto &streams, auto& ...s);
  lex::token get_token_from_stream(auto &streams, auto& ...s);

  lex::token &get_token_nomacro(lex::token &tok,  lex::flags flags, auto &streams, auto&& ...s);
  lex::token &get_token(lex::token &tok, lex::flags flags, auto &streams, auto&& ...s);

  bool enter_macro_stream(lex::token &tok, lex::token invoker, macro_p macro, auto &&next, auto& streams);
  bool collect_args(lex::token &tok, macro &macro, auto &&args, auto &&next);



  bool primary(pp_num &);
  bool binary (pp_num &, size_t prec);
  bool ternary(pp_num &);
  bool expr   (pp_num &);



  void skip_pp_line();
  auto handle_include_path(auto &&f) {
    return visit(get_token(flags::discard_next_line | flags::angled_string), overload {
      [&](lex::string_literal slit) {
        if(slit.front() != '\"') {
          d.diag(tok.loc, "error"_s, "include path of form <FILENAME> or \"FILENAME\" expected");
          return f("", "");
        }
        return f(find_include_file(sv(slit).substr(1, slit.size() - 2), std::vector{lexer->src_file.path.parent_path()}, d.opt.include_paths, d.opt.system_include_paths), slit);
      },
      [&](lex::angled_string astr) {
        sv s =  sv(astr).substr(1, astr.size() - 2);
        return f(find_include_file(s, d.opt.system_include_paths), astr);
      },
      [&](decltype("<"_s)) {
        std::string s = "<";
        while(get_token(flags::discard_next_line) && tok != ">"_s) {
          if(tok.prev_space) s += " ";
          s += tok.spelling();
        }
        if(tok != ">"_s)
          d.diag(tok.loc, "error"_s, "closing '>' expected");
        s += ">";
        return f(find_include_file(sv(s).substr(1, s.size() - 2), d.opt.system_include_paths), s);
      },
      [&](auto &) {
        d.diag(tok.loc, "error"_s, "include path of form <FILENAME> or \"FILENAME\" expected");
        return f("", "");
      }
    });
  }
  void line_control();
  void condition_skip();
  bool condition_directive();
  void macro_definition(macro macro = {});
  void process_include(fs::path path);
  void handle_pragma();
  void handle_include();
  void directive();



  void builtin_macro(string name, builtin_macro_type bmt) ;
  void init_builtin_macro();
public:
  lex::token &get_token(lex::flags flags) { return get_token(tok, flags, streams); }
  void output_line(FILE *out) {
    auto pos = lexer->buf;

    lex::token prev;
    while(get_token({})) {
     if(tok.start_of_line) {
       fputc('\n', out);
       while(pos != lexer->limit && is_space(*pos))
         fputc(*pos++, out);
       pos = lexer->buf;
      } else if(tok.prev_space || forms_token(prev, tok))
        fputc(' ', out);


      fprint(out, "{}", tok.spelling());

      prev = tok;
    }
  }


  void init_macro_from(sv str) {
    file file {
      str.begin(), str.end()
    };
    lex::lexer tmp{d, file, file.buf, file.limit};
    auto prev = lexer;
    lexer = &tmp;

    while(get_token_nomacro(tok, {}, streams) == "#"_s)
      macro_definition({.builtin = builtin_macro_type::predefined });
  }

  const auto &get_lexer() { return lexer; }


  preprocessor(driver &d, file &main) : d{d} {
    init_builtin_macro();
    lexers.emplace_back(std::make_unique<lex::lexer>(d, main));
    lexer = lexers.back().get();
  }

};

}}

