#pragma once

#include "pp.hpp"
#include "switch.hpp"
#include <set>
namespace c9 { namespace pp {
void preprocessor::condition_skip() {
  c9_assert(lexer->conditions.size());

  while(get_token_nodirect(flags::warn_on_invalid_tok)) {
    if(tok.start_of_line && tok == "#"_s) {
      if(!is<lex::identifier>(get_token_nodirect(flags::warn_on_invalid_tok | flags::discard_next_line))) {
        temp = tok;
        continue;
      }
      condition_directive();
      skip_pp_line();
    }

    if(lexer->conditions.empty() || !lexer->conditions.back().skip)
      return;
  }

  d.diag(lexer->conditions.back().cond_tok.loc, "error"_s, "unterminated condition directive");
}

bool preprocessor::condition_directive() {
  lex::token dtok = tok;
  constexpr static struct cond_directive { sv str; cond_flags flags; } tab[] = {
    {"if", cond_flags::if_ | cond_flags::expr },
    {"ifdef", cond_flags::if_ | cond_flags::def},
    {"ifndef", cond_flags::if_ | cond_flags::def | cond_flags::neg },
    {"elif", cond_flags::else_ | cond_flags::expr },
    {"elifdef",  cond_flags::else_ | cond_flags::def },
    {"elifndef", cond_flags::else_ | cond_flags::def | cond_flags::neg },
    {"else", cond_flags::else_ },
    {"endif", cond_flags::endif_ }
  };

  auto dp = std::ranges::find(tab, (sv)(lex::identifier &) dtok, &cond_directive::str);
  if(dp == std::end(tab))
    return false;


  bool inside_skipping = lexer->conditions.size() ? lexer->conditions.back().skip : false;

  if(dp->flags & cond_flags::if_) {
    lexer->conditions.emplace_back(dtok, dp->flags, inside_skipping, inside_skipping);
    if(inside_skipping)
      return true;
  } else {
    if(lexer->conditions.empty()) {
      d.diag(dtok.loc, "error"_s, "#{} without previous #if", dp->str);
      return true;
    }

    lexer->conditions.back().cond_tok = dtok;

    if(dp->flags & cond_flags::else_)
      lexer->conditions.back().skip ^= true;
    else {
      lexer->conditions.pop_back();
#ifdef MULTIPLE_INCLUDE_OPTIMIZATION
      if(lexer->conditions.empty() && lexer->buf == lexer->limit)
        lexer->src_file.miopt_name = lexer->miopt_name;
#endif
      return true;
    }
  }

  if(!lexer->conditions.back().true_entry) {
    bool eval = dp->flags == cond_flags::else_ ;

    if(dp->flags & cond_flags::def) {
      if(!is<lex::identifier>(
        get_token_nodirect(flags::warn_on_invalid_tok | flags::discard_next_line)
      ))
        d.diag(tok.loc, "error"_s, "macro name in #{} directive expected", dp->str);

      eval = macro_table.find((lex::identifier &) tok) != macro_table.end();
#ifdef MULTIPLE_INCLUDE_OPTIMIZATION
      if(dp->flags & cond_flags::neg && lexer->result_token_count == 1)
        lexer->miopt_name = (lex::identifier &) tok;
#endif
    } else if(dp->flags & cond_flags::expr) {
      pp_num r{};
      eval = expr(r) && r;
      if(temp) {
        eval = false;
        d.diag(temp.loc, "error"_s, "stray {} at the end of preprocessor expression", temp.spelling());
        temp.clear();
      }
    }

    if(dp->flags & cond_flags::neg)
      eval ^= true;
  c9_assert(lexer->conditions.size());
    lexer->conditions.back().skip = !(lexer->conditions.back().true_entry =  eval);
  } else
    lexer->conditions.back().skip = true;


  return true;
}

void preprocessor::process_include( fs::path path) {
  auto &file = d.files[path];

  if(file.miopt_name && macro_table.find(*file.miopt_name) != macro_table.end())
    return;

  lexers.push_back(std::make_unique<lex::lexer>(d, file));

  lexer = lexers.back().get();
}


inline fs::path find_include_file(sv s, const auto& ...dirs) {
  std::array p{rv::single(dirs)...};

  for(auto &path : p | rv::join | rv::join) {
    if(fs::is_regular_file(path/s))
      return path/s;
  }

  return {};
}
auto preprocessor::handle_include_path(auto &&f) {
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

void preprocessor::handle_include() {
  auto prev = lexer;
  if(lexers.size() == 199)
    d.diag(tok.loc, "error"_s, "limit of 200 nested includes reached");
  else {
    location_t loc = tok.loc;

    handle_include_path([&](fs::path path, sv tok) {
      if(!path.empty())
        process_include(path);
      else
        d.diag(loc, "error"_s, "no matching '{}' for include", tok);
    });

  }
//skip_pp_line();
  prev->next_line();
}



ALWAYS_INLINE void preprocessor::line_control() {
  if(!is<lex::numeric_constant>(tok))
    return d.diag(tok.loc, "error"_s, "line number expected in #line directive");

  lex::numeric_constant nc = tok;
  size_t line;
  auto [s, ec] = std::from_chars(nc.begin(), nc.end(), line, 10);


  if(ec == std::errc::result_out_of_range)
    return d.diag(tok.loc, "error"_s, "numeric in line directive out of range");
  if(s != nc.end())
    return d.diag(tok.loc, "error"_s, "line control numeric must be a simple digit sequence");

  c9_assert(ec == std::errc());

  lexer->line = line;
  if(is<lex::string_literal>(get_token(flags::discard_next_line)))
    lexer->file_name = (lex::string_literal) tok;


  skip_pp_line();
}

void preprocessor::skip_pp_line() {
  while(get_token_nomacro(tok, flags::discard_next_line | flags::warn_on_invalid_tok, streams))
    ;
}

void preprocessor::handle_pragma() {

}

void preprocessor::directive() {
  visit(get_token_nomacro(tok, flags::discard_next_line, streams), overload {
    [&](lex::eof) {
      skip_pp_line();
    },
    [&](lex::identifier ident) {
      case_switch{ident}
        ("define", [&] {
          macro_definition();
          skip_pp_line();
        })
        ("include", [&] {
          handle_include();
        })
        ("undef", [&] {
          if(!is<lex::identifier>(get_token_nomacro(tok, flags::discard_next_line, streams)))
            return d.diag(tok.loc, "error"_s, "invalid macro name in #undef");
          macro_table.erase((lex::identifier &) tok);
          skip_pp_line();
        })
        ("error", [&] {
          auto s = stringify([&](lex::token &tok) {
            return get_token_nomacro(tok, flags::discard_next_line | flags::warn_on_invalid_tok, streams);
          });
          d.diag(tok.loc, "error"_s, "{}", sv(s).substr(1, s.size() - 2));
          skip_pp_line();
        })
        ("warning", [&] {
          auto s = stringify([&](lex::token &tok) {
            return get_token_nomacro(tok, flags::discard_next_line | flags::warn_on_invalid_tok, streams);
          });
          d.diag(tok.loc, "warning"_s, "{}", sv(s).substr(1, s.size() - 2));
          skip_pp_line();
        })
        ("line", [&] {
          get_token_nomacro(tok, flags::discard_next_line, streams);
          line_control();
        })
        ("pragma", [&] {
          handle_pragma();
        })
        .default_([&] {
          if(!condition_directive())
            d.diag(tok.loc, "error"_s, "unknown preprocessor directive '{}'", (string) ident);
          skip_pp_line();
        });
    },
    [&](lex::numeric_constant &) {
      line_control();
    },
    [](auto &&) {

    }
  });
}



}}
