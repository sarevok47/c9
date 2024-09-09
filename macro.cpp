#pragma once

#include "lex.hpp"
#include "pp.hpp"
#include "token.hpp"
#include "target.hpp"
#include <memory>
#include <ranges>

namespace c9 { namespace pp {

lex::token preprocessor::get_token_from_stream_1(auto &streams, auto& ...s) {
  lex::token res;

  while(!res && streams.size())
    visit(streams.back(), [&](auto &stream) {
      if(stream.cur == stream.end()) {
        if(streams.end() - streams.begin() == 0) res = lex::padding{};
        return streams.pop_back();
      }
      res = *stream.cur;
      if constexpr(requires { stream.invoker; }) {
        if(stream.cur == stream.begin()) {
          res.prev_space    = stream.invoker.prev_space;
          res.start_of_line = stream.invoker.start_of_line;
        }
        if(stream.cur + 1 == stream.end())
          res.paste_left = stream.invoker.paste_left;

        if(bool(stream.macro->builtin)) [[unlikely]]
          res.loc = stream.invoker.loc;
        else {
          source_range source_range{stream.invoker.loc, stream.invoker.loc + 1};
          if constexpr(requires { stream.invoker_range; })
            source_range = stream.invoker_range;
          res.loc = d.loc_tab[macro_location{stream.cur->loc, source_range}];
        }
      }
      ++stream.cur;
    });

  return res;
}

lex::token preprocessor::get_token_from_stream(auto &streams, auto& ...s) {
  lex::token res = get_token_from_stream_1(streams, s...);
  if(res.paste_left) {
    std::string buf;

    buf += res.spelling();
    while(res.paste_left) {
      lex::token rhs = get_token_from_stream_1(streams, s...);
      buf += rhs.spelling();
      lex::lexer reader{d, lexer->src_file, buf.begin().base(), buf.end().base()};


      res = (lex::token_variant) reader.lex_token(flags::disable_loc | flags::paste_tokens);
      if(*reader.cur)
        d.diag(res.loc, "error"_s, "invalid concatenation of tokens {} and {}", res.spelling(), rhs.spelling());

      res.paste_left = rhs.paste_left;
    }
  }

  if(is<lex::identifier>(res)) ref_tuple(streams, s...)(for_each([&](auto& streams) {
    for(auto &s : streams)
      visit(s, overload {
        [&](auto &s) requires requires { s.macro; }  {
          res.no_expand |= (lex::identifier &) res == s.macro->name;
        },
        [](auto &) {}
      });
  }));

  return res;
}

lex::token &preprocessor::get_token_nomacro(lex::token &tok, lex::flags lexer_flags, auto &streams, auto&& ...s) {
start:
  if(temp) {
    tok = temp;
    temp.clear();
  } else {
    while(is<lex::padding>(tok = get_token_from_stream(streams ,s...)))
      ;
    if(!tok && &streams == &this->streams) {
      while((tok = lexer->lex_token(lexer_flags)) == "#"_s && tok.start_of_line) {
        directive();
        if(lexer->conditions.size() && lexer->conditions.back().skip)
          condition_skip();
      }

      if(!tok && lexers.size() > 1 && !(lexer_flags & flags::discard_next_line)) {
        lexers.pop_back();
        lexer = lexers.back().get();
        goto start;
      }
    }
  }
  ++lexer->result_token_count;
  return tok;
}

auto preprocessor::macro_invoker(lex::token tok) {
  macro_p m;
  if(is<lex::identifier>(tok) && !tok.no_expand)  {
    if(auto p = macro_table.find((lex::identifier &) tok); p != macro_table.end())
      m = p->second;
  }
  return m;
}


lex::token &preprocessor::get_token(lex::token &tok, lex::flags lexer_flags,auto &streams, auto&& ...s)  {
  auto next = [&] {
    return get_token_nomacro(tok, lexer_flags, streams, s...);
  };
  while(next()) {
    if(auto macro = macro_invoker(tok)) {
      auto t = tok;


      if(macro->is_funlike && next() != "("_s) {
        temp = tok;
        tok = t;
        break;
      }

      switch(macro->builtin) {
        case builtin_macro_type::bmt__LINE__: {
          char buf[std::numeric_limits<size_t>::digits10];
          lex::numeric_constant nc{{buf, std::to_chars(buf, std::end(buf), lexer->line).ptr } };
          tok = nc;
          break;
        }
        case builtin_macro_type::bmt__FILE__: {
          tok = lex::identifier{lexer->file_name};
          break;
        }
        case builtin_macro_type::bmt__has_include: {
          lex::token paren = next();
           if(paren != "("_s) {
             d.diag(tok.loc, "error"_s, "'(' expected after __has_include");
             temp = paren;
           }



           std::array data{ !handle_include_path([&](fs::path path, auto &&) { return path.empty(); })+ '0'};
           if(paren == "("_s && next() != ")"_s)
             d.diag(tok.loc, "error"_s, "')' expected after __has_include");
           tok = lex::numeric_constant{{data}};
          break;
        }
        // Not a builtin macro. Enter as normal
        default:
          if(enter_macro_stream(tok, t, macro, next, streams))
            continue;
      }
    }

    break;
  }
  return tok;
}

tokenbuf preprocessor::expand_arg(tokenbuf &unexpanded) {
  std::vector<stream> rs;
  rs.emplace_back(range_stream{unexpanded});
  tokenbuf expanded;
  for(lex::token temp; get_token(temp, lex::flags{}, rs,  streams); temp = lex::eof{} )
    expanded << temp;
  if(expanded.empty())
    expanded << lex::padding{};
  return expanded;
}

lex::string_literal preprocessor::stringify(tokenbuf &tokenbuf) {
  std::string buf = "\"";
  for(auto &tok : tokenbuf | iter_range) {
    if(tok != tokenbuf.begin() && (tok->prev_space || tok->start_of_line))
      buf += " ";
    buf += tok->spelling();
  }
  buf += "\"";
  return lex::string_literal{{buf.c_str()}};
}

bool preprocessor::enter_macro_stream(lex::token &tok, lex::token invoker, macro_p macro, auto &&next, auto& streams) {
  struct arg {
    tokenbuf unexpanded;
    std::optional<tokenbuf> expanded;
    std::optional<lex::string_literal> stringnized;
  };

  std::vector<arg> args;
  if(macro->is_funlike && !collect_args(tok, *macro, args, next))
    return false;

  c9_assert(args.size() == macro->noparams);

  source_range sr{invoker.loc, tok.loc + 1};

  if(tokenbuf fnbuf; macro->is_funlike) {
    struct {
      bool in;
      size_t count;
      size_t start_pos;
      lex::token va_opt;
    } va_opt_ctx{};
    for(auto &tok : macro->rlist | iter_range) {
      visit(*tok, overload {
        [&](lex::macro_arg a) {
          auto &arg = args[a.index];
          size_t first_pos = fnbuf.size();
          if(a.stringify)
            fnbuf << stringify(arg.unexpanded);
          else if(!tok->paste_left && (tok == macro->rlist.begin() || !tok[-1].paste_left)) {
            if(!arg.expanded)
              arg.expanded = expand_arg(args[a.index].unexpanded);
            fnbuf.insert(fnbuf.end(), arg.expanded->begin(), arg.expanded->end());
          } else {
            if(arg.unexpanded.empty())
              fnbuf << lex::padding{};
            fnbuf.insert(fnbuf.end(), arg.unexpanded.begin(), arg.unexpanded.end());
          }
          fnbuf[first_pos].prev_space = tok->prev_space;
          fnbuf.back().paste_left = tok->paste_left;
        },
        [&](lex::va_opt va_opt) {
          auto &variadic = args.back();
          if(!variadic.expanded)
            variadic.expanded = expand_arg(variadic.unexpanded);

          if(va_opt.stringify)
            fnbuf << stringify([&](lex::token &res) {  res = *++tok; return tok->last_va_opt_el;  });
          else if(is<lex::padding>(variadic.expanded->front())) {
            while(!tok->last_va_opt_el)
              ++tok;
            fnbuf << lex::padding{};
            return;
          } else {
            va_opt_ctx.in = true;
            va_opt_ctx.va_opt = *tok;
            va_opt_ctx.start_pos = fnbuf.size();
          }
        },
        [&](auto &&) {
          fnbuf << *tok;
        }
      });

      if(va_opt_ctx.in && va_opt_ctx.count++ == 1) [[unlikely]] {
      //  fnbuf[va_opt_ctx.start_pos].loc =        va_opt_ctx.va_opt.loc;
        fnbuf[va_opt_ctx.start_pos].prev_space = va_opt_ctx.va_opt.prev_space;
      }
      if(tok->last_va_opt_el) [[unlikely]] {
        fnbuf.back().paste_left = tok->paste_left;
        va_opt_ctx = {};
      }
    }
    streams.emplace_back(funlike_macro_stream{invoker, sr, macro, (mov(fnbuf))});
  } else
    streams.emplace_back(objlike_macro_stream{invoker, macro});

  return true;
}
bool preprocessor::collect_args(lex::token &tok, macro &macro, auto &&args, auto &&next) {
  size_t paren_blocks = 0;
  bool in_variadic = false;

  bool ok = true;

  if(next() && macro.noparams) {
    args.emplace_back();
    in_variadic = macro.is_variadic && macro.noparams == 1;
    do {
      if(tok == "("_s)
        ++paren_blocks;
      if(tok == ","_s && !paren_blocks && !in_variadic) {
        args.emplace_back();
        in_variadic = args.size() == macro.noparams && macro.is_variadic;
        continue;
      }
      if(tok == ")"_s) {
        if(!paren_blocks)
          break;
        --paren_blocks;
      }

      args.back().unexpanded << tok;
    } while(next());

    if(args.size() + 1 == macro.noparams && macro.is_variadic)
      args.emplace_back();
  }

  if(!(ok &= tok == ")"_s))
    d.diag(tok.loc, "error"_s, "unterminated args in macro invocation");
  else if(!(ok &= !(args.size() > macro.noparams)))
    d.diag(tok.loc, "error"_s, "too many arguments in macro invocation (given {}, while expected {})", args.size(), macro.noparams);
  else if(!(ok &= !(args.size() < macro.noparams)))
    d.diag(tok.loc, "error"_s, "too few arguments in macro invocation (given {}, while expected {})", args.size(), macro.noparams);

  return ok;
}


void preprocessor::macro_definition(macro macro) {
  auto next = [&] {
    return get_token_nomacro(tok, flags::discard_next_line | flags::warn_on_invalid_tok, streams);
  };

  if(!is<lex::identifier>(next())) {
    d.diag(tok.loc, "error"_s, "expect macro name");
    return;
  }

{
  macro.name = (lex::identifier) tok;
  std::vector<string> params;


  if(next() == "("_s && !tok.prev_space) {
    macro.is_funlike = true;
    do {
      lex::identifier id;

      if(is<lex::identifier>(next()) || tok == "..."_s) {
        if(is<lex::identifier>(tok))
          id = tok, next();
        if(tok == "..."_s) {
          macro.is_variadic = true;
          if(id.empty())
            id = {"__VA_ARGS__"};
          next();
        }
      } else if(!tok)
        return d.diag(tok.loc, "error"_s, "unterminated macro params");
      else if(tok == ")"_s)
        break;

      if(id.empty())
        return d.diag(tok.loc, "error"_s, "invalid macro param");
      if(std::ranges::any_of(params, [&](auto &s) { return s == id; }))
        return d.diag(tok.loc, "error"_s, "duplicate macro parameter '{}'", (string) id);

      params.emplace_back(id);
    } while(tok == ","_s);
    if(tok != ")"_s)
      return d.diag(tok.loc, "error"_s, "unterminated macro parameter list");
    next();
    macro.noparams = params.size();
  }


  if(tok == "##"_s) {
    d.diag(tok.loc,  "error"_s, "## cannot appear at start of macro");
    return;
  }

  auto indexof_param = [&](lex::token tok) -> size_t {
    if(is<lex::identifier>(tok))
      return std::find(params.begin(), params.end(), (string) (lex::identifier) tok) - params.begin();
    return params.size();
  };


  struct {
    bool in;
    size_t paren_depth ;

    size_t va_opt_tok_in_rlist_idx;
    lex::va_opt res;
  } va_opt_track{};

  for(; tok; next()) {
    if(macro.is_funlike) {
      lex::token hash = tok;
      if(tok != "#"_s)
        hash.clear();
      else
        next();

      if(size_t index = indexof_param(tok); index != params.size())
        tok = lex::macro_arg{index, bool(hash)};
      else if(macro.is_variadic && is<lex::identifier>(tok)
        && (lex::identifier &) tok == "__VA_OPT__"sv
      ) {
        auto t = tok;
        t = lex::va_opt{bool(hash)};
        if(va_opt_track.in) {
          d.diag(tok.loc, "error"_s, "nested __VA_OPT__");
          return;
        }
        if(next() != "("_s) {
          d.diag(tok.loc, "error"_s, "__VA_OPT__ must be followed by '('");
          return;
        }

        va_opt_track.in = true;
        macro.rlist << t;

        continue;
      } else if(hash)
        return d.diag(hash.loc, "error"_s, "'#' must be followed by macro parameter");
    }

    if(tok.inside_va_opt |= va_opt_track.in) {
      if(tok == "("_s)
        ++va_opt_track.paren_depth;
      if(tok == ")"_s) {
        if(!va_opt_track.paren_depth) {
          if(macro.rlist.size()
            && macro.rlist.back().inside_va_opt && macro.rlist.back().paste_left
          ) {
            auto loc= tok.loc;
            ++loc;
            return d.diag(loc, "error"_s, "'##' cannot appear at end of __VA_OPT__");
          }
          macro.rlist.back().last_va_opt_el = true;
          va_opt_track = {};
          continue;
        }
        --va_opt_track.paren_depth;
      }
    }


    if(tok == "##"_s)
      macro.rlist.back().paste_left = true;
    else
      macro.rlist << tok;
  }

  if(macro.rlist.size() && macro.rlist.back().paste_left) {
    d.diag(tok.loc, "error"_s, "'##' cannot appear at end of macro");
    return;
  }
  if(va_opt_track.in) {
    d.diag(tok.loc, "error"_s, "unterminated __VA_OPT__");
    return;
  }
  macro_table[macro.name] = make_macro(mov(macro));
}
}


void preprocessor::builtin_macro(string name, builtin_macro_type bmt) {
  macro_table[name] = macro { .builtin = bmt, .name = name };
}
void preprocessor::init_builtin_macro() {
  builtin_macro("__LINE__", builtin_macro_type::bmt__LINE__);
  builtin_macro("__FILE__", builtin_macro_type::bmt__FILE__);
  builtin_macro("__has_include", builtin_macro_type::bmt__has_include);
  builtin_macro("__has_builtin", builtin_macro_type::bmt__has_builtin);

  sv str = R"(
            #define _LP64 1
            #define __C99_MACRO_WITH_VA_ARGS 1
            #define __ELF__ 1
            #define __LP64__ 1
            #define __STDC_HOSTED__ 1
            #define __STDC_NO_COMPLEX__ 1
            #define __STDC_UTF_16__ 1
            #define __STDC_UTF_32__ 1
            #define __STDC_VERSION__ 201112L
            #define __STDC__ 1
            #define __GNUC__ 4
            #define __GNUC_MINOR__ 2
            #define __USER_LABEL_PREFIX__
            #define __alignof__ _Alignof
            #define __c9__ 1
            #define __const__ const
            #define __gnu_linux__ 1
            #define __inline__ inline
            #define __linux 1
            #define __linux__ 1
            #define __signed__ signed
            #define __typeof__ typeof
            #define __unix 1
            #define __unix__ 1
            #define __volatile__ volatile
            #define linux 1
            #define unix 1

#define __builtin_va_list int
#define __asm__(...)
            #define __has_feature(x) 0  // Compatibility with non-clang compilers.
            #define __has_attribute(x) 1
            #define __has_cpp_attribute(x) 1
    )";

  init_macro_from(str);
  init_macro_from(d.t.predefined_macro);
}

}}


