#pragma once

#include "pp.hpp"
#include "lex-conv.hpp"
#include "switch.hpp"



namespace c9 { namespace pp {


bool preprocessor::primary(pp_num &value) {
  return visit(get_token(flags::discard_next_line), overload {
    [&](auto &) {
      d.diag(tok.loc, "error"_s, "primary expected");
      return false;
    },
    [&](decltype("+"_s)) {
      bool r = primary(value);
      value = +value;
      return r;
    },
    [&](decltype("-"_s)) {
      bool r = primary(value);
      return value = -value, r;
    },
    [&](decltype("!"_s)) {
      bool r = primary(value);
      return value = !value, r;
    },
    [&](decltype("~"_s)) {
      bool r = primary(value);
      return value = ~value, r;
    },
    [&](decltype("("_s)) {
      bool ok = expr(value);
      if(!(ok &= get_token(flags::discard_next_line) == ")"_s))
        d.diag(tok.loc, "error"_s, "')' expected");
      return ok;
    },
    [&](lex::char_literal &cl) {
      variant_t<""_s, "U"_s, "u"_s, "u8"_s, "L"_s, "L"_s> prefix;

      auto s = cl.begin();
      lex::scan_impl(s, prefix, variant_types(prefix), 0_c, ""_s);
      ++s;
      value.unsigned_ = true;
      value.value = *s;
      return true;
    },
    [&](lex::numeric_constant nc) {
      lex::interpret_status stat;
      auto v = lex::interpret_nc(nc, stat);


      return visit(v, overload {
        [&](lex::integer int_) {
          bool ok = true;
          if(stat == lex::interpret_status::out_of_range) {
            d.diag(tok.loc, "error"_s, "too large integer constant appears in the preprocessor expression");
            ok = false;
          } else {
            value = int_.value;
            value.unsigned_ = visit(int_.suffix, overload {
              [&](lex::udl_string) {
                d.diag(tok.loc, "error"_s, "too large integer constant appears in the preprocessor expression");
                ok = false;
                return false;
              },
              [&](lex::integer_suffix suffix) {
                return visit(suffix, [&](auto s) {
                  return sv{s.c_str()}.contains('u') || sv{s.c_str()}.contains('U');
                });
              }
            });
          }
          return ok;
        },
        [&](lex::floating) {
          d.diag(tok.loc, "error"_s, "floating point constant cannot appear in the preprocessor expression");
          return false;
        }
      });
    },
    [&](lex::identifier &id) -> bool {
      bool ok = true;
      case_switch{id}
        ("defined", [&] {
          lex::token mid = get_token_nomacro(tok, flags::discard_next_line, streams);
          if(mid == "("_s) {
            mid = get_token_nomacro(tok, flags::discard_next_line, streams);

            if(get_token_nomacro(tok, flags::discard_next_line, streams) != ")"_s) {
              d.diag(tok.loc, "error"_s, "')' expected after defined");
              return ok = false;
            }
          }

          if(!is<lex::identifier>(mid)) {
            d.diag(tok.loc, "error"_s, "macro name must be an identifier");
            return ok = false;
          }

          value = macro_table.find((lex::identifier &) mid) != macro_table.end();
          return ok = true;
        })
        ("true", [&] {
          value = 1;
        })
        .default_([&] {
          value = 0;
        });

      return true;
    }
  });
}

bool preprocessor::binary(pp_num &value, size_t prec) {
  if(prec == lex::binary_prec::num_of)
    return primary(value);

  if(!binary(value, prec + 1))
    return false;

  for(;;) {
    location_t loc = get_token(flags::discard_next_line).loc;
    if(!lex::binary_op_tab(contains_if([&](auto x) {
      if constexpr(x.key != "<=>"_s) {
        if(prec == x.prec && tok == x.key) {
          pp_num rhs;
          binary(rhs, prec + 1);
          value.unsigned_ |= rhs.unsigned_;

          if((x.key == "/"_s || x.key == "%"_s) && rhs == 0) {
            d.diag(loc, "error"_s, "divide by 0 is not permitted in preprocessor expression");
            return false;
          }

          if(!value.unsigned_)
            value = x.f((int64_t) value.value, (int64_t) rhs.value);
          else
            value = x.f(value.value, rhs.value);

          return true;
        }
      }
      return false;
    })))
      break;
  }
      temp = tok;
  ++prec;

  return true;
}

bool preprocessor::ternary(pp_num &value) {
  bool ok = binary(value, 1);

  if(get_token(flags::discard_next_line) == "?"_s) {
    bool cond = value.value;

    pp_num true_, false_;
    if(!expr(true_))
      return false;
    if(get_token(flags::discard_next_line) != ":"_s) {
      d.diag(tok.loc, "error"_s, "':' expected in ternary preprocessor expression");
      return false;
    }
    if(!ternary(false_))
      return false;

    value = cond ? true_ : false_;
  } else
      temp = tok;

  return ok;
}
bool preprocessor::expr(pp_num &value) {
  return ternary(value);
}

} }
