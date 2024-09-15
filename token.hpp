#pragma once

#include "variant.hpp"
#include "diagnostics.hpp"
#include "string.hpp"
#include <utility>
#include "tree.hpp"


namespace c9 {
  namespace sema {
    struct node_t {
      tree::decl decl; // typedef or variable name
      bool decl_implicit{};
      tree::struct_decl struct_decl;
      tree::union_decl union_decl;
    };


    struct id {
      string name;
      size_t level;
      node_t *node{};

      bool is_global_scope() { return !level; }
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

constexpr auto puncs = tuple(
  "<=>"_s, "["_s, "]"_s, "("_s, ")"_s, "{"_s, "}"_s, "."_s, "->"_s, "++"_s,
  "--"_s, "&"_s, "*"_s, "+"_s, "-"_s, "~"_s, "!"_s, "/"_s, "%"_s, "<<"_s, ">>"_s,
  "<"_s, ">"_s, "<="_s, ">="_s, "=="_s, "!="_s, "|"_s, "^"_s, "&&"_s, "||"_s, "?"_s, ":"_s,
  "::"_s, ";"_s, "..."_s, "="_s, "*="_s, "/="_s, "%="_s, "+="_s, "-="_s, "<<="_s, ">>="_s, "&="_s, ","_s, "^="_s, "|="_s, "#"_s, "##"_s
);

enum class keyword {
#define KEYWORD(kw) kw##_,
# include "keywords.def"
#undef KEYWORD
  kwno
};


enum class nc_prefix {
  decimal = 10,
  hex = 16,
  octal = 8,
  binary = 2
};

constexpr auto types_token(auto ...types) {
  return  puncs([](auto ...punc) {
    return type_c<variant<decltype(types)..., decltype(punc)...>>;
  });
}


using token_string = string;


struct eof { bool operator==(eof) { return true; }};
struct identifier       : token_string {};
struct string_literal   : token_string {};
struct angled_string    : token_string {};
struct char_literal     : token_string {};
struct numeric_constant : token_string {
  bool floating = false;
  bool exponent = false;
};
struct undefined        : token_string {};


struct macro_arg             { size_t index; bool stringify; };
struct va_opt                { bool stringify; };
struct padding  {};





using token_variant  = decltype(+types_token(eof{}, identifier{}, keyword{},
                                             string_literal{}, angled_string{}, char_literal{}, numeric_constant{}, undefined{},
                                             macro_arg{}, va_opt{}, padding{}, sema::id{}));


#define token_string sv

static token_string stringnize(keyword kw) {
  constexpr static sv ts[] = {
#define KEYWORD(kw) #kw,
# include "keywords.def"
#undef KEYWORD
  };

  return {ts[(size_t) kw].begin(), ts[(size_t) kw].end()};
}
static token_string stringnize(auto &str) requires requires {  string{str}; }  { return str; }
static token_string stringnize(auto &x) requires requires { x.c_str(); } { return x.c_str(); }
//static token_string stringnize(auto &) { c9_assert(0); }



struct token  : token_variant {
  using token_variant::token_variant;
  using token_variant::operator=;


  bool start_of_line  {};
  bool prev_space     {};
  bool paste_left     {};
  bool no_expand      {};
  bool variadic_exp   {};
  bool inside_va_opt  {};
  bool last_va_opt_el {};

  location_t  loc;
  token_string spelling() {
    return visit(*this, overload {
      [](auto &&) -> token_string { c9_assert(0 && "invalid token for stringnize"); },
      [](lex::padding) -> token_string { return {}; },
      [](auto &&x) requires requires { stringnize(x); } { return stringnize(x); }
    });
  }


  void clear() { *this = token{}; }

  explicit operator bool() { return !this->is<eof>();  }
};
#undef token_string
/* Scan token started from null terminated iterator p by given set of tokens toks which match given prefix.
 * When every symbol is scanned, the tokens set are narrowed according to matched token substring (named prefix)
 * Which used to match tokens in next stages.
 *
 * Assumed that compiler will optimize subroutine to usual compare + jump */
ALWAYS_INLINE void scan_impl(auto &&p, auto &out, auto toks, auto idx, auto prefix) {
  bool m{};

  toks((for_each([&](auto tok)  {
    if constexpr((idx < tok(size))() && sv{prefix.c_str(), idx()} == sv{tok.c_str(), idx()}) {
      if(!m && *p == tok[idx]()) {
        ++p;

        m = true;
        auto t = tok.sub(make_seq(idx + 1_c));
        if constexpr( toks(contains(t)))
          out = t;

        scan_impl(p, out, toks, idx + 1_c, t);
      }
    }
  })));
}

ALWAYS_INLINE void scan_impl(auto &&p, auto finish, auto &out, auto toks, auto idx, auto prefix) {
  bool m{};

  toks((for_each([&](auto tok)  {
    if constexpr((idx < tok(size))() && sv{prefix.c_str(), idx()} == sv{tok.c_str(), idx()}) {
      if(!m && p != finish && *p == tok[idx]()) {
        ++p;

        m = true;
        auto t = tok.sub(make_seq(idx + 1_c));
        if constexpr( toks(contains(t)))
          out = t;

        scan_impl(p, out, toks, idx + 1_c, t);
      }
    }
  })));
}

bool forms_token(const token_variant &, const token_variant &);
token_variant scan(const char *&p);


enum binary_prec {
  none,
  logor, logand, bitor_, bitxor_,
  bitand_, rel, rel_1, three_way,
  shift, term, factor, num_of
};

template<class Key, class F> struct binop {
  Key key;
  F f;
  binary_prec prec;
};

constexpr auto binary_op_tab = tuple(
#define _(op, prec)  /*std::pair*/ binop { \
  #op ## _s, [](auto x, auto y) requires requires { x op y; } { return x op y; }, prec \
}
  _(&, binary_prec::bitand_),
  _(*, binary_prec::factor),
  _(+, binary_prec::term),
  _(-, binary_prec::term),
 // _(<=>, binary_prec::three_way),
  _(/, binary_prec::factor),
  _(%, binary_prec::factor),
  _(<<, binary_prec::shift),
  _(>>, binary_prec::shift),
  _(<, binary_prec::rel_1),
  _(>, binary_prec::rel_1),
  _(<=, binary_prec::rel_1),
  _(>=, binary_prec::rel_1),
  _(==, binary_prec::rel),
  _(!=, binary_prec::rel),
  _(^, binary_prec::bitxor_),
  _(|, binary_prec::bitor_),
  _(&&, binary_prec::logand),
  _(||, binary_prec::logor)
);
}}


