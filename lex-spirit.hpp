// Token stream interface for parser
// The name and idea is influenced by "Boost spirit" library
#pragma once

#include "lex.hpp"
#include "sema.hpp"

namespace c9 { namespace parse {
using lex::keyword;


template<class T> struct require_value { T value; };
template<class T, T ...c> auto operator""_req() { return require_value{string_seq<c...>{}}; }

struct lex_spirit {
  struct parser &parse;
  pp::preprocessor &pp;
private:


  sema::id lookup(lex::identifier s);

  lex::token peek[2];
  lex::token get_token() {
    lex::token tok = pp.get_token({});;
    if(is<lex::identifier>(tok)) {
      sv s = (lex::identifier &) tok;
      constexpr static std::pair<sv, lex::keyword> kws[] = {
#define KEYWORD(x) {#x, keyword::x##_},
#include "keywords.def"
      };


      if(auto p = std::ranges::find(kws, s, &std::pair<sv, lex::keyword>::first);
         p != std::end(kws)
      )
         tok = p->second;
      else
        tok = lookup(tok);
    }

    return tok;
  }

public:
  lex::token peek_token() { return peek[0]; }
  lex::token peek_2nd_token() {
    if(!peek[1])
      peek[1] = get_token();
    return peek[1];
  }
  void consume() {
    if(peek[1]) [[unlikely]] {
      peek[0] = peek[1];
      peek[1].clear();
    } else
      peek[0] = get_token();
  }

  lex_spirit(parser &parse, pp::preprocessor &pp) : parse{parse}, pp{pp} { consume(); }

  bool operator<=(auto f) requires requires { f(*this); } { return f(*this); }


  bool operator<=(keyword kw) {
    if(peek_token() == kw) {
      consume();
      return true;
    } else
      return false;
  }
  template<class T> bool operator<=(require_value<T> v);

  template<char ...c> bool operator<=(string_seq<c...> ss) {
    if(peek_token() == ss) {
      consume();
      return true;
    } else
      return false;
  }



};



auto operator,(auto &&lexeme, auto &&handler) {
  return [&](lex_spirit &ls) {
    if(ls <= lexeme) {
      if constexpr(requires { {handler() } -> std::same_as<bool>; }) return handler();
      else handler();
      return true;
    }
    return false;
  };
}
template<class T> auto operator,(type_<T> tc, auto handler) {
  return [=](lex_spirit &ls) {
    if(is<T>(ls.peek_token())) {
      if constexpr(requires { {handler() } -> std::same_as<bool>; }) return handler(ls.peek_token());
      else handler(ls.peek_token());
      return true;
    }
    return false;
  };
}

auto operator|(auto lexeme, auto lexeme_1) {
  return [=](lex_spirit &ls) { return ls <= lexeme || ls <= lexeme_1; };
}


auto operator>>(auto lexeme, auto lexeme_1) {
  return [=](lex_spirit &ls) { return ls <= lexeme && ls <= lexeme_1; };
}

auto operator/(auto f, auto &r) {
  return [=, &r](lex_spirit &ls) { return (ls.parse.*f)(r); };
}
auto operator%(auto f, auto &r) {
  return [=, &r](lex_spirit &ls) {
    r = (ls.parse.*f)();
    return true;
  };
}


auto operator+(auto lexeme) {
  return [=](lex_spirit &ls) {
    bool t{};
    while(ls <= lexeme) t = true;
    return t;
  };
}
auto operator*(auto lexeme) {
  return [=](lex_spirit &ls) {
    if(ls <= lexeme)
      ls <= +lexeme;
    return true;
  };
}
auto operator-(auto lexeme) {
  return [=](lex_spirit &ls) {
    ls <= lexeme;
    return true;
  };
}



}}
