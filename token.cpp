#include "token.hpp"


namespace c9 { namespace lex {

token_variant scan(const char *&p) {
  token_variant tok;
  scan_impl(p, tok, puncs, 0_c, ""_s);
  return tok;
}

bool forms_token(const token_variant &lhs, const token_variant &rhs) {
  return visit(lhs, rhs, overload {
    [](auto &, auto &) { return false; },
    [](lex::identifier &, lex::identifier &) { return true; },
    []<char ...c, char ...c1>(string_seq<c...>, string_seq<c1...>)
    requires requires { lex::token{string_seq<c..., c1...>{}}; } { return true; }
  });
}



}
}
