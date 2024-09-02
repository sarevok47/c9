#pragma once

#include "tree.hpp"

namespace c9 { namespace tree {
type_name make_pointer(auto type) {
  return type_name{{.type = pointer_t{.type = type}}};
}

namespace make_unsigend_detail {
auto operator>(auto signed_, auto unsigned_) { return [=](decltype(signed_)::value_type &) { return unsigned_; }; }
}
auto make_unsigned(auto tree) {
  using namespace make_unsigend_detail;
  return overload {
    char_type_node > unsigned_char_type_node,
    signed_char_type_node > unsigned_char_type_node,
    short_type_node > unsigned_short_type_node,
    int_type_node > unsigned_int_type_node,
    long_type_node > unsigned_long_type_node,
    long_long_type_node > unsigned_long_long_type_node
  }(tree);
}

type_decl strip_type(auto type) {
  return type(overload {
    [](typedef_decl_t &t)  { return strip_type(t.type); },
    [](type_name_t    &t)  { return strip_type(t.type); },
    [&](auto &)            { return type; }
  });
}
bool is_incomplete_type(type_decl type) {
  return strip_type(type)(overload {
    [](narrow<structural_decl_t> auto &decl) -> bool { return !decl.definition; },
    [](auto &) { return false; }
  });
}
/* integer promotions are applied only:
 * as part of usual arithmetic conversions (tree-trait.hpp::usual_arith_conv)
 * as part of default argument promotions
 * to the operand of the unary arithmetic operators + and - (semantics.hpp::build_unary_expression)
 * to the operand of the unary bitwise operator ~ (semantics.hpp::build_unary_expression) */
template<narrow<integer_type_t> Tree> integer_type promote_int(Tree type) {
  if(type.rank < int_type_t::rank) return int_type_node;
  else return type_node<Tree>;
}

type_decl promote(type_decl type) {
  return type(overload {
    [](narrow<integer_type_t> auto &type) -> type_decl { return promote_int(type); },
    [&](auto &) -> type_decl { return type;}
  });
}
// 6.3.1.8 Usual arithmetic conversions
type_decl usual_arith_conv(auto lhs, auto rhs) {
  auto one_is = []<class T>(T) {
    return overload {
      [](T, T)     -> type_decl  { return type_node<T>; },
      [](T, auto)  -> type_decl  { return type_node<T>; },
      [](auto, T)  -> type_decl  { return type_node<T>; }
    };
  };
  return overload {
    prioritizied_overload(one_is(void_type_t{}),
                          one_is(long_double_type_t{}),
                          one_is(double_type_t{}),
                          one_is(float_type_t{})),
    /* Otherwise, both operands are integers. Both operands undergo integer promotions (see below);
       then, after integer promotion, one of the following cases applies: */
    []<narrow<integer_type_t> L, narrow<integer_type_t> R>(L l, R r) {
      return prioritizied_overload(
        [](L, R) -> type_decl requires (L::rank < int_type_t::rank && R::rank < int_type_t::rank) {
          return int_type_node;
        },
        // If the types are the same, that type is the common type.
        [](L, L) -> type_decl { return type_node<L>; },
        /* Otherwise, if both operands have signed integer types or both have unsigned integer
         * types, the operand with the type of lesser integer conversion rank is converted to the type
         * of the operand with greater rank. */
        [](L l, R r) requires (l.is_unsigned() && r.is_unsigned()) || (l.is_signed() && r.is_signed())  {
            return l.size >= r.size ? type_decl{type_node<L>} : type_decl{type_node<R>};
          },
        /* Otherwise, if the operand that has unsigned integer type has rank greater or equal to
         * the rank of the type of the other operand, then the operand with signed integer type is
         * converted to the type of the operand with unsigned integer type.
         *
         * Otherwise, if the type of the operand with signed integer type can represent all the values
         * of the type of the operand with unsigned integer type, then the operand with unsigned
         * integer type is converted to the type of the operand with signed integer type.
         *
         * Otherwise, both operands are converted to the unsigned integer type corresponding to
         * the type of the operand with signed integer type. */
        [](L l, R r) -> type_decl {
          constexpr constant<requires { l.is_unsigned(); }> unsigned_in_lhs{};
          auto u = if_(unsigned_in_lhs, l, r);
          auto s = if_(unsigned_in_lhs, r, l);

          if(u.rank >= s.rank)
            return type_node<decltype(u)>;
          if(s.size >= s.size)
            return type_node<decltype(s)>;
          return type_node<decltype(u)>;
        }
      )(l, r);
    }
  }(lhs, rhs);
}


template<class L, class R> bool is_compatible(lex::binary_tok op, L &&lhs, R &&rhs) {
  return visit(op, overload {
    /* Constraints
     * 1 For addition, either both operands shall have arithmetic type, or one operand shall be a pointer to a
     * complete object type and the other shall have integer type. (Incrementing is equivalent to adding 1.)
     * 2 For subtraction, one of the following shall hold:
     * — both operands have arithmetic type;
     * — both operands are pointers to qualified or unqualified versions of compatible complete object
     * types; or
     * — the left operand is a pointer to a complete object type and the right operand has integer type. */
    [&](auto s) requires (s == "+"_s || s == "-"_s) {
      return prioritizied_overload(
        [s](pointer_t &, pointer_t &) { return s == "-"_s; },
        [](auto &, auto &) requires requires {lhs.is_scalar(); rhs.is_scalar(); } { return true; },
        [](auto &, auto &) { return false; }
      )(lhs, rhs);
    },
    /* Constraints
     * 1 Each of the operands shall have arithmetic type. */
    [](auto s) requires (s == "*"_s || s == "/"_s) {
      return requires { lhs.is_arithmetic(); rhs.is_arithmetic(); };
    },
    /* Constraints
     * 1 One of the following shall hold:
     * — both operands have real type; or
     * — both operands are pointers to qualified or unqualified versions of compatible object types. */
    [](auto s) requires (tuple("<"_s, "<="_s, ">"_s, ">="_s, "=="_s, "!="_s)(contains(s))) {
      if(narrow<L, structural_decl_t> && narrow<R, structural_decl_t>)
        return true;
      return requires { lhs.is_scalar(); rhs.is_scalar(); };
    },
    /* Constraints
     * 1 Each of the operands shall have integer type. */
    [](auto s) requires (tuple("%"_s, "|"_s, "&"_s, "^"_s, "<<"_s, ">>"_s)(contains(s))) {
      return requires { lhs.is_integer(); rhs.is_integer();};
    },
    /* Constraints
     * 1 Each of the operands shall have scalar type.*/
    [](auto s) requires (s == "||"_s || s == "&&"_s) {
      return requires {lhs.is_scalar(); rhs.is_scalar(); };
    }
  });
}
template<class L, class R> bool is_compatible(lex::assign_tok op, L &lhs, R &rhs) {
  return visit(op, overload {
    [&](auto s) {
      return is_compatible(s.sub(make_seq(s.size() - 1_c)), lhs, rhs);
    },
    /* Constraints
     * 1 One of the following shall hold125):
     * — the left operand has atomic, qualified, or unqualified arithmetic type, and the right operand
     * has arithmetic type;
     * — the left operand has an atomic, qualified, or unqualified version of a structure or union type
     * compatible with the type of the right operand;
     * — the left operand has atomic, qualified, or unqualified pointer type, and (considering the type
     * the left operand would have after lvalue conversion) both operands are pointers to qualified
     * or unqualified versions of compatible types, and the type pointed to by the left operand has all
     * the qualifiers of the type pointed to by the right operand;
     * — the left operand has atomic, qualified, or unqualified pointer type, and (considering the type
     * the left operand would have after lvalue conversion) one operand is a pointer to an object type,
     * and the other is a pointer to a qualified or unqualified version of void, and the type pointed to
     * by the left operand has all the qualifiers of the type pointed to by the right operand; */
    [&](decltype("="_s)) {
      return overload{
        [](auto &, auto &) requires requires {lhs.is_arithmetic(); rhs.is_arithmetic(); } { return true; },
        []<narrow<structural_decl_t> T>(T &lhs, T &rhs) { return &lhs == &rhs; },
        [](pointer_t &, pointer_t &) { return true; },
        [](auto &, auto &) { return false; }
      }(lhs, rhs);
    }
  });
}


}}
