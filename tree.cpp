#include "tree-trait.hpp"
#include "token.hpp"
namespace c9 { namespace tree {
bool operator==(type_decl lhs, type_decl rhs) {
  return visit(strip_type(lhs), strip_type(rhs), overload {
    [](pointer_t &lhs, pointer_t &rhs) { return lhs.type == rhs.type; },
    [](type_name_t &lhs, type_name_t &rhs) {
      return lhs.is_const == rhs.is_const && lhs.is_volatile == rhs.is_volatile
            && lhs.is_restrict == rhs.is_restrict && lhs.type == rhs.type;
    },
    [&](tree::function_type_t &lhs, tree::function_type_t &rhs) {
      if(strip_type(lhs.return_type) == strip_type(rhs.return_type))
        return lhs.is_variadic == rhs.is_variadic && std::ranges::equal(lhs.params, rhs.params, [&](auto &lhs, auto &rhs) {
          return strip_type(lhs.type) == strip_type(rhs.type);
        });
      return false;
    },
    []<class T>(T &lhs, T &rhs) { return &lhs == &rhs; },
    [](auto &, auto &) { return false; },
  });
}

type_decl get_decl_type(decl decl) {
  return decl(overload {
    [](auto &) -> type_decl { c9_assert(false); },
    [](auto &decl) requires requires { decl.type; } { return decl.type; }
  });
}
type_decl strip_type(tree::type_decl type) {
  return type(overload {
    [](typedef_decl_t &t)  { return strip_type(t.type); },
    [](type_name_t    &t)  { return strip_type(t.type); },
    [](enum_decl_t    &t)  { return strip_type(t.type); },
    [&](auto &)            { return type; }
  });
}
bool is_incomplete_type(type_decl type) {
  return strip_type(type)(overload {
    [](narrow<structural_decl_t> auto &decl) -> bool { return !decl.definition; },
    [](auto &) { return false; }
  });
}
type_decl promote(type_decl type) {
  return type(overload {
    [](narrow<integer_type_t> auto &type) -> type_decl { return promote_int(type); },
    [&](auto &) -> type_decl { return type;}
  });
}
bool operator==(tree::op lhs, tree::op rhs) {
  return visit(lhs, rhs, overload {
    [](tree::cst_t rhs, tree::cst_t lhs) { return lhs.data == rhs.data; },
    [](tree::temporary_t lhs, tree::temporary_t rhs) { return lhs.idx == rhs.idx; },
    [](tree::variable_t &lhs, tree::variable_t &rhs) { return &lhs == &rhs; },
    [](tree::ssa_variable_t lhs, tree::ssa_variable_t rhs) { return lhs.var.get_data() == rhs.var.get_data() && lhs.ssa_n == rhs.ssa_n; },
    [](auto &, auto &) { return false; }
  });
}
cst_t do_cast(cst_t value, arithmetic_type type) {
  cst_t r;
  r.type = type;
  visit(value.data, [&](auto value) {
    type(overload {
      [](auto) { c9_assert(0); },
      [&](float_type_t)       { r.data = (long double) (float) value; },
      [&](double_type_t)      { r.data = (long double) (double) value; },
      [&](long_double_type_t) { r.data = (long double) value; },
      [&](char_type_t)           { r.data = (__uint128_t) (char) value; },
      [&](unsigned_char_type_t)  { r.data = (__uint128_t) (unsigned char) value; },
      [&](signed_char_type_t)    { r.data = (__uint128_t) (signed char) value; },
      [&](short_type_t)    { r.data = (__uint128_t) (short) value; },
      [&](unsigned_short_type_t)  { r.data = (__uint128_t) (unsigned short) value; },
      [&](int_type_t)           { r.data = (__uint128_t) (int) value; },
      [&](unsigned_int_type_t)  { r.data = (__uint128_t) (unsigned int) value; },
      [&](long_type_t)           { r.data = (__uint128_t) (long) value; },
      [&](unsigned_long_type_t)  { r.data = (__uint128_t) (unsigned long) value; },
      [&](long_long_type_t)           { r.data = (__uint128_t) (long) value; },
      [&](unsigned_long_long_type_t)  { r.data = (__uint128_t) (unsigned long long) value; },
    });
  });
  return r;
}

cst tree_fold(expression expr, sv &err) {
  return expr(overload {
    [](auto &) -> cst { return {}; },
    [&](int_cst_expression_t int_) -> cst {
       return do_cast(cst_t{ .data = (__uint128_t) int_.value}, integer_type(int_.type));
    },
    [&](float_cst_expression_t float_) -> cst {
       return do_cast(cst_t{ .data = float_.value}, float_type(float_.type));
    },
    [&](cast_expression_t cast_) -> cst {
      if(auto cst = tree_fold(cast_.cast_from, err)) {
        *cst = do_cast(*cst, arithmetic_type(cast_.cast_to));
        return cst;
      } else
        return cst;
    },
    [&](decl_expression_t decl) -> cst {
      if(auto enum_ = (tree::enum_cst) decl.declref)
        return do_cast(cst_t{ .data = enum_->value}, enum_->type->type);
      return {};
    },
    [&](binary_expression_t binary) -> cst {
      auto lhs = tree_fold(binary.lhs, err), rhs = tree_fold(binary.rhs, err);
      if(!lhs || !rhs) return {};

      *lhs = do_cast(*lhs, arithmetic_type(binary.type));
      *rhs = do_cast(*rhs, arithmetic_type(binary.type));

      opt<cst_t> r;
      bool signed_ = binary.type.is_narrow<signed_integral_type_t>();
      // It's the most horrible code in the whole codebase
      lex::binary_op_tab(for_each([&]<class T>(T tok) {
        if(tok.key == binary.op)
          visit(lhs->data, rhs->data, [&](auto lhs, auto rhs) {
            if constexpr(requires { tok.f(lhs, rhs); }) {
              if constexpr(lex::is_relational(T{}.key) || T{}.key == "||"_s || T{}.key == "&&"_s) {
                if(std::is_integral_v<decltype(lhs)> && signed_)
                  r = cst_t{.data = (__uint128_t) tok.f((__int128_t) lhs, (__int128_t)  rhs) };
                else
                  r = cst_t{.data = (__uint128_t) tok.f(lhs, rhs) };
              } else if((T{}.key == "/"_s || T{}.key == "%"_s) && rhs == 0)
                err = "divide by 0 in constant expression";
              else
                r = cst_t{.data = tok.f(lhs, rhs) };
            }
          });
      }));

      // don't forget to cast result code into the target type
      if(r)
        return *r = do_cast(*r, arithmetic_type(binary.type));

      return {};
    }
  });
}
}}
