#include "tree-trait.hpp"

namespace c9 { namespace tree {
bool operator==(type_decl lhs, type_decl rhs) {
  return visit(strip_type(lhs), strip_type(rhs), overload {
    [](pointer_t &lhs, pointer_t &rhs) { return lhs.type == rhs.type; },
    [](type_name_t &lhs, type_name_t &rhs) {
      return lhs.is_const == rhs.is_const && lhs.is_volatile == rhs.is_volatile
            && lhs.is_restrict == rhs.is_restrict && lhs.type == rhs.type;
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
}}
