#pragma once
#include "sema.hpp"
#include "lex-conv.hpp"
#include "tree-dump.hpp"
namespace c9 { namespace sema {
tree::expression semantics::build_subscript_expression(source_range loc, tree::expression of, tree::expression with) {
  using namespace tree;
  if(!strip_type(with->type)([]<class T>(T &) { return narrow<T, integer_type_t>; })) {
    d.diag(loc, "error"_s, "index type '{}' in array subscript is not an integer", with->type);
    return {};
  }

  return strip_type(of->type)(overload {
    [&](pointer_t &ptr) -> expression {
      dereference_t deref{.expr = ({
        binary_expression_t binexpr{.op = "+"_s, .lhs = of, .rhs = build_binary_expression("*"_s, with, eval_sizeof_expression(with->loc, ptr.type)) };
        binexpr.type = ptr;
        binexpr.loc = loc;
        binexpr;
      })};
      deref.type = ptr.type;
      deref.loc = loc;
      return deref;
    },
    [&](auto &) -> expression {
      d.diag(loc, "error"_s, "invalid type '{}' to subscript", with->type);
      return {};
    }
  });
}
tree::function_call semantics::build_function_call(source_range loc, tree::expression calee, std::vector<tree::expression> &args) {
  return strip_type(calee->type)(overload {
    [&](tree::pointer_t &ptr) ->  tree::function_call {
      auto type = strip_type(ptr.type);
      if(!type.is<tree::function_type_t>()) {
        d.diag(loc, "error"_s, "'{}' is not a pointer to function type", ptr.type);
        return {};
      }

      auto &fun = (tree::function_type_t &) type;
      string funname = ({
        string name;
        if(auto declexpr = tree::decl_expression(calee))
          if(auto fun = tree::function(declexpr->declref)) name = fun->name;
         name;
      });

      auto quote = funname.size() ? "\'" : "";
      if(args.size() < fun.params.size()) {
        d.diag(loc, "error"_s, "too few arguments to call function {}{}{} (passed {}, while function gets)", quote, funname, quote, fun.params.size(), args.size());
        return {};
      }
      if(args.size() > fun.params.size() && !fun.is_variadic) {
        d.diag(loc, "error"_s, "too many arguments to call function {}{}{} (passed {}, while function gets)", quote, funname, quote, fun.params.size(), args.size());
        return {};
      }

      for(size_t i = 0; i < args.size(); ++i) {
        if(i == fun.params.size()) break;
        auto paramtype = strip_type(fun.params[i].type);
        if(!visit(strip_type(args[i]->type), paramtype, [&](auto &l, auto &r) {
          return is_compatible(lex::assign_tok{"="_s}, l, r);
        })) {
          d.diag(args[i]->loc, "error"_s, "incompatible types of argument and parameter ({} and {}) at position {}",
                                fun.params[i].type, args[i]->type, i);
          return {};
        }
        args[i] = build_cast_expression(args[i]->loc, args[i],  paramtype);
      }
      tree::function_call_t call{.calee = calee, .args = mov(args)};
      call.loc = loc;
      call.type = fun.return_type;
      return call;
    },
    [&](auto &) ->  tree::function_call {
      d.diag(loc, "error"_s, "'{}' is not a pointer to function type", calee->type);
      return {};
    }
  });
}

tree::cast_expression semantics::build_cast_expression(source_range sr, tree::expression expr, tree::type_decl type) {
  if(auto type1 = get_common_type(lex::assign_tok{"="_s}, sr, strip_type(type), strip_type(expr->type))) {
    tree::cast_expression_t cast{ .cast_from = expr,  .cast_to = type1 };
    cast.type = type1;
    return cast;
  }
  return {};
}
tree::expression semantics::build_decl_expression(location_t ref_loc, tree::decl decl) {
  tree::expression r;
  decl(overload {
    [&](tree::function_t &f) {
      r = tree::decl_expression{{{{.type = tree::function_type(f.type)->ptr_type, .loc = ref_loc}}, decl, f.type}};
    },
    [&](tree::variable_t &v) {
      tree::type_decl type = v.type;
      if(auto arr = (tree::array) type) type = arr->ptr_type;
      r = tree::decl_expression{{{{.type = type, .loc = ref_loc}}, decl, v.type}};
    },
    [&](tree::enum_cst_t &e) {
      r = tree::decl_expression{{{{.type = e.type, .loc = ref_loc}}, decl}};
    },
    [&](auto &) {
      d.diag(ref_loc, "error"_s, "typedef can't appear in expression");
    }
  });

  return r;
}

tree::statement_expression semantics::build_statement_expression(source_range loc, tree::compound_statement stmts) {
  using namespace tree;
  type_decl type = void_type_node;
  if(stmts->size())
    stmts->back()(overload {
      [&](narrow<expression_t> auto &expr) { type = expr.type;  },
                  [&](auto &) {}
    });
  statement_expression_t t{.stmts = stmts};
  t.loc = loc;
  t.type = type;
  return t;
}

tree::type_decl semantics::get_common_type(variant<lex::binary_tok, lex::assign_tok> op, source_range loc, tree::type_decl lhstype, tree::type_decl rhstype) {
  using namespace tree;
  return visit(lhstype, rhstype, [&]<class L, class R>(L &l, R &r) -> type_decl {
    if(!visit(op, [&](auto op) { return is_compatible(op, l, r); })) {
      d.diag(loc, "error"_s, "incompatible types ('{}' and '{}')", lhstype, rhstype);
      return {};
    }

    type_decl type;
    if(!visit(op, overload {
      [&](lex::binary_tok op) {
        return type;
      },
      [&](lex::assign_tok op) {
        if(op == "="_s) type = lhstype;
        return type;
      }
    })) {
      if(requires { l.is_pointer(); }) type = lhstype;
      else if(requires { r.is_pointer(); }) type = rhstype;
      else if constexpr(requires { l.is_arithmetic(); r.is_arithmetic(); })
        type = usual_arith_conv(l, r);
    }
    return type;
  });
}

tree::binary_expression semantics::build_binary_expression(lex::binary_tok op, tree::expression lhs, tree::expression rhs) {
  using namespace tree;

  binary_expression_t binexpr{ .op = op, .lhs = lhs, .rhs = rhs};
  binexpr.loc = lhs->loc + rhs->loc;
  if(!(binexpr.type = get_common_type(op, binexpr.loc, strip_type(lhs->type), strip_type(rhs->type))))
    return {};
  else {
    binexpr.lhs = build_cast_expression(lhs->loc, lhs, binexpr.type);
    binexpr.rhs = build_cast_expression(rhs->loc, rhs, binexpr.type);
  }

  if(op == "-"_s && (tree::pointer) strip_type(binexpr.type))
    binexpr.type = d.t.ptrdiff_type_node;
  else if(lex::is_relational(op) || op == "||"_s || op == "&&"_s)
    binexpr.type = int_type_node;
  return binexpr;
}

tree::assign_expression semantics::build_assign_expression(lex::assign_tok op, tree::expression lhs, tree::expression rhs) {
  using namespace tree;
  lvalue assign_to;
  if(!lhs.is_narrow<lvalue_t>()) {
    d.diag(lhs->loc, "error"_s, "left operand of assignment expression must be lvalue");
    return {};
  }
  assign_to = (lvalue) lhs;

  source_range loc = lhs->loc + rhs->loc;
  assign_expression_t assign{ .op = op, .lhs = lhs, .rhs = rhs};
  if(!(assign.type = get_common_type(op, loc, strip_type(lhs->type), strip_type(rhs->type))))
    return {};
  assign.loc = loc;
  return assign;
}

tree::ternary_expression semantics::build_ternary_expression(tree::expression cond, tree::expression lhs, tree::expression rhs) {
  if(!cond->type.is_narrow<tree::scalar_type_t>()) {
    d.diag(cond->loc + rhs->loc, "error"_s, "operand type eof ternary expression '{}' is not a scalar type", cond->type);
    return {};
  }
  tree::ternary_expression_t ternary{.cond = cond, .lhs = lhs, .rhs = rhs};
  ternary.loc = cond->loc + rhs->loc;
  if(!(ternary.type = get_common_type(lex::binary_tok{"=="_s}, lhs->loc + rhs->loc, strip_type(lhs->type), strip_type(rhs->type))))
    return {};
  ternary.lhs = build_cast_expression(lhs->loc, lhs, ternary.type);
  ternary.rhs = build_cast_expression(rhs->loc, rhs, ternary.type);
  return ternary;
}
tree::postcrement_expression semantics::build_postcrement_expression(source_range loc, lex::crement_tok tok, tree::expression expr) {
  return visit(tok, [&]<char l>(string_seq<l, l>) -> tree::postcrement_expression {
    if(!(tree::lvalue) expr) {
      d.diag(loc, "error"_s, "{}{} expression requires lvalue", l, l);
      return {};
    }
    tree::postcrement_expression_t r{.op = tok, .expr = expr};
    r.loc = loc;
    r.type = expr->type;
    if(get_common_type(lex::binary_tok{string_seq<l>{}}, loc, strip_type(expr->type), tree::int_type_node))
      return r;
    return {};
  });
}
tree::expression semantics::build_unary_expression(source_range loc, lex::token op, tree::expression expr) {
  tree::expression r;
  visit(op, overload {
    [](auto &) { c9_assert(0); },
    [&](decltype("++"_s)) {
      r = build_assign_expression("="_s, expr, build_binary_expression("+"_s, expr, tree::int_cst_expression{{1, tree::int_type_node, expr->loc}}));
    },
    [&](decltype("--"_s)) {
      r = build_assign_expression("="_s, expr, build_binary_expression("-"_s, expr, tree::int_cst_expression{{1, tree::int_type_node, expr->loc}}));
    },
    [&](decltype("&"_s))  {
      if(!expr.is_narrow<tree::lvalue_t>()) {
        d.diag(loc, "error"_s, "addressof expression requires lvalue");
        return;
      }
      if(auto deref = (tree::dereference) expr) {
        r = deref->expr;
        return;
      }
      if(auto access = (tree::access_member) expr) {
        access->addr = true;
        access->type = d.t.make_ptr(access->type);
        r = access;
        return;
      }
      tree::addressof_t addr{.expr = expr};
      travel_lvalue(expr, [&](tree::variable var) { var->alias = true; });
      addr.loc = loc;
      addr.type = d.t.make_ptr(tree::decl_expression(expr) ? tree::decl_expression(expr)->undecay : expr->type);
      r = addr;
    },
    [&](decltype("*"_s)) {
      if(auto ptr = (tree::pointer) strip_type(expr->type)) {
        if(ptr->type.is<tree::function_type_t>()) {
          expr->loc = loc;
          r = expr;
          return;
        }
        tree::dereference_t der{.expr = expr};
        der.loc = loc;
        der.type = tree::function_type(ptr->type) ? expr->type : ptr->type;
        r = der;
      } else {
        d.diag(loc, "error"_s, "cannot dereference non pointer type '{}'", expr->type);
        return;
      }
    },
    /* 6.5.3.3 Unary arithmetic operators
     * Constraints
     * 1 The operand of the unary + or - operator shall have arithmetic type; of the ~ operator, integer type;
     * of the ! operator, scalar type.
     * Semantics
     * 2 The result of the unary + operator is the value of its (promoted) operand. The integer promotions
     * are performed on the operand, and the result has the promoted type.
     * 3 The result of the unary - operator is the negative of its (promoted) operand. The integer promotions
     * are performed on the operand, and the result has the promoted type.
     * 4 The result of the ~ operator is the bitwise complement of its (promoted) operand (that is, each bit in
     * the result is set if and only if the corresponding bit in the converted operand is not set). The integer
     * promotions are performed on the operand, and the result has the promoted type. If the promoted
     * type is an unsigned type, the expression ~E is equivalent to the maximum value representable in
     * that type minus E.
     * 5 The result of the logical negation operator ! is 0 if the value of its operand compares unequal to
     * 0, 1 if the value of its operand compares equal to 0. The result has type int. The expression !E is
     * equivalent to (0==E).*/
    [&]<char ...c>(string_seq<c...> s) requires (lex::is_unary(s)) {
      tree::unary_expression_t unary{.op = s, .expr = expr};
      unary.loc = loc;

      if(s == "+"_s || s == "-"_s) {
        if(!expr->type.is_narrow<tree::arithmetic_type_t>()) {
          d.diag(loc, "error"_s, "operand of unary '{}' must be arithmetic type (type: '{}')", s.c_str(), expr->type);
          return;
        }
        unary.type = expr->type = promote(expr->type);
      } else if(s == "~"_s) {
        if(!expr->type.is_narrow<tree::integer_type_t>()) {
          d.diag(loc, "error"_s, "operand of unary '{}' must be integer type (type: '{}')", s.c_str(), expr->type);
          return;
        }
        unary.type = expr->type = promote(expr->type);
      } else if(s == "!"_s) {
        if(!expr->type.is_narrow<tree::scalar_type_t>()) {
          d.diag(loc, "error"_s, "operand of unary '{}' must be scalar type (type: '{}')", s.c_str(), expr->type);
          return;
        }
        unary.type = tree::int_type_node;
      } else
        c9_assert(0);
      r = unary;
    }
  });
  return r;
}

tree::string_cst_expression semantics::build_string(source_range loc, lex::string str) {
  if(auto &t = string_tab[str.value])
    return t;
  else
    return t = {{str.value, d.t.make_ptr(lex::get_prefix_type(str.prefix)), loc, string_tab.size() - 1}};
}

}}
