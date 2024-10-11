#pragma once
#include "semantics.hpp"
namespace c9 { namespace sema {

bool label_manager::process_label(tree::label label) {
  string name = label->name;
  if(auto p = labels.find(name); p != labels.end())
    return false;
  else
    labels.push_back({name, label});

  unresolved_targets | rv::transform([&](unresolved &u) {
    if(u.target_name == name)
      u.from->target = label;
    return u;
  });
  return true;
}
void label_manager::lookup_label(string name, tree::goto_statement goto_) {
  if(auto p = labels.find(name); p == labels.end())
    unresolved_targets.push_back({name, goto_});
  else
    goto_->target = p->value;
}

tree::subscript_expression semantics::build_subscript_expression(source_range loc, tree::expression of, tree::expression with) {
  using namespace tree;
  if(!strip_type(with->type)([]<class T>(T &) { return narrow<T, integer_type_t>; })) {
    d.diag(loc, "error"_s, "index in array subscript must be an integer");
    return {};
  }

  return strip_type(of->type)(overload {
    [&](pointer_t &ptr) -> subscript_expression {
      subscript_expression_t r{.of = of, .with = with };
      r.type = ptr.type;
      r.loc = loc;
      return r;
    },
    [&](auto &) -> subscript_expression {
      d.diag(loc, "error"_s, "invalid type to subscript");
      return {};
    }
  });
}
tree::function_call semantics::build_function_call(source_range loc, tree::expression calee, std::vector<tree::expression> &args) {
  return strip_type(calee->type)(overload {
    [&](tree::pointer_t &ptr) ->  tree::function_call {
      auto type = strip_type(ptr.type);
      if(!type.is<tree::function_type_t>()) {
        d.diag(loc, "error"_s, "calee cannot be a pointer to non function type");
        return {};
      }

      auto &fun = (tree::function_type_t &) type;
      if(args.size() < fun.params.size()) {
        d.diag(loc, "error"_s, "too few arguments to call function");
        return {};
      }
      if(args.size() > fun.params.size() && !fun.is_variadic) {
        d.diag(loc, "error"_s, "too few arguments to call function");
        return {};
      }

      for(size_t i = 0; i < args.size(); ++i) {
        auto paramtype = strip_type(fun.params[i].type);
        if(!visit(strip_type(args[i]->type), paramtype, [&](auto &l, auto &r) {
          return is_compatible(lex::assign_tok{"="_s}, l, r);
        })) {
          d.diag(args[i]->loc, "error"_s, "incompatible types of argument and parameter ('{}')", i);
          return {};
        }
        args[i] = maybe_build_cast_expression(args[i]->loc, paramtype, args[i]);
      }
      tree::function_call_t call{.calee = calee, .args = mov(args)};
      call.loc = loc;
      call.type = fun.return_type;
      return call;
    },
    [&](auto &) ->  tree::function_call {
      d.diag(loc, "error"_s, "calee in functiona call must be a function");
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
tree::decl_expression semantics::build_decl_expression(location_t ref_loc, tree::decl decl) {
  tree::decl_expression r;
  decl(overload {
    [&](tree::function_t &f) {
      r = tree::decl_expression{{{{.type = f.type->ptr_type, .loc = ref_loc}}, decl}};
    },
    [&](tree::variable_t &v) {
      r = tree::decl_expression{{{{.type = v.type, .loc = ref_loc}}, decl}};
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

tree::expression semantics::maybe_build_cast_expression(source_range loc, tree::type_decl to, tree::expression from) {
  using namespace tree;
  return visit(strip_type(to), strip_type(from->type), prioritizied_overload(
    [&]<class T>(T &, T &) -> expression{
      from->loc = loc;
      return from;
    },
    [&](auto lhs, auto rhs) -> expression requires requires { lhs.is_scalar(); rhs.is_scalar(); }
    || std::same_as<decltype(lhs), void_type_t> {
      cast_expression_t cast{  .cast_from = from, .cast_to = to };
      cast.loc = loc;
      cast.type = to;
      return cast;
    },
    [&](auto &, void_type_t) -> expression {
      d.diag(loc, "error"_s, "void typed value appears in cast expression");
      return {};
    },
    [&](union_decl_t &, auto &) -> expression {

      return {};
    },
    [&](auto &, auto &) -> expression {
      d.diag(loc, "error"_s, "incompatible types to cast expression");
      return {};
    }
  ));
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
      d.diag(loc, "error"_s, "incompatible types");
      return {};
    }

    type_decl type;
    if(!visit(op, overload {
      [&](lex::binary_tok op) {
        if(op == "-"_s && requires { l.is_pointer(); r.is_pointer(); })
          type = d.t.ptrdiff_type_node;
        else if(lex::is_relational(op) || op == "||"_s || op == "&&"_s)
          type = int_type_node;
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
    d.diag(cond->loc + rhs->loc, "error"_s, "operand of ternary expression must be scalar type");
    return {};
  }
  tree::ternary_expression_t ternary{.cond = cond, .lhs = lhs, .rhs = rhs};
  ternary.loc = cond->loc + rhs->loc;
  if(!(ternary.type = get_common_type(lex::binary_tok{"=="_s}, lhs->loc + rhs->loc, strip_type(lhs->type), strip_type(rhs->type))))
    return {};
    return ternary;
}

tree::expression semantics::build_unary_expression(source_range loc, lex::token op, tree::expression expr) {
  tree::expression r;
  visit(op, overload {
    [](auto &) { c9_assert(0); },
    [&](decltype("++"_s)) {

    },
    [&](decltype("--"_s)) {

    },
    [&](decltype("&"_s))  {
      if(!expr.is_narrow<tree::lvalue_t>()) {
        d.diag(loc, "error"_s, "addressof expression requires lvalue");
        return;
      }
      tree::addressof_t addr{.of = (tree::lvalue) expr};
      addr.loc = loc;
      addr.type = tree::pointer{{.type = expr->type}};
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
        d.diag(loc, "error"_s, "cannot dereference non pointer type");
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
          d.diag(loc, "error"_s, "operand of unary '{}' must be arithmetic type", s.c_str());
          return;
        }
        unary.type = expr->type = promote(expr->type);
      } else if(s == "~"_s) {
        if(!expr->type.is_narrow<tree::integer_type_t>()) {
          d.diag(loc, "error"_s, "operand of unary '{}' must be integer type", s.c_str());
          return;
        }
        unary.type = expr->type = promote(expr->type);
      } else if(s == "!"_s) {
        if(!expr->type.is_narrow<tree::scalar_type_t>()) {
          d.diag(loc, "error"_s, "operand of unary '{}' must be scalar type", s.c_str());
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


tree::return_statement semantics::build_return_statement(source_range loc, tree::type_decl return_type, tree::expression expr) {
  tree::type_decl type, expr_type = tree::void_type_node;
  if(expr) expr_type = strip_type(expr->type);
  if(!(type = get_common_type(lex::assign_tok{"="_s}, loc, strip_type(return_type), expr_type)))
    return {};
  tree::return_statement_t ret{.expr =  tree::cast_expression{{ .cast_from = expr, .cast_to = strip_type(return_type)}}};
  ret.expr->type = type;
  return ret;
}


tree::decl semantics::build_typedef_decl(rich_location rl, id id, tree::decl &node, tree::type_name type) {
  if(node) {
    redecl_error(rl, id.name, node, "");
    return {};
  }
  return node = tree::typedef_decl{{.name = id.name, .type = type}};
}
tree::decl semantics::build_local_extern_decl(rich_location rl, id id, tree::decl &node, tree::type_name type) {
  return node(overload {
    [&](auto &decl) -> tree::decl requires requires { decl.scs; } {
      if(decl.type != type) {
        redecl_error(rl, id.name, node, "with different type");
        return node;
      }
      if(decl.scs == "extern"_s)
        return node;


      redecl_error(rl, id.name, node, "with different storage class specifier ('{}' and 'extern')", sv_variant(decl.scs));
      return node;
    },
    [&](tree::empty_node_t) {
      return node = build_decl(rl, {.name = id.name, .level = 0}, type, "extern"_s, true);
    },
    [&](auto &) -> tree::decl {
      redecl_error(rl, id.name, node, "");
      return {};
    }
  });
}

tree::decl semantics::build_decl(rich_location rl, id id, tree::type_name type, storage_class_spec scs, bool implicit) {
  if(!id.node || !id.node->decl)
    id.node = &scopes.stack[id.level]->operator[](id.name);
  auto &decl = id.node->decl;

  if(scs == "typedef"_s)
    return build_typedef_decl(rl, id, decl, type);

  auto funtype = (tree::function_type) type->type;
  if((scs == "extern"_s || funtype) && !id.is_global_scope())
    return build_local_extern_decl(rl, id, decl, type);

  tree::type_decl dtype;

  auto scs_assign = [&]<class T>(T &&to, auto from) -> __remove_cvref(T) {
    return visit(from, [&](auto from) {
      if constexpr(!requires { to = from;  })
        d.diag(rl, "error"_s, "function cannot take '{}' storage class", from.c_str());
        else
          to = from;
      return to;
    });
  };
  if(decl) {
    if(dtype = get_decl_type(decl); decl && (dtype.is<tree::typedef_decl_t>() || dtype != type->type)) {
      redecl_error(rl, id.name, decl, "");
      return {};
    }

    return decl([&](auto &tree) -> tree::decl {
      if constexpr(requires { tree.scs; }) {
        auto err = [&] {
          redecl_error(rl, id.name, decl, "with different storage class specifier ('{}' and '{}')", sv_variant(tree.scs), sv_variant(scs));
          return tree::decl{};
        };

        if(tree.scs == "extern"_s && (scs != ""_s && scs != "extern"_s))
          return err();
        if(scs == "extern"_s && (tree.scs != ""_s && tree.scs != "extern"_s))
          return err();
        if(scs == "extern"_s || tree.scs == "extern"_s)
          return decl;
        if(__is_same(decltype(tree), tree::function_t &)) {
          scs_assign(tree.scs, scs);
          id.node->decl_implicit = false;
          return decl;
        } else
          return err();
      }
      c9_assert(0);
    });
  }


  id.node->decl_implicit = implicit;
  if(funtype)
    decl = tree::function{{.name = id.name, .type = funtype,
      .scs = scs_assign(decltype(tree::function_t::scs){}, scs)
    }};
  else
    decl = tree::variable{{.name = id.name, .type = type->type,
      .is_global = id.is_global_scope(),
      .scs = scs_assign(decltype(tree::variable_t::scs){}, scs)
    }};
  return decl;
}

} }
