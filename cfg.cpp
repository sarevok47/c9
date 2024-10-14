#include "cfg.hpp"
#include "tree-trait.hpp"

namespace c9 { namespace cfg {
void basic_block::search_def_for_phi(tree::variable var, flat_set<tree::op> &out, basic_block *stop) {
  for(auto elt : def.map<tree::ssa_variable_t>().storage | rv::reverse)
    if(elt.key->var == var) {
      out.emplace(tree::op(elt.key)); return;
    }

  for(auto pred : preds) {
    if(pred == stop) break;
    pred->search_def_for_phi(var, out, stop);
  }
}
void basic_block::place_phi(tree::ssa_variable ssa) {
  flat_set<tree::op> phi_ops = { };
  search_def_for_phi(ssa->var, phi_ops, this);

  auto &phi = phis[ssa->var];

  auto p = std::find_if(phi_ops.begin(), phi_ops.end(), [&](auto op) {
    return tree::ssa_variable(op) && tree::ssa_variable(op)->ssa_n == (phi.first ? phi.second->ssa_n : ssa->ssa_n + 1);
  });

  if(p != phi_ops.end())
    phi_ops.erase(p);

  if(phi.first)
    phi.first->elts.append(phi_ops);
  else {
    ssa->ssa_tab_n = ssa->var->ssa_tab_n = ++cfg.nssa;
    ssa->ssa_n = ++ssa->var->ssa_count;
    auto cpy = ssa.cpy();
    def.insert(cpy);
    phi = {tree::phi{{.elts = mov(phi_ops)}}, cpy};
  }
}
void basic_block::add_insn(tree::statement insn) { insns.emplace_back(insn);  }
tree::op basic_block::add_assign(tree::expression insn, tree::op dst) {
  if(auto ssa = (tree::ssa_variable) dst)
    ssa->ssa_tab_n = ssa->var->ssa_tab_n = ++cfg.nssa,
    ssa->ssa_n = ++ssa->var->ssa_count;

  def.insert(dst);
  add_insn(tree::mov{{.src = insn, .dst = dst }});
  return dst;
}
void basic_block::add_pred(basic_block *bb) {
  preds.emplace(bb);
  for(auto phi : phis)
    bb->search_def_for_phi(phi.key, phi.value.first->elts, bb);

}
struct cfg_stream {
  control_flow_graph &cfg;

  cfg_stream &operator>>(auto &&) { return *this; }
  cfg_stream &operator>>(basic_block *&bb) { bb = cfg.last_bb; return *this; }
  cfg_stream &operator>>(tree::op &op) { op = cfg.last_op; return *this;  }
  cfg_stream &operator>>(tree::expression expr) { if(expr) cfg.construct(expr); return *this;  }
  cfg_stream &operator>>(tree::statement  stmt) { if(stmt) cfg.construct(stmt); return *this;  }
  template<class ...T> cfg_stream &operator>>(variant<T...> &v) { visit(v, [&](auto tree) { if(tree) *this >> tree; }); return *this; }
};

cfg_stream control_flow_graph::cfg() { return cfg_stream{*this}; }
tree::op control_flow_graph::construct_var(tree::variable var) {
  if(var->is_global || var->alias) {
    vars.insert(var);
    last_bb->use.insert(var);
    return var;
  }

  auto ssa = [&] -> tree::ssa_variable {
    tree::ssa_variable_t ssa{.var = var, .ssa_n = var->ssa_count, .ssa_tab_n = var->ssa_tab_n};
    ssa.type = var->type;
    return ssa;
  }();
  if(!last_bb->use.find(ssa)) {
    last_bb->use.insert(ssa);
    last_bb->place_phi(ssa);
  }
  return ssa;
}
tree::expression control_flow_graph::construct_expr_no_op(tree::expression expr) {
  sv dummy;
  if(auto cst = tree_fold(expr, dummy))
    return cst;
  expr->type = tree::strip_type(expr->type);
  return expr(overload {
    [](auto &) -> tree::expression { fprint(stderr, "{}", __PRETTY_FUNCTION__); },
    [&](tree::string_cst_expression_t &) { return expr; },
    [&](tree::subscript_expression_t &expr) {
      expr.of = construct(expr.of);
      expr.with = construct(expr.with);
      return expr;
    },
    [&](tree::dereference_t &deref) {
      deref.expr = construct(deref.expr);
      return expr;
    },
    [&](tree::addressof_t &addr) {
      addr.expr = construct(addr.expr);
      return expr;
    },
    [&](tree::cast_expression_t &cast) -> tree::expression {
      if(visit(cast.type, strip_type(cast.cast_from->type), overload {
        [&](auto &lhs, auto &rhs) requires requires { lhs.is_integer(); rhs.is_integer(); } {
          return lhs.rank < rhs.rank;
        },
        [](auto &, auto &) { return true; }
      })) {
        cast.cast_from = construct(cast.cast_from);
        return expr;;
      } else {
        auto expr = construct_expr_no_op(cast.cast_from);
        expr->type = cast.type;
        return expr;
      }
    },
    [&](tree::decl_expression_t &expr) -> tree::expression {
      if(auto var = (tree::variable) expr.declref) return construct_var(var);
      if(auto fun = (tree::function) expr.declref)
        return fun;
      c9_assert(0);
    },
    [&](tree::assign_expression_t &assign) -> tree::expression {
      auto dst = construct(assign.lhs);
      auto src = construct_expr_no_op(assign.rhs);
      return last_bb->add_assign(src, dst);
    },
    [&](tree::binary_expression_t &b) -> tree::expression {
      b.lhs = construct(b.lhs);
      b.rhs = construct(b.rhs);
      return expr;
    },
    [&](tree::unary_expression_t &u) -> tree::expression {
      u.expr = construct(u.expr);
      return expr;
    },
    [&](tree::statement_expression_t &stmt_expr) -> tree::expression {
      for(auto stmt : *stmt_expr.stmts | iter_range) {
        if(stmt + 1 == stmt_expr.stmts->end() && stmt_expr.type != tree::void_type_node)
          return construct((tree::expression) *stmt);
        construct(*stmt);
      }
      return {};
    },
    [&](tree::function_call_t &fun_call) {
      fun_call.calee = construct(fun_call.calee);
      for(auto &arg : fun_call.args) arg = construct(arg);
      return fun_call;
    }
  });
}
tree::op control_flow_graph::construct(tree::expression expr) {
  if(auto op = tree::op(expr = construct_expr_no_op(expr)))
    last_op = op;
  else
    last_op = last_bb->add_assign(expr, make_tmp(expr->type));
  return last_op;
}
void control_flow_graph::construct(tree::statement stmt) {
  stmt(overload {
    [](auto &) {},
    [](tree::empty_node_t) {},
    [&](tree::if_statement_t &if_) {
      basic_block *pre_if, *if_start, *if_end, *else_end, *else_start;
      tree::op cond;
      cfg() >> if_.cond >> cond
            >> pre_if
            >> add_bb(pre_if) >> if_start    >> if_.if_stmt   >> if_end
            >> add_bb(pre_if) >> else_start  >> if_.else_stmt >> else_end
            >> add_bb(if_end, else_end)
            >> if_start->jump(*last_bb)
            >> pre_if->br    (cond, *if_start, *else_start);
      if_start->dominator = else_start->dominator = last_bb->dominator = pre_if;
    },
    [&](tree::for_statement_t &for_) {
      basic_block *cond_bb, *loop_body, *loop_finish;
      tree::op cond;

      cfg() >> for_.clause     >> add_bb(last_bb) >> cond_bb >> for_.cond >> cond
            >> add_bb(last_bb) >> loop_body >> for_.body >> for_.step >> loop_finish
            >> loop_finish->jump(*cond_bb)
            >> cond_bb->br(cond, *loop_body, add_bb(cond_bb));

      cond_bb->add_pred(loop_finish);
    },
    [&](tree::while_statement_t &while_) {
      basic_block *cond_bb, *loop_body, *loop_finish;
      tree::op cond;

      cfg() >> add_bb(last_bb) >> cond_bb >> while_.cond >> cond
            >> add_bb(last_bb) >> loop_body >> while_.body
            >> last_bb->jump(*cond_bb)
            >> cond_bb->br(cond, *loop_body, add_bb(cond_bb));

      cond_bb->add_pred(last_bb);
    },
    [&](tree::do_while_statement_t &while_) {
      basic_block *loop_body;
      tree::op cond;

      cfg() >> add_bb(last_bb) >> loop_body >> while_.body
            >> while_.cond >> cond >> last_bb->br(cond, *loop_body, add_bb(last_bb));

      loop_body->add_pred(last_bb);
    },
    [&](tree::return_statement_t &ret) {
      ret.expr = construct_expr_no_op(ret.expr);
      last_bb->add_insn(stmt);
    },
    [&](tree::variable_t &var) {
      if(var.definition)
        last_bb->add_assign(construct_expr_no_op(var.definition), construct_var(tree::variable(stmt)));
    },
    [&]<narrow<tree::expression_t> T>(T &) {
      construct_expr_no_op(tree::expression{tree::tree_value<T>(stmt)});
    },
    [&](tree::compound_statement_t &stmts) { for(auto stmt : stmts) construct(stmt); },
  });
  ++insn_count;
}

void control_flow_graph::collect_phi_operands(tree::ssa_variable tab[]) {
  cfg_walker{entry}([&](basic_block &bb) {
    for(auto phis : bb.phis)
      for(auto phi : phis.value.first->elts)
        tab[tree::ssa_variable(phi)->ssa_tab_n] = phis.value.second;
  });
}



void control_flow_graph::unssa() {
  tree::ssa_variable tab[nssa + 1];
  collect_phi_operands(tab);

  for(auto bb = &entry; bb; bb = bb->step())
     bb->phis.clear();

  for_each_insn([&, ptab = (tree::ssa_variable *) tab](tree::statement &insn, basic_block &bb) {
    visit_ops(insn, [&](auto &op) {
      if(auto ssa = (tree::ssa_variable) op)
        if(auto unssa = ptab[ssa->ssa_tab_n]) {
          op = unssa;
          bb.def.erase(ssa);
          bb.def.insert(unssa);
        }
    });
  });
}
void control_flow_graph::convert_to_two_address_code() {
  cfg_walker{entry}([&](basic_block &bb) {
    for(auto insn : bb.insns | iter_range)
      if(auto mov = (tree::mov) *insn) {
        mov->src(overload {
          [](auto &) {},
          [&](auto &expr) requires requires { expr.lhs; expr.rhs; } {
            bb.insns.insert(insn, tree::mov{{.src = expr.rhs, .dst = mov->dst}});
            expr.rhs = mov->dst;
          }
        });
      }
  });
}
}}
