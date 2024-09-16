#include "cfg.hpp"
#include "tree-trait.hpp"

namespace c9 { namespace cfg {
void basic_block::search_def_for_phi(tree::variable var, flat_set<tree::op> &out, basic_block *stop) {
  for(auto elt : def.map<tree::ssa_variable_t>())
    if(elt->var == var) {
      out.emplace(tree::op(elt)); return;
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
    return tree::ssa_variable(op) && tree::ssa_variable(op)->ssa_n == (phi.first ? phi.second : ssa->ssa_n + 1);
  });

  if(p != phi_ops.end())
    phi_ops.erase(p);

  if(phi.first)
    phi.first->elts.append(phi_ops);
  else
    phi = {tree::phi{{.elts = mov(phi_ops)}},/* ssa->ssa_n*/ ssa->ssa_n = ++ssa->var->ssa_count  };
}
void basic_block::add_insn(tree::statement insn) { insns.emplace_back(insn);  }
tree::op basic_block::add_assign(tree::expression insn, tree::op dst) {
  if(auto ssa = (tree::ssa_variable) dst)
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
  cfg_stream &operator>>(tree::expression expr) { cfg.construct(expr); return *this;  }
  cfg_stream &operator>>(tree::statement  stmt) { cfg.construct(stmt); return *this;  }
};

cfg_stream control_flow_graph::cfg() { return cfg_stream{*this}; }

tree::expression control_flow_graph::construct_expr_no_op(tree::expression expr) {
  sv dummy;
  if(auto cst = tree_fold(expr, dummy))
    return cst;
  return expr(overload {
    [](auto &) -> tree::expression {},
    [&](tree::int_cst_expression_t &int_) -> tree::expression {
      return tree::cst{{.data = (__uint128_t) int_.value}};
    },
    [&](tree::float_cst_expression_t &float_) -> tree::expression {
      return tree::cst{{.data = (long double) float_.value}};
    },
    [&](tree::decl_expression_t &expr) -> tree::expression {
      if(auto var = (tree::variable) expr.declref) {
        if(var->is_global) {
          last_bb->use.insert(var);
          return var;
        }

        tree::ssa_variable ssa{{ .var = var, .ssa_n = var->ssa_count }};
        if(!last_bb->use.find(ssa)) {
          last_bb->use.insert(ssa);
          last_bb->place_phi(ssa);
        }
        return ssa;
      }
      if(auto fun = (tree::function) expr.declref)
        return fun;
      c9_assert(0);
    },
    [&](tree::assign_expression_t &assign) -> tree::expression {
      auto dst = construct(assign.lhs);
      auto src = construct_expr_no_op(assign.rhs);
      last_bb->add_assign(src, dst);
      return dst;
    },
    [&](tree::binary_expression_t &b) -> tree::expression {
      b.lhs = construct_expr_no_op(b.lhs);
      b.rhs = construct_expr_no_op(b.rhs);
      if(tree::op(b.lhs) && tree::op(b.rhs))
        return expr;
      return last_bb->add_assign(expr, make_tmp());
    }
  });
}
tree::op control_flow_graph::construct(tree::expression expr) {
  if(auto op = tree::op(expr = construct_expr_no_op(expr)))
    last_op = op;
  else
    last_op = last_bb->add_assign(expr, make_tmp());
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

      cfg() >> for_.clause     >> for_.cond >> cond >> cond_bb
            >> add_bb(last_bb) >> loop_body >> for_.body >> for_.step >> loop_finish
            >> loop_finish->jump(*cond_bb)
            >> cond_bb->br(cond, *loop_body, add_bb(cond_bb));

      cond_bb->add_pred(loop_finish);
    },
    [&](tree::variable_t &var) {
      if(var.definition)
        last_bb->add_assign(var.definition, tree::variable(stmt));
    },
    [&]<narrow<tree::expression_t> T>(T &) {
      construct_expr_no_op(tree::expression{tree::tree_value<T>(stmt)});
    },
    [&](tree::compound_statement_t &stmts) { for(auto stmt : stmts) construct(stmt); },
  });
}
}}
