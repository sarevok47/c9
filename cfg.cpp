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
  cfg_stream &operator<<(tree::expression expr) {
    *this >> expr;
    if((tree::cst) cfg.last_op)
      cfg.last_op = cfg.last_bb->add_assign(cfg.last_op, cfg.make_tmp(cfg.last_op->type));
    return *this;
  }
  cfg_stream &operator>>(tree::expression expr) { if(expr) cfg.construct(expr); return *this;  }
  cfg_stream &operator>>(tree::statement  stmt) { if(stmt) cfg.construct(stmt); return *this;  }
  template<class ...T> cfg_stream &operator>>(variant<T...> &v) { visit(v, [&](auto tree) { if(tree) *this >> tree; }); return *this; }
};

cfg_stream control_flow_graph::cfg() { return cfg_stream{*this}; }
tree::op control_flow_graph::construct_var(tree::variable var) {
  if(var->is_global || var->alias || var->scs == "static"_s || !(tree::scalar_type) strip_type(var->type)) {
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
    [&](tree::dereference_t &deref) {
      deref.expr = construct(deref.expr);
      return expr;
    },
    [&](tree::addressof_t &addr) {
      addr.expr = construct(addr.expr);
      return expr;
    },
    [&](tree::access_member_t &access) {
      if(auto deref = (tree::dereference) access.expr)
        access.expr = deref->expr;
      access.expr = construct(access.expr);
      return expr;
    },
    [&](tree::cast_expression_t &cast) -> tree::expression {
      if(visit(cast.type, strip_type(cast.cast_from->type), overload {
        [&](auto &lhs, auto &rhs) requires requires { lhs.is_integer(); rhs.is_integer(); } {
          return lhs.rank < rhs.rank;
        },
        []<class T>(T &, T &) { return false; },
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
      if(auto var = (tree::variable) expr.declref) {
        if(strip_type(expr.type).get_data() != strip_type(expr.undecay).get_data())
          return ({ tree::addressof_t a{.expr = construct_var(var)}; a.type = expr.type; a; });

        return construct_var(var);
      }
      if(auto fun = (tree::function) expr.declref)
        return fun;
      c9_assert(0);
    },
    [&](tree::assign_expression_t &assign) -> tree::expression {
      auto dst = construct(assign.lhs);
      auto src = construct_expr_no_op(assign.rhs);
      last_bb->add_assign(src, dst);
      if(auto access = (tree::access_member) assign.lhs)
        last_bb->add_insn(tree::load_addr{{.src = dst, .dst = tree::op(access->expr), .offset = access->member.offset }});
      if(auto deref = (tree::dereference) assign.lhs)
        last_bb->add_insn(tree::load_addr{{.src = dst, .dst = tree::op(deref->expr) }});
      return dst;
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
      cfg() << if_.cond >> cond
            >> pre_if
            >> add_bb(pre_if) >> if_start    >> if_.if_stmt   >> if_end
            >> add_bb(pre_if) >> else_start  >> if_.else_stmt >> else_end
            >> add_bb(if_end, else_end)
            >> if_start->jump(*last_bb)
            >> pre_if->br    (cond,  *if_start, *else_start);
      if_start->dominator = else_start->dominator = last_bb->dominator = pre_if;
    },
    [&](tree::for_statement_t &for_) {
      breaks.emplace(); continues.emplace();
      basic_block *cond_bb, *loop_body, *loop_finish;
      tree::op cond;

      cfg() >> for_.clause     >> add_bb(last_bb) >> cond_bb << for_.cond >> cond
            >> add_bb(last_bb) >> loop_body >> for_.body >> for_.step >> loop_finish
            >> loop_finish->jump(*cond_bb)
            >> cond_bb->br(cond,  *loop_body, add_bb(cond_bb));

      cond_bb->add_pred(loop_finish);
      for(auto break_ : breaks.top()) {
        if(break_->insns.size() && (tree::jump) break_->insns.back()) break_->insns.pop_back();
        break_->jump(*last_bb);
        last_bb->add_pred(break_);
      }
      for(auto continue_ : continues.top()) {
        if(continue_->insns.size() && (tree::jump) continue_->insns.back()) continue_->insns.pop_back();
        continue_->jump(*cond_bb);
        cond_bb->add_pred(continue_);
      }
      breaks.pop(); continues.pop();
    },
    [&](tree::while_statement_t &while_) {
      breaks.emplace(); continues.emplace();
      basic_block *cond_bb, *loop_body, *loop_finish;
      tree::op cond;

      cfg() >> add_bb(last_bb) >> cond_bb << while_.cond >> cond
            >> add_bb(last_bb) >> loop_body >> while_.body
            >> last_bb->jump(*cond_bb)
            >> cond_bb->br(cond,  *loop_body, add_bb(cond_bb));

      cond_bb->add_pred(last_bb);
      for(auto break_ : breaks.top()) {
        if(break_->insns.size() && (tree::jump) break_->insns.back()) break_->insns.pop_back();
        break_->jump(*last_bb);
        last_bb->add_pred(break_);
      }
      for(auto continue_ : continues.top()) {
        if(continue_->insns.size() && (tree::jump) continue_->insns.back()) continue_->insns.pop_back();
        continue_->jump(*cond_bb);
        cond_bb->add_pred(continue_);
      }
      breaks.pop(); continues.pop();
    },
    [&](tree::do_while_statement_t &while_) {
      breaks.emplace(); continues.emplace();
      basic_block *loop_body;
      tree::op cond;

      cfg() >> add_bb(last_bb) >> loop_body >> while_.body
            >> while_.cond >> cond >> last_bb->br(cond, *loop_body, add_bb(last_bb));

      loop_body->add_pred(last_bb);
      for(auto break_ : breaks.top()) {
        if(break_->insns.size() && (tree::jump) break_->insns.back()) break_->insns.pop_back();
        break_->jump(*last_bb);
        last_bb->add_pred(break_);
      }
      for(auto continue_ : continues.top()) {
        if(continue_->insns.size() && (tree::jump) continue_->insns.back()) continue_->insns.pop_back();
        continue_->jump(*loop_body);
        loop_body->add_pred(continue_);
      }
      breaks.pop(); continues.pop();
    },
    [&](tree::switch_statement_t &switch_) {
      breaks.emplace();
      tree::op cond;
      switch_.cond = construct(switch_.cond);
      last_bb->add_insn(stmt);
      // basic blocks for jumps
      // used in unswitch()
      for(auto case_ : switch_.cases)
        cfg() >> add_bb(last_bb);
      cfg() >> add_bb(last_bb) >> switch_.stmt;

      if(!switch_.default_)
        switch_.default_ = tree::default_statement{{.bb = &add_bb(last_bb)}};

      for(auto break_ : breaks.top()) {
        if(break_->insns.size() && (tree::jump) break_->insns.back()) break_->insns.pop_back();
        break_->jump(*last_bb);
        last_bb->add_pred(break_);
      }
      breaks.pop();
    },
    [&](tree::case_statement_t &case_)       { case_.bb    = &add_bb(last_bb); },
    [&](tree::default_statement_t &default_) { default_.bb = &add_bb(last_bb); },
    [&](tree::break_statement_t)    { breaks   .top().emplace_back(last_bb); add_bb(last_bb); },
    [&](tree::continue_statement_t) { continues.top().emplace_back(last_bb); add_bb(last_bb); },
    [&](tree::return_statement_t &ret) {
      ret.expr = construct(ret.expr);
      last_bb->add_insn(stmt);
    },
    [&](tree::function_t &) {},
    [&](tree::variable_t &var) {
      if(var.scs == "static"_s) return;
      auto var_op = construct_var(tree::variable(stmt));
      if(auto init = (tree::initializer_list) var.definition)
        for(auto [offset, init] : *init)
          last_bb->add_insn(tree::load_addr{{.src = construct(init), .dst = var_op, .offset = offset }});
      else if(var.definition)
        last_bb->add_assign(construct_expr_no_op(var.definition), var_op);
    },
    [&](tree::block_decl_t &block) {
      for(auto decl : block) construct(decl);
    },
    [&]<narrow<tree::expression_t> T>(T &) {
      auto expr = construct_expr_no_op(tree::expression{tree::tree_value<T>(stmt)});
      last_bb->add_assign(expr, make_tmp(expr->type));
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

 // for(auto bb = &entry; bb; bb = bb->step())
    //bb->phis.clear();

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
            tree::mov pre{{.src = expr.lhs, .dst = mov->dst}};
            expr.lhs = pre->dst;
            std::swap(expr.lhs, expr.rhs);
            tree::compound_statement_t i{{}, { pre, *insn }};
            *insn = i;
          }
        });
      }
  });
}
void control_flow_graph::unswitch() {
  cfg_walker{entry}([&](basic_block &bb) {
    for(auto insn : bb.insns | iter_range)
      if(auto switch_ = (tree::switch_statement) *insn) {
        auto bbp = bb.step();
        for(auto case_ : switch_->cases) {
          tree::binary_expression_t binexpr{.op = "-"_s, .lhs = switch_->cond, .rhs = case_->cond};
          binexpr.type = tree::int_type_node;
          auto tmp = bbp->add_assign(binexpr, make_tmp(tree::int_type_node));
          bbp->br(tmp, *case_->bb, *bbp->step());
          bbp = bbp->step();
        }
        bbp->jump(*switch_->default_->bb);
        *insn = tree::empty_node_t{};
      }
  });
}
}}
