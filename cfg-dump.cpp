#include "cfg.hpp"
#include <x86/target.hpp>
namespace c9 { namespace cfg {
void tree_dump(FILE *out, tree::statement tree) {
  if(!tree::expression(tree) && !tree::type_decl(tree))
    fprint(out, "\t");
  visit(tree, overload {
    [&](tree::mov_t &mov) {
  //    tree_dump(out, mov.dst->type);
      fprint(out, " ");
      tree_dump(out, mov.dst);
      fprint(out, " = ");
      tree_dump(out, mov.src);
    },
    [&](tree::variable_t &var) {
      fprint(out, "{}", var.name);
    },
    [&](tree::dereference_t deref) {
      fprint(out, "*");
      tree_dump(out, deref.expr);
    },
    [&](tree::load_addr_t load_addr) {
      fprint(out, "*");
      tree_dump(out, load_addr.dst);
      fprint(out, " = ");
      tree_dump(out, load_addr.src);
    },
    [&](tree::string_cst_expression_t &expr) {
      fprint(out, "\"{}\"", expr.value);
    },
    [&](tree::function_t &fun) {
      fprint(out, "{}", fun.name);
    },
    [&](tree::cst_t cst) {
      visit(cst.data, [&](auto v) { fprint(out, "{}", v); });
    },
    [&](tree::temporary_t tmp) {
      fprint(out, "__temp_{}", tmp.idx);
    },
    [&](tree::ssa_variable_t &var) {
      fprint(out, "__ssa_{}_{}", var.var->name, var.ssa_n, var.ssa_tab_n);
    },
    [&](tree::return_statement_t &ret) {
      fprint(out, "return ");
      tree_dump(out, ret.expr);
    },
    [&](tree::target_op_t &op) {
      visit(op.data, overload {
        [](auto &) {},
        [&](x86::op &op) { x86::dump_op(out, op, "q"_s); }
      });
    },
    [&](tree::jump_t jump) {
      fprint(out, "jump bb_{}", jump.target.i);
    },
    [&](tree::br_t &br) {
      fprint(out, "br ");
      tree_dump(out, br.cond);
      fprint(out, " ? bb_{} : bb_{}", br.true_.i, br.false_.i);
    },
    [&](tree::binary_expression_t &expr) {
      fprint(out, " ");
      tree_dump(out, expr.lhs);
      fprint(out, " {} ", visit(expr.op, [](auto s) { return s.c_str(); }));
      tree_dump(out, expr.rhs);
    },
    [&](tree::unary_expression_t &expr) {
      fprint(out, "{}", visit(expr.op, [](auto s) { return s.c_str(); }));
      tree_dump(out, expr.expr);
    },
    [&](tree::reload_t &reload) {
      fprint(out, "reload(");
      tree_dump(out, reload.op);
      fprint(out, ", ");
      tree_dump(out, reload.reg);
      fprintln(out, ")");
    },
    [&](tree::spill_statement_t &spill) {
      fprint(out, "spill(");
      tree_dump(out, spill.op);
      fprint(out, ", ");
      tree_dump(out, spill.reg);
      fprintln(out, ")");
    },
    [&](tree::function_call_t &fun_call) {
      tree_dump(out, fun_call.calee);
      fprint(out, "(");
      for(auto arg : fun_call.args | iter_range) {
        tree_dump(out, *arg);
        if(arg + 1 != fun_call.args.end()) fprint(out, ", ");
      }
      fprintln(out, ")");
    },
    [&](tree::compound_statement_t &compound) {
      for(auto stmt : compound) tree_dump(out, stmt);
    },
    [&](tree::phi_t phi) {
      fprint(out, "phi ");
      for(auto elt : phi.elts | iter_range) {
        tree_dump(out, *elt);
        ++elt;
        if(elt != phi.elts.end()) fprint(out, ", ");
        --elt;
      }
    },
    [&](narrow<tree::integer_type_t> auto &tree) {
      fprint(out, "{}", tree.name);
    },
    [&](auto &) {
      fprint(out, "{}", __PRETTY_FUNCTION__);
    }
  });
  if(!tree::expression(tree) && !tree::op(tree) && !tree::type_decl(tree))
    fprintln(out, "");
}

void basic_block::dump(FILE *out) {
  fprintln(out, "bb_{}:", i);
  fprint(out, "\t // defs: ");
  def.visit_each([&](auto &def) {
    for(auto def : def) {
      tree_dump(out, def);
      fprint(out, " ");
    }
  });

  fprint(out, "\n\t // uses: ");
  use.visit_each([&](auto &use) {
    for(auto use : use) {
      tree_dump(out, use);
      fprint(out, " ");
    }
  });
  fprint(out, "\n\t // preds: ");
  for(auto pred : this->preds)
    fprint(out, "bb_{} ", pred->i);
  fprint(out, "\n\t // succs: ");
  for(auto succ : this->succs)
    fprint(out, "bb_{} ", succ->i);
  if(dominator)
     fprintln(out, "\n\t // dominator: bb_{}", dominator->i);
  fprintln(out, "");

  for(auto phi : phis)
    tree_dump(out, tree::mov{{.src = phi.value.first, .dst = phi.value.second }});
  for(auto insn : insns)
    tree_dump(out, insn);
}
}}
