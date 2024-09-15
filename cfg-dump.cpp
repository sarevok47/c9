#include "cfg.hpp"

namespace c9 { namespace cfg {
void tree_dump(FILE *out, tree::type_decl tree) {}
void tree_dump(FILE *out, tree::statement tree) {
  if(!tree::expression(tree))
    fprint(out, "\t");
  visit(tree, overload {
    [&](tree::mov_t &mov) {
      tree_dump(out, mov.dst);
      fprint(out, " = ");
      tree_dump(out, mov.src);
    },
    [&](tree::variable_t &var) {
      fprint(out, "{}", var.name);
    },
    [&](tree::cst_t cst) {
      visit(cst.data, [&](auto v) { fprint(out, "{}", v); });
    },
    [&](tree::temporary_t tmp) {
      fprint(out, "__temp_{}", tmp.idx);
    },
    [&](tree::ssa_variable_t &var) {
      fprint(out, "__ssa_{}_{}", var.var->name, var.ssa_n);
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
      tree_dump(out, expr.type);
      fprint(out, " ");
      tree_dump(out, expr.lhs);
      fprint(out, " {} ", visit(expr.op, [](auto s) { return s.c_str(); }));
      tree_dump(out, expr.rhs);
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
    [&](auto &) {
      fprint(out, "{}", __PRETTY_FUNCTION__);
    }
  });
  if(!tree::expression(tree) && !tree::op(tree))
    fprintln(out, "");
}

void basic_block::dump(FILE *out) {
  fprintln(out, "bb_{}:", i);
  fprint(out, "\t // defs: ");
  for(auto def : this->def) {
    tree_dump(out, def);
    fprint(out, " ");
  }
  fprint(out, "\n\t // uses: ");
  for(auto use : this->use) {
    tree_dump(out, use);
    fprint(out, " ");
  }
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
    tree_dump(out, tree::mov{{.src = phi.value.first, .dst = tree::ssa_variable{{ .var = phi.key, .ssa_n = phi.value.second}} }});
  for(auto insn : insns)
    tree_dump(out, insn);
}
}}
