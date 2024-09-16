#pragma once

#include "tree-opt.hpp"

namespace c9 { namespace tree_opt {
using namespace cfg;
void constprop(control_flow_graph &cfg) {
  flat_map<tree::ssa_variable, tree::op> var_tab;
  tree::op temp_tab[cfg.ntmp];

  cfg_walker walk{cfg.entry};

  walk([&](basic_block &bb) {
    auto try_propagate = [&var_tab, &bb, t = (tree::op *) temp_tab](auto &value) {
      for(;;)
        if(auto temp = (tree::temporary) value; temp && t[temp->idx])
          bb.use.erase(temp), value = t[temp->idx];
        else if(auto ssa = (tree::ssa_variable) value; ssa && var_tab[ssa])
          bb.use.erase(ssa),  value = var_tab[ssa];
        else break;
    };

    for(auto phi : bb.phis)
      if(phi.value.first->elts.size() == 1)
        var_tab[tree::ssa_variable{{.var = phi.key, .ssa_n = phi.value.second}}] = *phi.value.first->elts.begin();

    for(auto insn : bb.insns)
      if(auto mov = (tree::mov) insn) {
        mov->src(overload {
          [&](auto &) { },
          [&](narrow<tree::op_t> auto &) {
            try_propagate(mov->src);
          },
          [&](tree::binary_expression_t &binexpr) {
            try_propagate(binexpr.lhs);
            try_propagate(binexpr.rhs);
          }
        });

        if(!tree::variable(mov->dst) && tree::op(mov->src)) {
          if(auto var = tree::ssa_variable(mov->dst))
            var_tab[var] = tree::op(mov->src);
          else
            temp_tab[tree::temporary(mov->dst)->idx] = tree::op(mov->src);
        }
        if(tree::phi(mov->src) && tree::phi(mov->src)->elts.size() == 1) {
          auto el = *tree::phi(mov->src)->elts.begin();
          mov->src = el;
          if(auto var = tree::ssa_variable(mov->dst))
            var_tab[var] = el;
          else
            temp_tab[tree::temporary(mov->dst)->idx] = el;
        }
      }
  });
}
}}
