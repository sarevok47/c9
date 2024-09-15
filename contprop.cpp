#pragma once

#include "tree-opt.hpp"

namespace c9 { namespace tree_opt {
using namespace cfg;
void constprop(control_flow_graph &cfg) {
  flat_map<tree::op, tree::op> tab;
  for(basic_block *bb = &cfg.entry; bb; bb = bb->step())
    for(auto insn : bb->insns) {



      if(auto mov = (tree::mov) insn) {
        if(auto op = (tree::op) mov->src)
          tab[mov->dst] = op;
      }
    }
}
}}
