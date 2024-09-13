#pragma once


#include "tree-opt.hpp"
namespace c9 { namespace tree_opt {
using namespace tree;
using namespace cfg;

bool operator==(expression lhs, expression rhs) {
  if(op(lhs) && op(rhs)) return op(lhs) == op(rhs);
  return visit(lhs, rhs, overload {
    [&](binary_expression_t lhs, binary_expression_t rhs) {
      bool r{};
      if(lhs.op == rhs.op) {
        r = lhs.op != "%"_s && rhs.op != "/"_s && lhs.lhs == rhs.rhs && lhs.rhs == rhs.lhs;
        if(!r) r = lhs.lhs == rhs.lhs && lhs.rhs == rhs.rhs;
      }
      return r;
    },
    [](auto &, auto &) {  return false; }
  });
}
static op find_expression_in_range(auto begin, auto end, expression expr) {
  for(; begin != end; ++begin)
    if(auto move = (tree::mov) *begin)
      if(move->src == expr) return move->dst;
  return {};
}
static void cse(basic_block &bb) {
  for(auto insn : bb.insns | iter_range)
    if(auto move = (tree::mov) *insn) {
      if(auto op = find_expression_in_range(bb.insns.begin(), insn, move->src))
        move->src = op;
      else
        bb.visit_dominators([&](basic_block &bb) {
          if(auto op = find_expression_in_range(bb.insns.begin(), bb.insns.end(), move->src))
            move->src = op;
        });
    }
}
void cse(cfg::control_flow_graph &cfg) {
  for(basic_block *bb = &cfg.entry; bb; bb = bb->step())  cse(*bb);
}

}}
