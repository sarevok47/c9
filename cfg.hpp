#pragma once

#include <vector>
#include <set>
#include "tree.hpp"
#include "tree-trait.hpp"
#include "simple.hpp"
namespace c9 { namespace tree { static bool operator<(tree::op lhs, tree::op rhs) { return lhs.get_data() < rhs.get_data();  }} }
namespace c9 { namespace cfg {
void tree_dump(FILE *out, tree::statement tree);
struct basic_block {
  size_t i{};
  std::vector<tree::statement> insns;

  std::set<basic_block *> preds, succs;
  std::set<tree::op> def, use;

  basic_block *dominator{};

  basic_block &push(basic_block bb) { return *(next = new auto{mov(bb)}); }
  void add_insn(tree::statement insn);
  tree::op add_assign(tree::expression insn, tree::op dst);

  basic_block *step() { return next; }

  basic_block &jump(basic_block &target) {
    add_insn(tree::jump{{.target = target}});
    return *this;
  }
  basic_block &br(tree::expression cond, basic_block &true_bb, basic_block &false_bb) {
    add_insn(tree::br{{.cond = cond, .true_ = true_bb, .false_ = false_bb}});
    return *this;
  }

  void dump(FILE *out);

  void search_def_for_phi(tree::ssa_variable, std::set<tree::op> &out);
  void place_phi(tree::ssa_variable);

  void visit_dominators(auto &&f) {
    for(auto bb = this; bb = bb->dominator; )
      f(*bb);
  }

  basic_block() = default;
  basic_block(size_t i, std::same_as<basic_block *> auto ...preds) : i{i}, preds{preds...} {}
private:
  basic_block *next{};
};
class control_flow_graph {public:
  driver &d;
  size_t nlabel = 1, ntmp{};
  basic_block entry, *last_bb = &entry;
  tree::op last_op;

  tree::op make_tmp() { return tree::temporary{{.idx = ntmp++ }}; }

  basic_block &add_bb(auto ...preds) {
    last_bb = &last_bb->push({nlabel++, preds...});
    ((preds->succs.emplace(last_bb)), ...);
    ((last_bb->succs.emplace(preds)), ...);

    if constexpr(sizeof...(preds) == 1)
      last_bb->dominator = (preds, ...);
    else
      last_bb->dominator = 0_c(preds...)->dominator;
    return *last_bb;
  }

  friend struct cfg_stream;
  struct cfg_stream cfg();

  tree::expression construct_expr_no_op(tree::expression expr);
  tree::op construct(tree::expression expr);
  void construct(tree::statement stmt);

  void dump();
};

}}
