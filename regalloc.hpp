#pragma once


#include "cfg.hpp"
#include "variant.hpp"

namespace c9 { namespace regalloc {
struct live_interval {
  tree::op op;
  cfg::basic_block *entry;
  size_t start{}, finish{}, spill_pos = -1;
  std::pair<tree::target_op, bool> *reg{};
  bool reload{};
};
class register_allocator {
  std::vector<regalloc::live_interval> intervals;
  std::vector<live_interval> active;

  void compute_interval(cfg::basic_block *bb, tree::op def);
  void compute_intervals();
  std::pair<tree::target_op, bool> *next_reg();
  void get_spill_reg_for_call(std::pair<tree::target_op, bool> *reg, size_t insn_pos, std::list<tree::statement>::iterator insn);
  void reload(live_interval li, size_t insn_pos, std::list<tree::statement>::iterator insn);
  void process_interval(live_interval li);
  void next_interval(live_interval &i);

  void free_reg(auto reg) { reg->second = true; }
public:
  std::vector<std::pair<tree::target_op, bool>> tab;
  std::vector<std::pair<tree::target_op, bool> *> call_regs;
  std::pair<tree::target_op, bool> * ret_reg;
  cfg::control_flow_graph &cfg;

  void operator()() {
    compute_intervals();
    for(auto &i : intervals) next_interval(i);

    for(auto &a : active) process_interval(a);
  }

  template<class Reg> register_allocator(cfg::control_flow_graph &cfg, Reg reg, auto op, auto &&call_regs, auto ret_reg) : cfg{cfg}{
    for(size_t i = 0; i != size_t(Reg::num_of); ++i)
      tab.emplace_back(tree::target_op{{.data = op = Reg{i} }}, true);
    for(auto reg : call_regs)
      this->call_regs.emplace_back(&tab[size_t(reg)]);
    this->ret_reg = &tab[size_t(ret_reg)];
  }
};
}}
