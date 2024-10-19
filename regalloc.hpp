#pragma once


#include "cfg.hpp"
#include "variant.hpp"

namespace c9 { namespace regalloc {
struct live_interval {
  tree::op op;
  cfg::basic_block *entry;
  size_t start{}, finish{1}, spill_pos = -1;
  std::pair<tree::target_op, bool> *reg{};
  bool reload{};

  bool operator==(live_interval li) { return op == li.op; }
  bool operator<(live_interval li) const {
     if (start == li.start)
      return finish < li.finish;
    return start < li.start;
  }
};
class register_allocator {
  // By some reason flat_set works without bug
  flat_set<regalloc::live_interval> intervals;
  std::vector<live_interval> active;

  void compute_interval(cfg::basic_block *bb, tree::op def);
  void compute_intervals(auto pred) {
    for(auto def : cfg.vars.map<tree::variable_t>())
      if(pred(def->type)) compute_interval(&cfg.entry, def);
    for(cfg::basic_block *bb = &cfg.entry; bb; bb = bb->step()) {
      for(auto def : bb->def.map<tree::ssa_variable_t>())
        if(pred(def->type)) compute_interval(bb, def);
      for(auto def : bb->def.map<tree::temporary_t>())
        if(pred(def->type)) compute_interval(bb, def);
    }

    #if 0
    for(auto interval : intervals) {
      fprint(stderr, "interval: '");
      cfg::tree_dump(stderr, interval.op);
      fprintln(stderr, "' start: {}, finish: {}", interval.start, interval.finish);
  }
  #endif
  }
  std::pair<tree::target_op, bool> *next_reg();
  void get_spill_reg_for_call(std::pair<tree::target_op, bool> *reg, size_t insn_pos, std::list<tree::statement>::iterator insn, tree::op dst);
  void reload(live_interval li, size_t insn_pos, std::list<tree::statement>::iterator insn);
  void process_interval(live_interval li);
  void next_interval(live_interval &i);

  void free_reg(auto reg) { reg->second = true; }
public:
  std::vector<std::pair<tree::target_op, bool>> tab;
  std::vector<std::pair<tree::target_op, bool> *> call_regs;
  std::pair<tree::target_op, bool> * ret_reg;
  bool spill_ret{};
  size_t spill_sofs = -1;
  cfg::control_flow_graph &cfg;

  void operator()(auto pred) {
    compute_intervals([pred](tree::type_decl type) { return pred(strip_type(type)); });
    for(auto i : intervals) next_interval(i);

    for(auto &a : active) process_interval(a);
  }

  template<class Reg> register_allocator(cfg::control_flow_graph &cfg, Reg reg, auto op, auto &&call_regs, auto ret_reg) : cfg{cfg} {
    for(size_t i = 0; i != size_t(Reg::num_of); ++i)
      tab.emplace_back(tree::target_op{{.data = op = Reg{i} }}, true);
    for(auto reg : call_regs)
      this->call_regs.emplace_back(&tab[size_t(reg)]);
    this->ret_reg = &tab[size_t(ret_reg)];
  }
};
}}
