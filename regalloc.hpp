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
  std::vector<std::pair<tree::target_op, bool>> tab;
  std::vector<regalloc::live_interval> intervals;
  std::vector<live_interval> active;


  void compute_intervals() {
    for(cfg::basic_block *bb = &cfg.entry; bb; bb = bb->step()) {
      for(auto def : bb->def.map<tree::ssa_variable_t>()) {
        regalloc::live_interval li{def, bb, (size_t) -1, {}, {}};
        li.spill_pos = -1;
        bool start = true;
        for(cfg::basic_block *bb1 = bb; bb1; bb1 = bb1->step()) {
          size_t insn_count = bb1->insn_pos;
          for(auto insn : bb1->insns) {
            cfg::visit_ops(insn, [&](auto &op) {
              if(tree::op(op) == def) {
                if(start) {
                  li.start = insn_count;
                  li.finish = insn_count + 1;
                  start =false;
                } else {
                  li.finish = insn_count + 1;
                }
              }
            });
            ++insn_count;
          }
        }
        intervals.emplace_back(li);
      }

    }
  }
  std::pair<tree::target_op, bool> *next_reg() {
    if(auto p = std::ranges::find_if(tab, [](auto pair) { return pair.second; }); p != tab.end()) {
      p->second = false;
      return &*p;
    } else
      return {};
  }
  void free_reg(auto reg) { reg->second = true; }

 void process_interval(live_interval li) {
    size_t insn_cnt = li.entry->insn_pos;
    tree::target_op reg;
    if(li.reg) {
      reg = li.reg->first.cpy();
      reg->type = li.op->type;
    }

    for(cfg::basic_block *bb = li.entry; bb; bb = bb->step()) {;
      for(auto insn : bb->insns | iter_range) {
        if(reg) {
          if(insn_cnt == li.spill_pos)
            bb->insns.insert(insn, tree::mov{{.src = reg, .dst = li.op}});
          cfg::visit_ops(*insn, [&](auto &op) {
            if(tree::op(op) == li.op) op = reg;
          });
        } else {
          c9_assert(!li.reload);
          cfg::visit_ops(*insn, [&](auto &op) {
            if(tree::op(op) == li.op) {
              auto tmp = cfg.make_tmp(li.op->type);
              bb->insns.insert(insn, tree::mov{{ .src = li.op, .dst = tmp }});
              live_interval tmp_li{
                .op = tmp, .entry = bb, .start = insn_cnt, .finish = insn_cnt + 2, .reload = true
              };
              op = tmp;
              next_interval(tmp_li);
            }
          });
        }
        ++insn_cnt;
      }
    }
  }

  void next_interval(live_interval &i) {
    while(active.size()) {
      if(active.front().finish >= i.start) break;
      auto li = active.front();
      active.erase(active.begin());
      process_interval(li);
      free_reg(li.reg);
    }

    if(auto reg = next_reg()) {
      i.reg = reg;
      active.emplace_back(i);
    } else {
      auto p = active.end() - 1;
      while(p->reload) {
        c9_assert(p != active.begin());
        --p;
      }
      if(p->finish > i.finish || i.reload) {
        i.reg = p->reg;
        p->spill_pos = i.start;
        auto old = *p;
        active.erase(p);
        process_interval(old);
        active.emplace_back(i);
      }
      else {
        i.spill_pos = i.start;
        process_interval(i);
      }
    }
  }
public:
  cfg::control_flow_graph &cfg;
  void operator()() {
    compute_intervals();
    for(auto &i : intervals) next_interval(i);

    for(auto &a : active) process_interval(a);
  }

  template<class Reg> register_allocator(cfg::control_flow_graph &cfg, Reg reg, auto op) : cfg{cfg} {
    variant_types(reg)(for_each([&](auto reg) {
      tab.emplace_back(tree::target_op{{.data = op = Reg{reg} }}, true);
    }));
  }
};
}}
