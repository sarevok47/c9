#include "regalloc.hpp"

namespace c9 { namespace regalloc {
void register_allocator::compute_interval(cfg::basic_block *bb, tree::op def) {
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
void register_allocator::compute_intervals() {
  for(cfg::basic_block *bb = &cfg.entry; bb; bb = bb->step()) {
    for(auto def : bb->def.map<tree::ssa_variable_t>())
      compute_interval(bb, def);
    for(auto def : bb->def.map<tree::temporary_t>())
      compute_interval(bb, def);
  }
  std::ranges::sort(intervals, [](auto &a, auto &b) {
    if (a.start == b.start)
      return a.finish < b.finish;
    return a.start < b.start;
  });
}
std::pair<tree::target_op, bool> *register_allocator::next_reg() {
  if(auto p = std::ranges::find_if(tab, [](auto pair) { return pair.second; }); p != tab.end()) {
    p->second = false;
    return &*p;
  } else
    return {};
}


void register_allocator::get_spill_reg_for_call(std::pair<tree::target_op, bool> *reg, size_t insn_pos, std::list<tree::statement>::iterator insn) {
  while(active.size()) {
    for(auto p : active | iter_range)
      if(p->finish < insn_pos) {
        auto li = *p;
        active.erase(p);
        process_interval(li);
        free_reg(li.reg);
        goto repeat;
      } else if(p->reg == reg) {
        tree::compound_statement_t compound = {{}, std::vector<tree::statement>{
          tree::spill_statement{{.reg = p->reg->first, .op = p->op, }},
          *insn,
          tree::reload{{.reg = p->reg->first, .op = p->op,}},
        }};
        *insn = compound;
        return;
      }


    break;
    repeat:
  }
}
void register_allocator::reload(live_interval li, size_t insn_pos, std::list<tree::statement>::iterator insn) {
  for(auto p : active | iter_range)
    if(p->finish < insn_pos) {
      auto liold = *p;
      active.erase(p);
      process_interval(liold);
      free_reg(liold.reg);
      cfg::visit_ops(*insn, [&](auto &op) {
        if(tree::op(op) == li.op) op = liold.reg->first;
      });
      tree::compound_statement_t compound = {{}, std::vector<tree::statement>{
        tree::reload{{.reg = liold.reg->first, .op = li.op}},
        *insn
      }};
      *insn = compound;
      break;
    }
  auto reg = next_reg();
  c9_assert(reg);
  cfg::visit_ops(*insn, [&](auto &op) {
    if(tree::op(op) == li.op) op = reg->first;
  });
  tree::compound_statement_t compound = {{}, std::vector<tree::statement>{
    tree::reload{{.reg = reg->first, .op = li.op}},
    *insn
  }};
  *insn = compound;
}

void register_allocator::process_interval(live_interval li) {
  size_t insn_cnt = li.entry->insn_pos;
  tree::target_op reg;
  if(li.reg) {
    reg = li.reg->first.cpy();
    reg->type = li.op->type;
  }

  for(cfg::basic_block *bb = li.entry; bb; bb = bb->step()) {;
    for(auto insn : bb->insns | iter_range) {
      if(auto mov = (tree::mov) *insn)
        if(auto funcall = (tree::function_call) mov->src) {
          get_spill_reg_for_call(ret_reg, insn_cnt, insn);
          for(size_t i = 0; i != funcall->args.size(); ++i)
            get_spill_reg_for_call(call_regs[i], insn_cnt, insn);
        }

      if(reg && insn_cnt == li.spill_pos) {
        tree::compound_statement_t compound = {{}, std::vector<tree::statement>{
          tree::spill_statement{{.reg = reg, .op = li.op }},
          *insn,
        }};
        *insn = compound;
      }


      if(reg && insn_cnt <= li.spill_pos) {
        cfg::visit_ops(*insn, [&](auto &op) {
          if(tree::op(op) == li.op) op = reg;
        });
      } else {
        c9_assert(!li.reload);
        reload(li, insn_cnt, insn);
      }
      ++insn_cnt;
    }
  }
}

void register_allocator::next_interval(live_interval &i) {
  while(active.size()) {
    for(auto p : active | iter_range)
      if(p->finish < i.start) {
        auto li = *p;
        active.erase(p);
        process_interval(li);
        free_reg(li.reg);
        goto repeat;
      }
      break;
    repeat:
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
}}
