#include "regalloc.hpp"

namespace c9 { namespace regalloc {
void register_allocator::compute_interval(cfg::basic_block *bb, tree::op def) {
  regalloc::live_interval li{def, bb};
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
  intervals.emplace(li);
}
std::pair<tree::target_op, bool> *register_allocator::next_reg() {
  if(auto p = std::ranges::find_if(tab, [](auto pair) { return pair.second; }); p != tab.end()) {
    p->second = false;
    return &*p;
  } else
    return {};
}

template<class T> T comp_exp_cast(tree::base tree) {
  if(auto r = (T) tree) return r;
  if(auto c = (tree::compound_statement) tree)
    for(auto tree : *c) return comp_exp_cast<T>(tree);
  return {};
}
template<class ...T> void process_trees(tree::base tree, auto &&f) {
  if(auto c = (tree::compound_statement) tree)
    for(auto tree : *c) process_trees<T...>(tree, f);
  else {
    bool match{};
    (([&] { if(auto t = (T) tree) match = true, f(t); }()), ...);
    if(!match) f(tree);
  }
}

void register_allocator::get_spill_reg_for_call(std::pair<tree::target_op, bool> *reg, size_t insn_pos, std::list<tree::statement>::iterator insn, tree::op dst) {
  while(active.size()) {
    for(auto p : active | iter_range)
      if(p->finish < insn_pos) {
        auto li = *p;
        active.erase(p);
        process_interval(li);
        free_reg(li.reg);
        goto repeat;
      } else if(p->reg == reg && p->start < insn_pos && p->op != dst) {
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
  if(auto mov = comp_exp_cast<tree::mov>(*insn))
    if(auto addr = (tree::addressof) mov->src)
      return;
  if(bool b{}; cfg::visit_ops(*insn, [&](auto &op) { b |= tree::op(op) == li.op; }), !b)
    return;

  auto reload = [&](auto reg) {
    bool src{}, dst{};

    auto process_ops = [&](auto &i) { cfg::visit_ops(i, [&](auto &op) { if(tree::op(op) == li.op) src = true, op = reg; }); };
    process_trees<tree::mov, tree::load_addr>(*insn, overload {
      [&](tree::mov mov) {
        if(mov->dst == li.op) dst = true, mov->dst = reg;
        process_ops(mov->src);
      },
      [&](tree::load_addr load_addr) { process_ops(load_addr->src); },
      [&](auto) { process_ops(*insn); }
    });


    tree::compound_statement_t compound;
    if(src) compound.emplace_back(tree::reload{{.reg = reg, .op = li.op}});
    compound.emplace_back(*insn);
    if(dst) compound.emplace_back(tree::spill_statement{{.reg = reg, .op = li.op, }});
    *insn = compound;
  };

  for(auto p : active | iter_range)
    if(p->finish < insn_pos) {
      auto liold = *p;
      active.erase(p);
      process_interval(liold);
      free_reg(liold.reg);
      auto reg = liold.reg->first.cpy();
      reg->type = li.op->type;
      return reload(reg);
    }

  auto reg = next_reg()->first.cpy();
  reg->type = li.op->type;
  reload(reg);
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
      if(auto mov = comp_exp_cast<tree::mov>(*insn))
        if(auto funcall = (tree::function_call) mov->src) {
          if(spill_ret) get_spill_reg_for_call(ret_reg, insn_cnt, insn, mov->dst);
          for(size_t i = 0; i != funcall->args.size() && i < std::size(call_regs); ++i)
            get_spill_reg_for_call(call_regs[i], insn_cnt, insn, mov->dst);

          if(spill_sofs != -1 && !funcall->args.size()
            && std::ranges::any_of(funcall->args, [&](auto a) { return a->type->size > spill_sofs; })
          )
            get_spill_reg_for_call(call_regs[0], insn_cnt, insn, mov->dst);
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

  if(tree::variable(i.op)) {
    i.spill_pos = i.start;
    process_interval(i);
    return;
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
