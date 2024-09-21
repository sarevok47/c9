#pragma once

#include "target.hpp"



auto visitor(auto &&f) { return [&](auto &&value) { return f(value); }; }
namespace c9 { namespace x86 {
using namespace tree;

void codegen::gen(cfg::basic_block &entry) {
  for(auto bb = &entry; bb; bb = bb->step()) {
    size_t insn_idx = insns.size();
    for(auto insn : bb->insns) gen(insn);

    fprintln(stderr, "bb_{}:", bb->i);
    for(; insn_idx != insns.size(); ++insn_idx) dump_insn(stderr, insns[insn_idx]);
  }
}

data_type get_type(type_decl type) {
  return type(overload {
    [](auto &)       -> data_type { c9_assert(0); },
    [](int_type_t &) -> data_type { return ""_s; }
  });
}

insn make_binary_insn(lex::binary_tok binary_op, data_type type, op lhs, op rhs) {
  return visit(binary_op, overload {
    [](auto) -> insn {},
    [&](decltype("+"_s)) -> insn { return add{type, {lhs, rhs}}; },
    [&](decltype("-"_s)) -> insn { return sub{type, {lhs, rhs}}; },
  });
}

op codegen::gen(tree::op operand) {
  return operand(overload {
    [&](tree::ssa_variable_t &ssa_) -> op {
      auto ssa = *unssa(ssa_);
      if(!ssa_vars[ssa.ssa_tab_n])
        ssa_vars[ssa.ssa_tab_n] = alloc_virt_reg(ssa);
      if(ssa.type.is_narrow<tree::floating_type_t>())
        return xmmreg{ssa_vars[ssa.ssa_tab_n]};
      return intreg{ssa_vars[ssa.ssa_tab_n]};
    },
    [&](tree::temporary_t tmp) -> op {
      if(!tmps[tmp.idx])
        tmps[tmp.idx] = alloc_virt_reg(tmp);
      if(tmp.type.is_narrow<tree::floating_type_t>())
        return xmmreg{tmps[tmp.idx]};
      return intreg{tmps[tmp.idx]};
    },
    [&](cst_t cst) {
      return (int) (__uint128_t) cst.data;
    },
    [](auto &) -> op {}
  });
}
void codegen::gen(tree::expression expr, op dst) {
  expr(overload {
    [](auto &) {},
    [&](narrow<tree::op_t> auto &op) {
      *this << mov{ get_type(op.type), {gen(tree::op(expr)), dst} };
    },
    [&](binary_expression_t expr) {
      if(expr.op == "||"_s || expr.op == "&&"_s) {

      } else {
        auto src = gen(tree::op(expr.lhs));
        gen(expr.rhs, dst);
        *this << make_binary_insn(expr.op, get_type(expr.type), src, dst);
      }
    }
  });
}
void codegen::gen(tree::statement stmt) {
  stmt(overload {
    [](auto &) {},
    [&](mov_t mov) {
      gen(mov.src, gen(mov.dst));
    },
    [&](br_t br) {
      *this << test{get_type(br.cond->type), {gen(br.cond), gen(br.cond)}};
      *this << jcc{{br.false_}};
    },
    [&](jump_t jump) {
      *this << jmp{jump.target};
    }
  });
}
}}
