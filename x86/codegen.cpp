#pragma once

#include "target.hpp"



auto visitor(auto &&f) { return [&](auto &&value) { return f(value); }; }
namespace c9 { namespace x86 {
using namespace tree;

void codegen::gen(cfg::basic_block &entry) {
  *this << sub{"q"_s, {0, intreg::rsp}};
  for(auto bb = &entry; bb; bb = bb->step()) {
    size_t insn_idx = insns.size();
    label_list.emplace_back(insn_idx, bb->i);
    for(auto insn : bb->insns) gen(insn);
  }

  ((sub &) insns.front()).ops[0] = (int) sp;
  for(auto add : ret_insert_add_sp_pos) *add = (int) sp;
}

void codegen::dump(FILE *out) {
  auto l = label_list.begin();
  for(size_t i = 0; i != insns.size(); ++i) {
    if(i == l->first)
      fprintln(out, ".bb_{}:", (l++)->second);
    dump_insn(out, insns[i]);
  }

  while(l != label_list.end())
    fprintln(out, ".bb_{}:", (l++)->second);
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
    [&](tree::target_op_t op) {
      return (x86::op) op.data;
    },
    [&](tree::variable_t &var) -> op {
      return var.is_global ? memop{intreg::rip, var.name} : ({
        auto &pos = local_vars[operand];
        if(!pos) pos = sp += 4;
        memop{intreg::rsp, (int) pos};
      });
    },
    [&](cst_t cst) {
      return (int) (__uint128_t) cst.data;
    },
    [&](tree::function_t &fun) { return fun.name; },
    [](auto &) -> op { c9_assert(0); }
  });
}
void codegen::gen(tree::expression expr, op dst) {
  expr(overload {
    [](auto &) {},
    [&](narrow<tree::op_t> auto &op) {
      *this << mov{ get_type(op.type), {gen(tree::op(expr)), dst} };
    },
    [&](cast_expression_t &cast) {
      gen(cast.cast_from, dst);
      *this << movsx{get_type(cast.type), get_type(cast.cast_from->type), {dst, dst}};
    },
    [&](dereference_t &deref) {
      gen(deref.expr, dst);
      *this << mov{get_type(deref.type), { memop{dst, 0}, dst}};
    },
    [&](addressof_t &addr) {
      *this << lea{get_type(addr.type), { gen(tree::op(addr.expr)), dst }};
    },
    [&](unary_expression_t &expr) {
      gen(tree::op(expr.expr), dst);

      visit(expr.op, overload {
        [&](decltype("~"_s)) {
          *this << not_{get_type(expr.type), {dst}};
        },
        [](auto &) {

        }
      });
    },
    [&](function_call_t &fcall) {
      intreg *it = int_call_conv_sysv;
      for(auto arg : fcall.args) {
        if(it != std::end(int_call_conv_sysv))
          ++it, gen(arg, *it);
        else {
          // push
        }
      }
      *this << call{gen(tree::op(fcall.calee)) };
      // TODO ADD TYPE HANDLER INSTEAD OF "q"_s
      *this << mov{"q"_s, {intreg::rax, dst}};
    },
    [&](string_cst_expression_t &cst) {
      *this << mov{"q"_s, {cst.sym, dst}};
    },
    [&](binary_expression_t expr) {
      auto src = gen(tree::op(expr.lhs)), dst = gen(tree::op(expr.rhs));
      *this << make_binary_insn(expr.op, get_type(expr.type), src, dst);
    }
  });
}
void codegen::gen(tree::statement stmt) {
  stmt(overload {
    [](auto &) {},
    [&](mov_t mov) {
      auto dst = gen(mov.dst);
      gen(mov.src, dst);
    },
    [&](compound_statement_t &compound) {
      for(auto stmt : compound) gen(stmt);
    },
    [&](spill_statement_t &spill) {
      auto reg = gen(tree::op(spill.reg));
      if(auto var = (tree::variable) spill.op; var && (var->is_global || var->scs == "static"_s))
        *this << mov{get_type(spill.op->type), {reg, memop{intreg::rip, var->name} }};
      else {
        auto &pos = local_vars[spill.op];
        if(!pos) pos = sp += 4;
        *this << mov{get_type(spill.op->type), {reg, memop{intreg::rsp, (int) pos} }};
      }
    },
    [&](reload_t &reload) {
      auto reg = gen(tree::op(reload.reg));
      if(auto var = (tree::variable) reload.op; var && (var->is_global || var->scs == "static"_s))
        *this << mov{get_type(reload.op->type), {memop{intreg::rip, var->name}, reg }};
      else {
        auto &pos = local_vars[reload.op];
        if(!pos) pos = sp += 4;
        *this << mov{get_type(reload.op->type), {memop{intreg::rsp, (int) pos}, reg }};
      }
    },
    [&](load_addr_t &load) {
      *this << mov(get_type(load.src->type), { gen(load.src), memop{gen(load.dst)} });
    },
    [&](br_t br) {
      *this << test{get_type(br.cond->type), {gen(br.cond), gen(br.cond)}};
      *this << jcc{{br.false_}};
    },
    [&](jump_t jump) {
      *this << jmp{jump.target};
    },
    [&](return_statement_t &r) {
      gen(r.expr, intreg::rax);
      *this << add{"q"_s, {0, intreg::rsp}};
      ret_insert_add_sp_pos.emplace_back(&(int &) ((add &) insns.back()).ops[0]);
      *this << ret{};
    }
  });
}
}}

