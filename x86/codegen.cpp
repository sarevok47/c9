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

data_type get_type(type_decl type) {
  return type(overload {
    [](auto &)                      -> data_type { c9_assert(0); },
    [](char_type_t &)               -> data_type { return "b"_s; },
    [](unsigned_char_type_t &)      -> data_type { return "b"_s; },
    [](int_type_t &)                -> data_type { return "l"_s; },
    [](unsigned_int_type_t &)       -> data_type { return "l"_s; },
    [](long_type_t &)               -> data_type { return "q"_s; },
    [](unsigned_long_type_t &)      -> data_type { return "q"_s; },
    [](long_long_type_t &)          -> data_type { return "q"_s; },
    [](unsigned_long_long_type_t &) -> data_type { return "q"_s; },
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
    [&](tree::target_op_t op) {
      return (x86::op) op.data;
    },
    [&](tree::ssa_variable_t &) {
      auto &pos = local_vars[operand];
      if(!pos) pos = sp += 4;
      return indirect_op{intreg::rbp, (int) pos};
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
      gen(mov.src, gen(mov.dst));
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

