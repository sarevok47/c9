#pragma once

#include "target.hpp"
#include "tree-dump.hpp"

auto visitor(auto &&f) { return [&](auto &&value) { return f(value); }; }
namespace c9 { namespace x86 {
using namespace tree;

void function_codegen::gen(cfg::basic_block &entry) {
  *this << push{"q"_s, {intreg::rbp}} << mov{"q"_s, {intreg::rsp, intreg::rbp}};
  size_t sub_sp = insns.size();
  *this << sub{"q"_s, {0, intreg::rsp}};

  std::vector<insn> store_reg_insns;

  std::pair int_{int_call_conv_sysv.begin(), int_call_conv_sysv.end()};
  std::pair xmm{xmm_call_conv_sysv.begin(), xmm_call_conv_sysv.end()};
  for(size_t i = 0; i != cfg.params.size(); ++i) {
    auto &param = cfg.params[i];
    auto type = strip_type(cfg.function->ftype()->params[0].type);

    int param_sp = 8;
    auto store_param = [&](auto &pair, op store_reg, op op, auto type) {
      bool store = op.is<memop>() || pair.first == pair.second;
      x86::op param_op = store ? store_reg : op;
      insn insn;
      insn = pair.first != pair.second
             ? mov{type, {*pair.first++, param_op}}
             : mov{type, {memop{intreg::rbp, param_sp += 8}, param_op}};
      if(!param.op) return;
      if(store) {
        store_reg_insns.emplace_back(insn);
        store_reg_insns.emplace_back(mov{type, {store_reg, op}});
      } else
        *this << insn;
    };
    auto p = [&](auto param) { return !param.op ? op{} : (param.reg ? param.reg->data[0_c] : gen(param.op)); };
    type(overload {
      [](auto &) {},
      [&](narrow<tree::floating_type_t> auto &) {
        store_param(xmm, xmmreg::xmm0, p(param), get_type(type));
      },
      [&](narrow<tree::pointer_t>      auto &type) { store_param(int_, intreg::rax, p(param), get_type(type)); },
      [&](narrow<tree::integer_type_t> auto &type) { store_param(int_, intreg::rax, p(param), get_type(type)); },
      [&](narrow<tree::structural_decl_t> auto &struct_) {
        if(!param.op)
          param_sp += struct_.size;
        else if(struct_.size <= 16) {
          auto op = (memop) p(param);
          struct_.definition->for_each([&](tree::record_member field) {
            auto type = strip_type(field->type);
            if((tree::floating_type) type)
              store_param(xmm, xmmreg::xmm0, op, get_type(type));
            else
              store_param(int_, intreg::rax, op, get_type(type));
            op.offset += type->align;
            param_sp  += type->align;
          });
        } else {
          local_vars[param.op] = param_sp += 8;
          param_sp += struct_.size;
        }
      }
    });
  }
  insns.insert(insns.end(), store_reg_insns.begin(), store_reg_insns.end());

  for(auto bb = &entry; bb; bb = bb->step()) {
    size_t insn_idx = insns.size();
    label_list.emplace_back(insn_idx, bb->i);
    for(auto insn : bb->insns) gen(insn);
  }

  ((sub &) insns[sub_sp]).ops[0] = -sp + 8;
  for(auto add : ret_insert_add_sp_pos) ((x86::add &) insns[add]).ops[0] = -sp + 8;
}

void function_codegen::dump(FILE *out) {
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

op function_codegen::gen(tree::op operand) {
  return operand(overload {
    [&](tree::target_op_t op) {
      return (x86::op) op.data;
    },
    [&](tree::variable_t &var) -> op {
      return var.is_global ? memop{intreg::rip, 0, var.name} : ({
        auto &pos = local_vars[operand];
        if(!pos) pos = sp -= var.type->size;
        memop{intreg::rbp, (int) pos};
      });
    },
    [&](tree::ssa_variable_t &var) -> op {
      return ({
        auto &pos = local_vars[operand];
        if(!pos) pos = sp -= var.type->size;
        memop{intreg::rbp, (int) pos};
      });
    },
    [&](cst_t cst) {
      return (int) (__uint128_t) cst.data;
    },
    [&](tree::function_t &fun) { return fun.name; },
    [](auto &) -> op { c9_assert(0); }
  });
}
void function_codegen::gen(tree::expression expr, op dst) {
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
      *this << mov{get_type(deref.type), { memop{gen(tree::op(deref.expr)), 0}, dst}};
    },
    [&](access_member_t &access) {
      if(access.addr) *this << lea{get_type(access.member->type), { memop{ gen(tree::op(access.expr)), (int) access.member.offset}, dst }};
      else            *this << mov{get_type(access.member->type), { memop{ gen(tree::op(access.expr)), (int) access.member.offset}, dst }};
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
      std::pair int_{int_call_conv_sysv.begin(), int_call_conv_sysv.end()};
      std::pair xmm{xmm_call_conv_sysv.begin(), xmm_call_conv_sysv.end()};
      for(auto arg : fcall.args) {
        auto store_arg = [&](auto &pair, op op, auto type) {
          if(pair.first != pair.second)  *this << mov{type, {op, *pair.first++}};
          else *this << mov{type, {op, memop{intreg::rbp, sp -= 8}}}, sp;
        };
        strip_type(arg->type)(overload {
          [](auto &) {},
             [&](narrow<tree::floating_type_t> auto &) {
               store_arg(xmm, gen(tree::op(arg)), get_type(arg->type));
             },
             [&](narrow<tree::pointer_t>      auto &type) { store_arg(int_, gen(tree::op(arg)), get_type(arg->type)); },
             [&](narrow<tree::integer_type_t> auto &type) { store_arg(int_, gen(tree::op(arg)), get_type(arg->type)); },
             [&](narrow<tree::structural_decl_t> auto &struct_) {
                auto op = (memop) gen(tree::op(arg));

                auto type = strip_type(arg->type);
                if(type->size <= 16) {
                  has_xmmnum_0(type)
                    ? store_arg(xmm, op, type->size > 4 ? data_type{"sd"_s} : data_type{"ss"_s})
                    : store_arg(int_, op, type->size > 8 ? data_type{"q"_s} : get_int_type(type->size));
                  if(type->size > 8)
                    op.offset += size(get_type(type)) - 8,
                    has_xmmnum_1(type)
                      ? store_arg(xmm, op, get_int_type(type->size - 8))
                      : store_arg(int_, op, get_int_type(type->size - 8));
                } else
                  for(size_t i{}; i <= struct_.size; op.offset += 8, i += 8) {
                    auto type = struct_.size - i < 8 ? get_int_type(struct_.size - i) : data_type("q"_s);
                    *this << mov{type, {op, intreg::rax}};
                    *this << mov{type, {intreg::rax, memop{intreg::rbp, sp}}}, sp -= 8;
                  }
             }
        });
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
void function_codegen::gen(tree::statement stmt) {
  auto genmem = [&](auto insn) {
    bool reload = __is_same(decltype(insn), reload_t);
    auto reg = gen(tree::op(insn.reg));
    memop memop;
    if(auto var = (tree::variable) insn.op; var && (var->is_global || var->scs == "static"_s))
      memop = {intreg::rip, 0, var->name};
    else {
      auto &pos = local_vars[insn.op];
      if(!pos) pos = sp -= insn.op->type->size;
      memop = {intreg::rbp, (int) pos};
    }
    op src = memop, dst = reg;
    if(!reload) std::swap(src, dst);

    if(!(tree::scalar_type) strip_type(insn.op->type)) *this << lea{"q"_s, {src, dst}};
    else *this << mov{get_type(insn.op->type), {src, dst}};
  };
  stmt(overload {
    [](auto &) {},
    [&](mov_t mov) {
      auto dst = gen(mov.dst);
      gen(mov.src, dst);
    },
    [&](compound_statement_t &compound) {
      for(auto stmt : compound) gen(stmt);
    },
    [&](spill_statement_t &spill) { genmem(spill); },
    [&](reload_t &reload) { genmem(reload); },
    [&](load_addr_t &load) {
      auto dst = gen(load.dst);
      visit(dst, overload {
        [&](auto &) {c9_assert(0); },
        [&](intreg reg) {  dst = memop{reg, (int) load.offset }; },
        [&](narrow<memop> auto &mem) { mem.offset += load.offset; }
      });
      *this << mov(get_type(load.src->type), {gen(load.src), dst});
    },
    [&](br_t br) {
      *this << test{get_type(br.cond->type), {gen(br.cond), gen(br.cond)}};
      *this << jcc{{&br.false_}};
    },
    [&](jump_t jump) {
      *this << jmp{&jump.target};
    },
    [&](return_statement_t &r) {
      gen(r.expr, intreg::rax);
      ret_insert_add_sp_pos.emplace_back(insns.size());
      *this << add{"q"_s, {0, intreg::rsp}};
      *this << pop{"q"_s, {intreg::rbp}} << ret{};
    }
  });
}
}}

