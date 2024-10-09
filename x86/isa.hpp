#pragma once
#include "variant.hpp"

namespace c9 {
namespace cfg { struct basic_block; }
namespace x86 {
enum class intreg { rax, rcx, rdx, rbx, rsi, rdi, rbp, rsp, r8, r9, r10, r11, r12, r13, r14, r15, num_of };
enum class xmmreg { xmm0, num_of };
struct indirect_op { intreg base; int offset; };
using sym = string;
using op = variant<sym, intreg, xmmreg, int, indirect_op>;


using data_type = variant_t<"b"_s, "l"_s, "q"_s, "ss"_s, "sd"_s>;
template<size_t opn, auto name> struct alu_insn { data_type type; op ops[opn]; };
struct jmp { cfg::basic_block &target; };
struct jcc : jmp {
  bool is_unsigned{};
  variant_t<"ne"_s, "e"_s> op;
};
struct call { op target; };
struct ret {};

using add =  alu_insn<2, "add"_s>;
using sub =  alu_insn<2, "sub"_s>;
using mov =  alu_insn<2, "mov"_s>;
using test = alu_insn<2, "test"_s>;
using xor_  = alu_insn<2, "xor"_s>;
using neg =   alu_insn<1, "neg"_s>;
using not_ =  alu_insn<1, "not"_s>;
struct movsx { data_type src, dst; op ops[2]; };

using insn = variant<add, sub, mov, test, xor_, neg, not_, jmp, jcc, call, ret, movsx>;
}}
