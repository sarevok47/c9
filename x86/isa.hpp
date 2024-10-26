#pragma once
#include "variant.hpp"

namespace c9 {
namespace cfg { struct basic_block; }
namespace x86 {
enum class intreg { rax, rcx, rdx, rbx, rsi, rdi, rbp, rsp, r8, r9, r10, r11, r12, r13, r14, r15, rip, num_of };
enum class xmmreg { xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7, xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15, num_of };
using sym = string;
struct memop { intreg base; int offset; string sym; };
struct memop_index : memop { intreg index; };
struct memop_scale : memop_index { variant_t<1_c, 2_c, 4_c, 8_c> scale; };
using op = variant<sym, intreg, xmmreg, int, memop, memop_index, memop_scale>;


using data_type = variant_t<"b"_s, "w"_s, "l"_s, "q"_s, "ss"_s, "sd"_s>;
using opcode    = variant_t<"e"_s, "ne"_s, "le"_s, "l"_s, "g"_s, "ge"_s>;

template<size_t opn, auto name> struct alu_insn { data_type type; op ops[opn]; };
struct set {
  opcode op;
  intreg dst;
};
struct jmp { cfg::basic_block *target; };
struct jcc : jmp {
  bool is_unsigned{};
  opcode op;
};
struct call { op target; };
struct ret {};
using add =  alu_insn<2, "add"_s>;
using sub =  alu_insn<2, "sub"_s>;
using or_ =  alu_insn<2, "or"_s>;
using and_ = alu_insn<2, "and"_s>;
using xor_ = alu_insn<2, "xor"_s>;
using sar  = alu_insn<2, "sar"_s>;
using sal =  alu_insn<2, "sal"_s>;
using mov =  alu_insn<2, "mov"_s>;
using lea =  alu_insn<2, "lea"_s>;
using test = alu_insn<2, "test"_s>;
using cmp =  alu_insn<2, "cmp"_s>;
using xor_ = alu_insn<2, "xor"_s>;
using neg =  alu_insn<1, "neg"_s>;
using not_ = alu_insn<1, "not"_s>;
using push = alu_insn<1, "push"_s>;
using pop =  alu_insn<1, "pop"_s>;
struct movsx { data_type src, dst; op ops[2]; };

using insn = variant<add, sub, or_, and_, xor_, sar, sal, mov, lea, test, cmp, xor_, neg, not_, set, jmp, jcc, call, ret, push, pop, movsx>;
}}
