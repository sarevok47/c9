#pragma once
#include "variant.hpp"

namespace c9 {
namespace cfg { struct basic_block; }
namespace x86 {
using intreg = variant_t<"rax"_s, "rcx"_s, "rdx"_s, "rbx"_s, "rsi"_s, "rdi"_s, "rbp"_s, "rsp"_s>;
using xmmreg = variant_t<"xmm0"_s, "xmm1"_s, "xmm2"_s, "xmm3"_s, "xmm4"_s, "xmm5"_s, "xmm6"_s,
                         "xmm7"_s, "xmm8"_s, "xmm9"_s, "xmm10"_s, "xmm11"_s, "xmm12"_s, "xmm13"_s, "xmm14"_s, "xmm15"_s>;
struct indirect_op { intreg base; int offset; };

using op = variant<intreg, xmmreg, int, indirect_op>;


using data_type = variant_t<"b"_s, ""_s, "l"_s, "q"_s, "ss"_s, "sd"_s>;
template<size_t opn, auto name> struct alu_insn { data_type type; op ops[opn]; };
struct jmp { cfg::basic_block &target; };
struct jcc : jmp {
  bool is_unsigned{};
  variant_t<"ne"_s, "e"_s> op;
};

using add =  alu_insn<2, "add"_s>;
using sub =  alu_insn<2, "sub"_s>;
using mov =  alu_insn<2, "mov"_s>;
using test = alu_insn<2, "test"_s>;

using insn = variant<add, sub, mov, test, jmp, jcc>;
}}
/*
__ssa_phi = -8(%rsp)
__ssa_phi = 2
__ssa_phi = __ssa_x_phi + __ssa_phi
store __ssa_phi, -8(%rsp)
//TODO INSERT TMP COPY IF USE AND DST MEM IF DEF
*/
