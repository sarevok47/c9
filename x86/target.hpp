#pragma once

#include "../target.hpp"
#include "cfg.hpp"
#include "variant.hpp"

namespace c9 { namespace x86 {
using intreg = variant_t<size_t{}, "rax"_s, "rcx"_s, "rdx"_s, "rbx"_s, "rsi"_s, "rdi"_s, "rbp"_s, "rsp"_s>;
using xmmreg = variant_t<size_t{}, "xmm0"_s, "xmm1"_s, "xmm2"_s, "xmm3"_s, "xmm4"_s, "xmm5"_s, "xmm6"_s,
                            "xmm7"_s, "xmm8"_s, "xmm9"_s, "xmm10"_s, "xmm11"_s, "xmm12"_s, "xmm13"_s, "xmm14"_s, "xmm15"_s>;
struct direct_op   { intreg reg; };
struct indirect_op { intreg base; int scale, displacement; };

using op = variant<intreg, xmmreg, int, direct_op, indirect_op>;


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
static void dump_type(FILE *out, data_type type) { visit(type, [&](auto s) { fprint(out, "{}", s.c_str()); }); }
static void dump_op(FILE *out, op op) {
  visit(op, overload {
    [&]<class T>(T reg) requires std::same_as<T, intreg> || std::same_as<T, xmmreg> {
      visit(reg, overload {
        [&](size_t cnt) { fprint(out, "%{}", cnt); },
        [&](auto s)     { fprint(out, "{}", s.c_str()); }
      });
    },
    [&](int imm) { fprint(out, "{}", imm); },
    [](auto) {}
  });
}
static void dump_insn(FILE *out, insn insn) {
  fprint(out, "\t");
  visit(insn, overload {
    [&]<size_t n, auto s>(alu_insn<n, s> alu) {
      fprint(out, "{}", s.c_str());
      dump_type(out, alu.type);
      fprint(out, " ");
      for(size_t i = 0; i != n; ++i) {
        dump_op(out, alu.ops[i]);
        if(i + 1 != n) fprint(out, ", ");
      }
      fprintln(out, "");
    },
    [&](jmp jmp) { fprintln(out, "jmp bb_{}", jmp.target.i); },
    [&](jcc jcc) { visit(jcc.op, [&](auto s) { fprintln(out, "j{} bb_{}", s.c_str(), jcc.target.i); }); },
    [](auto) {

    }
  });
}

struct codegen {
  tree::ssa_variable *unssa_tab;
  size_t *tmps, *ssa_vars;

  std::vector<insn> insns;
private:
  tree::ssa_variable unssa(tree::ssa_variable ssa) {
    if(auto unssa = unssa_tab[ssa->ssa_tab_n])
      return unssa;
    return ssa;
  }

  size_t vintreg{}, vxmmreg{};

  flat_map<size_t, size_t> tmpvintreg, tmpvxmmreg;

  size_t alloc_virt_reg(auto op) {
    if(op.type.template is_narrow<tree::floating_type_t>()) return ++vxmmreg;
    else return ++vintreg;
  }


public:
  void gen(cfg::basic_block &entry);
  op gen(tree::op op);
  void gen(tree::expression, op dst);
  void gen(tree::statement);

  codegen &operator<<(auto value) { insns.emplace_back(value); return *this; }

  codegen(tree::ssa_variable *unssa_tab, size_t *tmps, size_t *ssa_vars) : unssa_tab{unssa_tab}, tmps{tmps}, ssa_vars{ssa_vars} {}
};



}


struct x86_target : target {
  x86_target() {
    ptrdiff_type_node = tree::long_type_node;
    size_type_node = tree::unsigned_long_type_node;
    predefined_macro = R""(
            #define __SIZEOF_DOUBLE__ 8
            #define __SIZEOF_FLOAT__ 4
            #define __SIZEOF_INT__ 4
            #define __SIZEOF_LONG_DOUBLE__ 8
            #define __SIZEOF_LONG_LONG__ 8
            #define __SIZEOF_LONG__ 8
            #define __SIZEOF_POINTER__ 8
            #define __SIZEOF_PTRDIFF_T__ 8
            #define __SIZEOF_SHORT__ 2
            #define __SIZEOF_SIZE_T__ 8
            #define __SIZE_TYPE__ unsigned long int
            #define __x86_64 1
            #define __x86_64__ 1
            #define __CHAR_BIT__ 8
            #define __amd64 1
            #define __amd64__ 1
    )"";
  }
};
}
