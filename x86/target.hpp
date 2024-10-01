#pragma once

#include "../target.hpp"
#include "isa.hpp"
#include "cfg.hpp"
#include "variant.hpp"

namespace c9 { namespace x86 {
static void dump_type(FILE *out, data_type type) { visit(type, [&](auto s) { fprint(out, "{}", s.c_str()); }); }
static void dump_op(FILE *out, op op) {
  visit(op, overload {
    [&]<class T>(T reg) requires std::same_as<T, intreg> || std::same_as<T, xmmreg> {
      visit(reg, overload {
        [&](size_t cnt) { fprint(out, "%{}", cnt); },
        [&](auto s)     { fprint(out, "%{}", s.c_str()); }
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
  std::vector<insn> insns;


  void gen(cfg::basic_block &entry);
  op gen(tree::op op);
  void gen(tree::expression, op dst);
  void gen(tree::statement);

  codegen &operator<<(auto value) { insns.emplace_back(value); return *this; }
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
