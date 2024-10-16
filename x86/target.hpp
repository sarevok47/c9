#pragma once

#include "../target.hpp"
#include "isa.hpp"
#include "cfg.hpp"
#include "variant.hpp"
#include "regalloc.hpp"

namespace c9 {
namespace x86 {
static void dump_type(FILE *out, data_type type) { visit(type, [&](auto s) { fprint(out, "{}", s.c_str()); }); }
static void dump_op(FILE *out, op op, data_type type) {
  visit(op, overload {
    [&](sym &sym) { fprint(out, "{}", sym); },
    [&](intreg intreg) {
      sv _8[] = {
        "al", "cl", "dl", "bl", "ah", "ch", "dh", "bh",
        "r8b", "r9b", "r10b", "r11b", "r12b", "r13b", "r14b", "r15b"
      };
      sv _16[] = {
        "ax", "cx", "dx", "bx", "si", "di", "bp", "sp",
        "r8w", "r9w", "r10w", "r11w", "r12w", "r13w", "r14w", "r15w", "ip"
      };
      sv _64[] = {
        "rax", "rcx","rdx", "rbx", "rsi", "rdi", "rbp", "rsp", "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", "rip"
      };
      sv _32[] = {
        "eax", "ecx", "edx", "ebx", "esi", "edi", "ebp", "esp", "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d", "eip"
      };
      size_t idx = size_t(intreg);
      fprint(out, "%{}",
             visit(type, overload {
               [&](decltype("b"_s)) { return _8; },
               [&](decltype("w"_s)) { return _16; },
               [&](decltype("l"_s)) { return _32; },
               [&](decltype("q"_s)) { return _64; },
             })[idx]);
    },
    [&](xmmreg xmmreg) { fprint(out, "%xmm_{}", size_t(xmmreg)); },
    [&](narrow<memop> auto op) {
      visit(op.offset, [&](auto offset) { fprint(out, "{}(", offset); });
      dump_op(out, op.base, "q"_s);
      if constexpr(requires { op.index; }) {
        fprint(out, ", ");
        dump_op(out, op.index, "q"_s);
        if constexpr(requires { op.scale; })
          visit(op.scale, [&](auto c) { fprint(out, ", {}", c()); });
      }

      fprint(out, ")");
    },
    [&](int imm) { fprint(out, "${}", imm); },
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
        dump_op(out, alu.ops[i], alu.type);
        if(i + 1 != n) fprint(out, ", ");
      }
      fprintln(out, "");
    },
    [&](movsx movsx) {
      fprint(out, "movs");
      dump_type(out, movsx.src);
      dump_type(out, movsx.dst);
      fprint(out, " ");
      dump_op(out, movsx.ops[0], movsx.src);
      fprint(out, ", ");
      dump_op(out, movsx.ops[1], movsx.dst);
      fprintln(out, "");
    },
    [&](jmp jmp) { fprintln(out, "jmp .bb_{}", jmp.target.i); },
    [&](jcc jcc) { visit(jcc.op, [&](auto s) { fprintln(out, "j{} .bb_{}", s.c_str(), jcc.target.i); }); },
    [&](call call) { fprint(out, "call ");  dump_op(out, call.target, "l"_s); fprintln(out, ""); },
    [&](ret &) { fprintln(out, "ret"); },
    [](auto) {

    }
  });
}

static data_type get_type(tree::type_decl type) {
  using namespace tree;
  return type(overload {
    [](auto &)                      -> data_type { c9_assert(0); },
    [](signed_char_type_t &)        -> data_type { return "b"_s; },
    [](char_type_t &)               -> data_type { return "b"_s; },
    [](unsigned_char_type_t &)      -> data_type { return "b"_s; },
    [](short_type_t &)              -> data_type { return "w"_s; },
    [](unsigned_short_type_t &)     -> data_type { return "w"_s; },
    [](int_type_t &)                -> data_type { return "l"_s; },
    [](unsigned_int_type_t &)       -> data_type { return "l"_s; },
    [](long_type_t &)               -> data_type { return "q"_s; },
    [](unsigned_long_type_t &)      -> data_type { return "q"_s; },
    [](long_long_type_t &)          -> data_type { return "q"_s; },
    [](unsigned_long_long_type_t &) -> data_type { return "q"_s; },
    [](float_type_t  &)             -> data_type { return "l"_s; },
    [](double_type_t &)             -> data_type { return "q"_s; },
    [](long_double_type_t &)        -> data_type { return "q"_s; },
    [](pointer_t &)                 -> data_type { return "q"_s; },
  });
}
static size_t size(data_type dt) {
  return visit(dt, overload {
    [](decltype("b"_s)) { return 1; },
    [](decltype("w"_s)) { return 2; },
    [](decltype("l"_s)) { return 4; },
    [](decltype("q"_s)) { return 8; },
  });
}
struct codegen {
  std::vector<insn> insns;
  int sp{};

  flat_map<tree::op, size_t> local_vars;
  std::vector<std::pair<size_t, size_t>> label_list;
  std::vector<int *> ret_insert_add_sp_pos;

  void gen(cfg::basic_block &entry);
  op gen(tree::op op);
  void gen(tree::expression, op dst);
  void gen(tree::statement);

  codegen &operator<<(auto value) { insns.emplace_back(value); return *this; }

  void dump(FILE *out);
};
static intreg int_call_conv_sysv[] = { intreg::rdi, intreg::rsi, intreg::rdx, intreg::rcx, intreg::r8,intreg::r9 };
constexpr static intreg int_ret_reg = intreg::rax;
}



struct x86_target : target {

  tree::pointer make_ptr(tree::type_decl type) override { return {{type, 8}};  }
  x86_target()  {
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
