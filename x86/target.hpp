#pragma once

#include "../target.hpp"
#include "isa.hpp"
#include "cfg.hpp"
#include "sema.hpp"
#include "variant.hpp"
#include "regalloc.hpp"
#include "tree-opt.hpp"
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
               [&](auto) -> sv * { c9_assert(0); }
             })[idx]);
    },
    [&](xmmreg xmmreg) { fprint(out, "%xmm{}", size_t(xmmreg)); },
    [&](narrow<memop> auto op) {
      fprint(out, "{}{:+d}(", op.sym, op.offset);
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
    [&](jmp jmp) { fprintln(out, "jmp .bb_{}", jmp.target->i); },
    [&](jcc jcc) { visit(jcc.op, [&](auto s) { fprintln(out, "j{} .bb_{}", s.c_str(), jcc.target->i); }); },
    [&](call call) { fprint(out, "call ");  dump_op(out, call.target, "l"_s); fprintln(out, ""); },
    [&](ret &) { fprintln(out, "ret"); },
    [](auto) {

    }
  });
}

static data_type get_type(tree::type_decl type) {
  using namespace tree;
  return strip_type(type)(overload {
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
    [](float_type_t  &)             -> data_type { return "ss"_s; },
    [](double_type_t &)             -> data_type { return "sd"_s; },
    [](long_double_type_t &)        -> data_type { return "q"_s; },
    [](pointer_t &)                 -> data_type { return "q"_s; },
    [](array_t   &)                 -> data_type { return "q"_s; },
    [](function_type_t   &)         -> data_type { return "q"_s; },
    [](narrow<structural_decl_t> auto &)   -> data_type { return "q"_s; },
  });
}
static size_t size(data_type dt) {
  return visit(dt, overload {
    [](decltype("b"_s))  { return 1; },
    [](decltype("w"_s))  { return 2; },
    [](decltype("l"_s))  { return 4; },
    [](decltype("q"_s))  { return 8; },
    [](decltype("ss"_s)) { return 4; },
    [](decltype("sd"_s)) { return 8; },
  });
}
static data_type get_int_type(size_t size) {
  switch(size) {
    case 1: return "b"_s;
    case 2: return "w"_s;
    case 4: return "l"_s;
    case 8: return "q"_s;default: return "q"_s;
  }
}
static data_type get_xmm_type(size_t size) {
  switch(size) {
    case 4: return "ss"_s;
    case 8: return "sd"_s;
  }
}
struct function_codegen {
  cfg::control_flow_graph &cfg;
  std::vector<insn> insns;
  int sp{};

  flat_map<tree::op, int> local_vars;
  std::vector<std::pair<size_t, size_t>> label_list;
  std::vector<size_t> ret_insert_add_sp_pos;

  void gen(cfg::basic_block &entry);
  op gen(tree::op op);
  void gen(tree::expression, op dst);
  void gen(tree::statement);

  function_codegen &operator<<(auto value) { insns.emplace_back(value); return *this; }

  void dump(FILE *out);
};
static std::array int_call_conv_sysv = { intreg::rdi, intreg::rsi, intreg::rdx, intreg::rcx, intreg::r8,intreg::r9 };
constexpr static intreg int_ret_reg = intreg::rax;
static auto xmm_call_conv_sysv = [] {
  return make_seq(7_c)([&](auto ...c) { return std::array{xmmreg(c()) ...}; });
}();
constexpr static xmmreg xmm_ret_reg = xmmreg::xmm0;

class codegen {
  size_t nlabel = 1;
  std::set<tree::variable> section_data;
  std::vector<std::pair<opt<function_codegen>, tree::function>> section_text;
public:
  driver &d;
  sema::semantics &sema;
  void gen_var(tree::variable_t &var) {

  }
  void operator()(tree::decl decl) {
    decl(overload {
      [](auto &) {},
      [&](tree::variable_t &) {

      },
      [&](tree::function_t &fun) {
        std::vector<cfg::param> params(tree::function_type(fun.type)->params.size());
        cfg::control_flow_graph cfg{.d = d, .nlabel = nlabel,  .function = tree::function(decl), .params = params};
        cfg.construct(fun.definition);
        for(auto var : cfg.vars.map<tree::variable_t>()) section_data.emplace(var);
        c9::tree_opt::constprop(cfg);
        c9::tree_opt::cse(cfg);
        cfg.unssa();
        cfg.convert_to_two_address_code();
        cfg::cfg_walker walk{cfg.entry};
        bool int_ = bool((tree::integer_type) strip_type(fun.type));
      {
        regalloc::register_allocator alloc{cfg, x86::intreg{}, x86::op{}, x86::int_call_conv_sysv, x86::int_ret_reg};
        alloc.spill_ret = int_;
        alloc.spill_sofs = 16;
        alloc.tab[size_t(x86::intreg::rsp)].second = false;
        alloc.tab[size_t(x86::intreg::rbp)].second = false;
        alloc.tab[size_t(x86::intreg::rip)].second = false;
        alloc([](tree::type_decl type) { return (tree::integer_type) type || (tree::pointer) type; } );
      }
      {
        regalloc::register_allocator alloc{cfg, x86::xmmreg{}, x86::op{}, x86::xmm_call_conv_sysv, x86::xmm_ret_reg};
        alloc.spill_ret = !int_;
        alloc([](tree::type_decl type) { return (tree::floating_type) type; });
      }
        function_codegen codegen{cfg};
        codegen.gen(cfg.entry);
        section_text.emplace_back(fun.definition ? c9::mov(codegen) : opt<function_codegen>{}, tree::function(decl));
      }
    });
  }

  void print(FILE *out) {
    fprintln(out, ".section .data");
    for(auto var : section_data) if(var->is_global) fprintln(out, ".global {}", var->name);
    for(auto var : section_data) fprintln(out, "{}:\n\t.zero {}",  var->name, var->type->size);



    for(auto str : sema.string_tab) {
      fprint(out, "{}:\n\t.string \"", str.second->sym);
      str.second->print_to(out);
      fprintln(out, "\"");
    }

    fprintln(out, ".section .text");
    for(auto &[_, fun] : section_text)
      if(fun->scs != "static"_s) fprintln(out, ".global {}", fun->name);
    for(auto &[codegen, fun] : section_text)
      if(codegen) {
        fprintln(out, "{}:", fun->name);
        codegen->dump(stderr);
      }
  }

  codegen(driver &d, sema::semantics &sema) : d{d}, sema{sema} {}
};
}

static bool has_xmmnum(tree::type_decl t, size_t low, size_t high, size_t offset) {
  auto type = strip_type(t);
  bool m = true;
  if(auto s = (tree::structural_decl) type)
    s->definition->for_each([&](tree::record_member record_member) {
      if(!has_xmmnum(record_member->type, low, high, offset + record_member.offset))
        m = false;
    });
  else if(auto arr = (tree::array) type) {
    for(size_t i = 0; i != arr->size; i += arr->type->size)
      if(!has_xmmnum(arr->type, low, high, i))
        m = false;
  } else
    m = offset < low || offset >= high || (tree::float_type) type || (tree::double_type) type;
  return m;
}
static bool has_xmmnum_0(tree::type_decl ty) {
  return has_xmmnum(ty, 0, 8, 0);
}
static bool has_xmmnum_1(tree::type_decl ty) {
  return has_xmmnum(ty, 8, 16, 0);
}

struct x86_target : target {
  tree::pointer make_ptr(tree::type_decl type) override { return {{type, 8}};  }

  size_t mark_arg_regs(size_t regnum, bool xmm, tree::function_call fcall) override {
    size_t i = 0;
    auto f = [&](auto &&f, tree::type_decl type) -> void {
      type = strip_type(type);
      if(auto s = (tree::structural_decl) type; s && s->size <= 16) {
        bool xmm_0 = has_xmmnum_0(type), xmm_1 = has_xmmnum_1(type);
        if(i + xmm_0 + xmm_1 != regnum || (!xmm && i + !xmm_0 + !xmm_1 != regnum))
          i += xmm ? xmm_0 + xmm_1 : !xmm_0 + !xmm_1;
      } else if((!xmm || !(tree::floating_type) type))
        ++i;
    };
    for(auto arg : fcall->args) f(f, arg->type);

    return i;
  }
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
