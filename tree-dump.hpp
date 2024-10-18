#pragma once

#include "tree.hpp"

namespace c9 { namespace tree {
struct type_exp_format {
  template<class T> static void operator()(std::format_context& ctx, tree::tree_value<T> type) { type([&](auto &t) { operator()(ctx, t); }); }

  static void operator()(std::format_context& ctx, auto &t) {
    if constexpr(requires { t.name; }) std::format_to(ctx.out(), "{}", t.name);
    else c9_assert(0);
  }
  static void operator()(std::format_context& ctx, narrow<tree::builtin_type_t> auto &builtin) { if constexpr(requires { builtin.name; }) std::format_to(ctx.out(), "{}", builtin.name); }
  static void operator()(std::format_context& ctx, tree::type_name_t &type) {
    std::string str;
    if(type.is_const)
      str += "const ";
    if(type.is_volatile)
      str += "volatile ";
    if(type.is_restrict)
      str += "restrict ";
    std::format_to(ctx.out(), "{}", str);
    operator()(ctx, type.type);
  }
  static void operator()(std::format_context& ctx, tree::typedef_decl_t &type) {
    std::format_to(ctx.out(), "{} (", type.name);
    operator()(ctx, type.type);
    std::format_to(ctx.out(), ")", type.name);
  }
  static void operator()(std::format_context& ctx, tree::function_type_t &fn, size_t ptrs = 0) {
    operator()(ctx, fn.return_type);
    std::format_to(ctx.out(), "({:*>{}}) ", "", ptrs);
    std::format_to(ctx.out(), "(");
    for(auto param : fn.params | iter_range) {
      operator()(ctx,  param->type);
      if(param + 1 != fn.params.end()) std::format_to(ctx.out(), ", ");
    }
    std::format_to(ctx.out(), ")");
  }
  static void operator()(std::format_context& ctx, tree::array_t &arr, size_t ptrs = 0) {
    operator()(ctx, arr.type);
    if(ptrs) std::format_to(ctx.out(), "({:*>{}}) ", "", ptrs);
    std::format_to(ctx.out(), "[");
    // TODO int formatter
   // operator()(ctx, arr.numof);
    std::format_to(ctx.out(), "]");
  }
  static void operator()(std::format_context& ctx, tree::pointer_t &ptr) {
    auto exp = ptr.type; size_t ptrs = 1;
    while(auto p = (tree::pointer) exp) ++ptrs, exp = p->type;
    if(auto fn = (tree::function_type) exp)
      operator()(ctx, *fn, ptrs);
    else {
      operator()(ctx, exp);
      std::format_to(ctx.out(), " {:*>{}}", "", ptrs);
    }
  }
};

struct dumper {
  FILE *out;
  size_t ntab = 0;

  template<class ...T> void print(std::format_string<T...> fmt, T&& ...args) {
    fprint(out, "{: >{}}", "|-", ntab * 2);
    fprintln(out, fmt, (decltype(args)) args...);
  }
  template<class T> void operator()(tree::tree_value<T> tree) { ++ntab; tree([&](auto &t) { (*this)(t); }); --ntab; }
  void operator()(auto &tree) {
    sv fnname = __PRETTY_FUNCTION__;
    auto tree_name = [&] {
      auto start = fnname.find_first_of('=') + sizeof(" c9::tree::");
      auto end   = fnname.find_first_of(']') - 1;
      return sv{fnname.begin() + start, fnname.begin() + end};
    }();

    print("{}:", tree_name);
    if constexpr(requires { tree.fields; })
      for(auto field : tree.fields) (*this)(field);
    if constexpr(__is_same(decltype(tree), tree::compound_statement_t &))
      for(auto stmt : tree)  (*this)(stmt);
#define FIELD_NAME(name) if constexpr(requires { tree.name; }) ++ntab,print(#name ": {}", tree.name), --ntab;
    FIELD_NAME(name) FIELD_NAME(size)

#define FIELD(f) if constexpr(requires { tree.f; }) (*this)(tree.f);
    FIELD(expr) FIELD(cond)  FIELD(declref) FIELD(lhs) FIELD(rhs) FIELD(member)
    FIELD(value) FIELD(sym)  FIELD(type) FIELD(init) FIELD(if_stmt) FIELD(else_stmt) FIELD(stmt) FIELD(return_type)
    FIELD(cast_from) FIELD(cast_to) FIELD(definition)
#undef FIELD
  }

  template<char ...c> void operator()(string_seq<c...> str) { print("{}", str.c_str()); }
  //template<auto ...v> void operator()(variant_t<v...> variant) { visit(variant, [&](auto value) { (*this)(value); }); }
};

}}

template<c9::narrow<c9::tree::type_decl_t> T> struct std::formatter<c9::tree::tree_value<T>> {
  constexpr auto parse(auto &ctx) { return ctx.begin(); }
  auto format(c9::tree::tree_value<T> tree, std::format_context& ctx) const {
    c9::tree::type_exp_format{}(ctx, tree);
    return ctx.out();
  }
};
