#pragma once

#include <vector>
#include <set>
#include <list>
#include "tree.hpp"
#include "tree-trait.hpp"
#include "simple.hpp"


namespace c9 { namespace tree { static bool operator<(tree::op lhs, tree::op rhs) { return lhs == rhs;  }} }
namespace c9 { namespace cfg {
template<class T> struct count_map {
  flat_map<T, size_t> storage;
public:
  struct iterator {
    decltype(storage.begin()) it{};

    iterator &operator++() { return ++it, *this; }
    const T &operator*() { return it->key; }

    bool operator==(iterator rhs) { return it == rhs.it; }

    iterator(auto it) : it{it} {}
  };

  iterator begin() { return storage.begin(); }
  iterator end()  { return storage.end(); }

  iterator find(T value) { return storage.find(value); }

  void insert(T value) { ++storage[value]; }
  void erase(T value) {
    auto p = storage.find(value);
    if(p != storage.end())
      if(!--p->value) storage.erase(p);
  }
};
class defusechain {
  std::tuple<
            count_map<tree::variable>,
            count_map<tree::ssa_variable>,
            count_map<tree::temporary>
            > storage;

  consteval static size_t get_idx(auto &) = delete;
  consteval static size_t get_idx(type_<tree::variable_t>)     { return 0; }
  consteval static size_t get_idx(type_<tree::ssa_variable_t>) { return 1; }
  consteval static size_t get_idx(type_<tree::temporary_t>)    { return 2; }

  void visit(tree::op op, auto &&f) {
    return op(overload {
      [&](auto &) { c9_assert(0); },
      [&]<class T>(T &value) requires requires { get_idx(type_c<T>); } { f(value, std::get<get_idx(type_c<T>)>(storage)); }
    });
  }
public:
  template<class T> auto &map() { return std::get<get_idx(type_c<T>)>(storage); }
  void insert(tree::op op) { visit(op, [&]<class T>(T &&, auto &map) { map.insert(tree::tree_value<T>(op)); }); }
  void erase(tree::op op) {  visit(op, [&]<class T>(T &&, auto &map) { map.erase(tree::tree_value<T>(op)); }); }
  tree::op find(tree::op op) {
    tree::op r;
    visit(op, [&](auto &&value, auto &map) {
      auto p = map.find(value);
      if(p != map.end()) r = *p;
    });
    return r;
  }
  void visit_each(auto &&f) {
    f(std::get<0>(storage));
    f(std::get<1>(storage));
    f(std::get<2>(storage));
  }
};
void tree_dump(FILE *out, tree::statement tree);
struct basic_block {
  class control_flow_graph &cfg;
  size_t i{};
  std::list<tree::statement> insns;
size_t insn_pos{};
  std::set<basic_block *> preds, succs;
  flat_map<tree::variable, std::pair<tree::phi, tree::ssa_variable>> phis;
  defusechain def, use;

  basic_block *dominator{};

  basic_block &push(basic_block bb) { return *(next = new auto{mov(bb)}); }
  void add_insn(tree::statement insn);
  tree::op add_assign(tree::expression insn, tree::op dst);
  void add_pred(basic_block *bb);

  basic_block *step() { return next; }

  basic_block &jump(basic_block &target) {
    add_insn(tree::jump{{.target = target}});
    return *this;
  }
  basic_block &br(tree::op cond, basic_block &true_bb, basic_block &false_bb) {
    add_insn(tree::br{{.cond = cond, .true_ = true_bb, .false_ = false_bb}});
    return *this;
  }

  void dump(FILE *out);

  void search_def_for_phi(tree::variable var, flat_set<tree::op> &out, basic_block *stop);
  void place_phi(tree::ssa_variable);

  void visit_dominators(auto &&f) {
    for(auto bb = this; bb = bb->dominator; )
      f(*bb);
  }

  basic_block(control_flow_graph &cfg) : cfg{cfg} {}
  basic_block(control_flow_graph &cfg, size_t i, std::same_as<basic_block *> auto ...preds) : cfg{cfg}, i{i}, preds{preds...} {}
  basic_block(control_flow_graph &cfg, size_t i, size_t insn_pos, std::same_as<basic_block *> auto ...preds) : cfg{cfg}, i{i}, insn_pos{insn_pos}, preds{preds...} {}
private:
  basic_block *next{};
};

struct cfg_walker {
  basic_block &entry;

  std::set<basic_block *> bb_stack;


  void operator()(auto &&f, basic_block &bb) {
    size_t sz = bb_stack.size();
    if(bb_stack.emplace(&bb), sz == bb_stack.size())
      return;
    f(bb);
    for(auto succ : bb.succs) (*this)(f, *succ);
  }
  void operator()(auto &&f) { (*this)(f, entry); }
};
struct param { tree::op op; tree::target_op reg; };
class control_flow_graph {public:
  driver &d;
  size_t &nlabel, ntmp{}, nssa{};size_t insn_count{};
  basic_block entry{*this}, *last_bb = &entry;
  tree::op last_op;

  defusechain vars;
  tree::function function;

  std::vector<param> &params;

  tree::op make_tmp(tree::type_decl type) {
    tree::temporary_t tmp{.idx = ntmp++};
    tmp.type = type;
    return tmp;
  }

  basic_block &add_bb(auto ...preds) {
    last_bb = &last_bb->push({*this, nlabel++, insn_count, preds...});
    ((preds->succs.emplace(last_bb)), ...);

    if constexpr(sizeof...(preds) == 1)
      last_bb->dominator = (preds, ...);
    else
      last_bb->dominator = 0_c(preds...)->dominator;
    return *last_bb;
  }

  friend struct cfg_stream;
  struct cfg_stream cfg();

  tree::op construct_var(tree::variable var);
  tree::expression construct_expr_no_op(tree::expression expr);
  tree::op construct(tree::expression expr);
  void construct(tree::statement stmt);

  void collect_phi_operands(tree::ssa_variable tab[]);
  void for_each_insn(auto &&f) {
    for(auto bb = &entry; bb; bb = bb->step())
      for(auto &insn : bb->insns) f(insn, *bb);
  }
  void unssa();
  void convert_to_two_address_code();

  void dump();
};
void visit_ops(auto &insn, auto &&f) {
  insn(prioritizied_overload (
    [&](narrow<tree::op_t> auto &) { f(insn); },
    [&](tree::function_call_t &fcall) { for(auto &arg : fcall.args) visit_ops(arg, f); },
    [&](narrow<tree::expression_t> auto &expr) {
      if constexpr(requires { expr.expr;}) visit_ops(expr.expr, f);
      if constexpr(requires { expr.cast_from; }) visit_ops(expr.cast_from, f);
      if constexpr(requires { expr.lhs; }) visit_ops(expr.lhs,  f);
      if constexpr(requires { expr.rhs; }) visit_ops(expr.rhs,  f);
    },
    [&](tree::return_statement_t &ret) {
      visit_ops(ret.expr, f);
    },
    [&](auto &mov) requires requires { mov.src; mov.dst; } {
      visit_ops(mov.src, f);
      visit_ops(mov.dst, f);
    },
    [&](tree::compound_statement_t &compound) { for(auto &stmt : compound) visit_ops(stmt, f); },
    [&](tree::br_t &br) { visit_ops(br.cond, f); },
    [](auto &) {}
  ));
}
}}
