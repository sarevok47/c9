#pragma once

#include "tree.hpp"
#include "tree-trait.hpp"
#include <flat-map.hpp>
#include <stack>
namespace c9 { namespace sema {



class label_manager {
  flat_map<string, tree::label> labels;


  struct unresolved {
    string target_name;
    tree::goto_statement from;

    bool operator==(sv rhs) const { return target_name == rhs; }
  };
  std::vector<unresolved> unresolved_targets;
public:
  bool process_label(tree::label label) {
    string name = label->name;
    if(auto p = labels.find(name); p != labels.end())
      return false;
    else
      labels.push_back({name, label});

    unresolved_targets | rv::transform([&](unresolved &u) {
      if(u.target_name == name)
        u.from->target = label;
      return u;
    });
    return true;
  }
  void lookup_label(string name, tree::goto_statement goto_) {
    if(auto p = labels.find(name); p == labels.end())
      unresolved_targets.push_back({name, goto_});
    else
      goto_->target = p->value;
  }
};


struct compound_scope {};
struct fn_scope     { label_manager labels; };
struct control_scope {};
struct switch_scope { tree::switch_statement tree; };


struct scope  {  variant<compound_scope, fn_scope, control_scope, switch_scope> v;  };


template<class ...T> struct scope_manager {
  struct scope : /*flat_map*/std::unordered_map<string, node_t, string::hash> { variant<T...> v; };
  std::list<scope> stack;
private:
  std::tuple<std::stack<refw<T>>...> ctx_scope_stacks;
public:
  bool in_global() const { return stack.size() == 1; }

  template<class U> auto &ctx_scope_get() { return std::get<std::stack<refw<U>>>(ctx_scope_stacks); }
  template<class S = compound_scope> void push_scope(S s = {}) {
    stack.emplace_back(scope{ .v = mov(s)});
    ctx_scope_get<S>().push((S &) stack.back().v);
  }
  void pop_scope() {
    visit(stack.back().v, [&]<class S>(S &) {
      ctx_scope_get<S>().pop();
    });
    stack.pop_back();
  }
};

struct semantics {
  scope_manager<compound_scope, fn_scope, control_scope, switch_scope> scopes;

  auto &global_scope() { return scopes.stack.front(); }
  id name_lookup(string name) {
    auto &scopes = this->scopes.stack;
    size_t scop = scopes.size() - 1;
    for(auto &scope : scopes | rv::reverse) {
      if(auto p = scope.find(name); p != scope.end())
        return { .name = name, .node = node_ref{ scop, &p->/*value*/second } };

      --scop;
    }

    return { .name = name };
  }

  node_t &get_or_def_node(id id) {
    if(id.node && id.node->level == scopes.stack.size() - 1)
      return *id.node->node;
    return scopes.stack.back()[id.name];
  }


  semantics() { scopes.push_scope(compound_scope{});  }
};



}}
