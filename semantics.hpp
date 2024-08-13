#pragma once

#include "tree.hpp"
#include "tree-trait.hpp"
#include <flat-map.hpp>

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



struct scope : flat_map<string, node_t> { variant<compound_scope, fn_scope, control_scope, switch_scope> v; };
struct last_scopes {
  compound_scope *compound{};
  fn_scope       *function{};
  control_scope  *control {};
  switch_scope   *switch_ {};
};

class semantics {public:
  std::vector<scope> scopes;

public:

  scope &global_scope() { return scopes.front(); }
  id lookup(string name) {
    size_t scop = scopes.size() - 1;
    for(auto &scope : scopes | rv::reverse) {
      if(auto p = scope.find(name); p != scope.end())
        return { .name = name, .node = node_ref{ scop, &p->value } };

      --scop;
    }

    return { .name = name };
  }

  node_t &get_or_def_node(id id) {
    if(id.node && id.node->level == scopes.size())
      return *id.node->node;
    return scopes.back()[id.name];
  }


  auto &push_scope(auto v) requires requires(scope s) { s.v.indexof(v);  } {
    scopes.push_back({ .v = mov(v) });
    return (decltype(v) &) scopes.back().v;
  }
  compound_scope &push_scope() { return push_scope(compound_scope{}); }
  void pop_scope() {  scopes.pop_back(); }

  semantics() {
    scopes.emplace_back();
  }


};



}}
