#pragma once

#include "driver.hpp"
#include <unordered_map>
#include "tree.hpp"
#include "token.hpp"
#include "tree-trait.hpp"
#include "flat-map.hpp"
#include "target.hpp"
#include <stack>

namespace c9 { namespace sema {
using storage_class_spec = variant_t<""_s, "typedef"_s, "extern"_s, "static"_s, "auto"_s, "register"_s>;
class label_manager {
  flat_map<string, tree::label> labels;

  struct unresolved {
    string target_name;
    tree::goto_statement from;

    bool operator==(sv rhs) const { return target_name == rhs; }
  };
  std::vector<unresolved> unresolved_targets;
public:
  bool process_label(tree::label label);
  void lookup_label(string name, tree::goto_statement goto_);
};


struct compound_scope {};
struct fn_scope     { tree::function_type type; label_manager labels; };
struct control_scope {};
struct switch_scope { tree::switch_statement tree; };


struct scope  {  variant<compound_scope, fn_scope, control_scope, switch_scope> v;  };


template<class ...T> struct scope_manager {
  struct scope : /*flat_map*/std::unordered_map<string, node_t, string::hash> {
    variant<T...> v;
    scope(variant<T...> v) : v{mov(v)} {}
  };
  std::vector<std::unique_ptr<scope>> stack;
private:
  std::tuple<std::stack<refw<T>>...> ctx_scope_stacks;
public:
  bool in_global() const { return stack.size() == 1; }

  template<class U> auto &ctx_scope_get() { return std::get<std::stack<refw<U>>>(ctx_scope_stacks); }
  template<class S = compound_scope> void push_scope(S s = {}) {
    stack.emplace_back(std::make_unique<scope>(mov(s)));
    ctx_scope_get<S>().push((S &) stack.back()->v);
    if constexpr(__is_same(S, fn_scope))
      for(auto &dector : s.type->params)
        stack.back()->operator[](dector.name).decl = tree::variable{{
          .name = dector.name, .type = dector.type
        }};
  }
  void pop_scope() {
    visit(stack.back()->v, [&]<class S>(S &) {
      ctx_scope_get<S>().pop();
    });
    stack.pop_back();
  }
};



struct semantics {
  driver &d;
  scope_manager<compound_scope, fn_scope, control_scope, switch_scope> scopes;



  auto &global_scope() { return scopes.stack.front(); }
  id name_lookup(string name) {
    auto &scopes = this->scopes.stack;
    size_t scop = scopes.size() - 1;
    for(auto &scope : scopes | rv::reverse) {
      if(auto p = scope->find(name); p != scope->end())
        return {  name, scop, &p->/*value*/second };

      --scop;
    }

    return { name, scopes.size() - 1 };
  }

  node_t &get_or_def_node(id id) {
    if(id.node && id.level == scopes.stack.size() - 1)
      return *id.node;
    return (*scopes.stack.back())[id.name];
  }

  tree::subscript_expression build_subscript_expression(source_range loc, tree::expression of, tree::expression with);
  tree::function_call build_function_call(source_range loc, tree::expression calee, std::vector<tree::expression> & args);
  tree::decl_expression build_decl_expression(location_t ref_loc, tree::decl decl);

  tree::expression maybe_build_cast_expression(source_range loc, tree::type_decl to, tree::expression from);

  tree::statement_expression build_statement_expression(source_range loc, tree::compound_statement stmts);
  tree::type_decl get_common_type(variant<lex::binary_tok, lex::assign_tok> op, source_range loc, tree::type_decl lhstype, tree::type_decl rhstype);


  tree::binary_expression build_binary_expression(lex::binary_tok op, tree::expression lhs, tree::expression rhs);
  tree::assign_expression build_assign_expression(lex::assign_tok op, tree::expression lhs, tree::expression rhs);
  tree::ternary_expression build_ternary_expression(tree::expression cond, tree::expression lhs, tree::expression rhs);

  tree::expression build_unary_expression(source_range loc, lex::token op, tree::expression expr);

  tree::return_statement build_return_statement(source_range loc, tree::type_decl return_type, tree::expression expr);

  template<class T> T build_access_member_expression(tree::expression expr, string name) {
    auto type = strip_type(expr->type);

    if(__is_same(T, tree::pointer_access_member)) {
      if(!type.is<tree::pointer_t>()) {
        d.diag(expr->loc, "error"_s, "left operand of '->' must be a pointer");
        return {};
      }
      type = tree::pointer(type)->type;
    }

    if(!type.is_narrow<tree::structural_decl_t>()) {
      d.diag(expr->loc, "error"_s, "cannot access member in not structural types");
      return {};
    }
    if(is_incomplete_type(type)) {
      d.diag(expr->loc, "error"_s, "cannot access incomplete type");
      return {};
    }

    if(tree::variable field = tree::structural_decl(type)->definition->find(name))
      return {{.expr = expr, .member = field}};

    d.diag(expr->loc, "error"_s, "no matching '{}' member", name);
    return {};
  }


  template<class ...T> void redecl_error(rich_location rl, string name, tree::decl &decl, std::format_string<T...> fmt, T&& ...args) {
    d.diag(rl, "error"_s, "redeclaration of {} '{}' {}",
           decl(overload {
             [](tree::function_t &)     { return "function"; },
             [](tree::typedef_decl_t &) { return "typedef";  },
             [](auto &)                 { return "variable"; }
           }),
           name, std::format(fmt, (decltype(args)) args...));
  }

  tree::decl build_typedef_decl(rich_location rl, id id, tree::decl &node, tree::type_name type);
  tree::decl build_local_extern_decl(rich_location rl, id id, tree::decl &node, tree::type_name type);
  tree::decl build_decl(rich_location rl, id id, tree::type_name type, storage_class_spec scs, bool implicit = false);

  semantics(driver &d) : d{d} { scopes.push_scope(compound_scope{});  }
};

}}
