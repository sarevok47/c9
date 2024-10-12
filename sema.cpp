#pragma once
#include "sema.hpp"
namespace c9 { namespace sema {

bool label_manager::process_label(tree::label label) {
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
void label_manager::lookup_label(string name, tree::goto_statement goto_) {
  if(auto p = labels.find(name); p == labels.end())
    unresolved_targets.push_back({name, goto_});
  else
    goto_->target = p->value;
}
tree::return_statement semantics::build_return_statement(source_range loc, tree::type_decl return_type, tree::expression expr) {
  tree::type_decl type, expr_type = tree::void_type_node;
  if(expr) expr_type = strip_type(expr->type);
  if(!(type = get_common_type(lex::assign_tok{"="_s}, loc, strip_type(return_type), expr_type)))
    return {};
  tree::return_statement_t ret{.expr =  tree::cast_expression{{ .cast_from = expr, .cast_to = strip_type(return_type)}}};
  ret.expr->type = type;
  return ret;
}


tree::decl semantics::build_typedef_decl(rich_location rl, id id, tree::decl &node, tree::type_name type) {
  if(node) {
    redecl_error(rl, id.name, node, "");
    return {};
  }
  return node = tree::typedef_decl{{.name = id.name, .type = type}};
}
tree::decl semantics::build_local_extern_decl(rich_location rl, id id, tree::decl &node, tree::type_name type) {
  return node(overload {
    [&](auto &decl) -> tree::decl requires requires { decl.scs; } {
      if(decl.type != type) {
        redecl_error(rl, id.name, node, "with different type");
        return node;
      }
      if(decl.scs == "extern"_s)
        return node;


      redecl_error(rl, id.name, node, "with different storage class specifier ('{}' and 'extern')", sv_variant(decl.scs));
      return node;
    },
    [&](tree::empty_node_t) {
      return node = build_decl(rl, {.name = id.name, .level = 0}, type, "extern"_s, true);
    },
    [&](auto &) -> tree::decl {
      redecl_error(rl, id.name, node, "");
      return {};
    }
  });
}

tree::decl semantics::build_decl(rich_location rl, id id, tree::type_name type, storage_class_spec scs, bool implicit) {
  if(!id.node || !id.node->decl)
    id.node = &scopes.stack[id.level]->operator[](id.name);
  auto &decl = id.node->decl;

  if(scs == "typedef"_s)
    return build_typedef_decl(rl, id, decl, type);

  auto funtype = (tree::function_type) type->type;
  if((scs == "extern"_s || funtype) && !id.is_global_scope())
    return build_local_extern_decl(rl, id, decl, type);

  tree::type_decl dtype;

  auto scs_assign = [&]<class T>(T &&to, auto from) -> __remove_cvref(T) {
    return visit(from, [&](auto from) {
      if constexpr(!requires { to = from;  })
        d.diag(rl, "error"_s, "function cannot take '{}' storage class", from.c_str());
        else
          to = from;
      return to;
    });
  };
  if(decl) {
    if(dtype = get_decl_type(decl); decl && (dtype.is<tree::typedef_decl_t>() || dtype != type->type)) {
      redecl_error(rl, id.name, decl, "");
      return {};
    }

    return decl([&](auto &tree) -> tree::decl {
      if constexpr(requires { tree.scs; }) {
        auto err = [&] {
          redecl_error(rl, id.name, decl, "with different storage class specifier ('{}' and '{}')", sv_variant(tree.scs), sv_variant(scs));
          return tree::decl{};
        };

        if(tree.scs == "extern"_s && (scs != ""_s && scs != "extern"_s))
          return err();
        if(scs == "extern"_s && (tree.scs != ""_s && tree.scs != "extern"_s))
          return err();
        if(scs == "extern"_s || tree.scs == "extern"_s)
          return decl;
        if(__is_same(decltype(tree), tree::function_t &)) {
          scs_assign(tree.scs, scs);
          id.node->decl_implicit = false;
          return decl;
        } else
          return err();
      }
      c9_assert(0);
    });
  }


  id.node->decl_implicit = implicit;
  if(funtype)
    decl = tree::function{{.name = id.name, .type = funtype,
      .scs = scs_assign(decltype(tree::function_t::scs){}, scs)
    }};
  else
    decl = tree::variable{{.name = id.name, .type = type->type,
      .is_global = id.is_global_scope(),
      .scs = scs_assign(decltype(tree::variable_t::scs){}, scs)
    }};
  return decl;
}

} }
