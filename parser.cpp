#pragma once

#include "parse.hpp"
#include "tree-trait.hpp"
#include "semantics.hpp"

namespace c9 { namespace parse {
sema::id lex_spirit::lookup(lex::identifier s) { return parse.name_lookup(s); }
bool parser::starts_typename(lex::token tok) {
  return visit(tok, overload {
    [](auto &) { return false; },
               [&](sema::id &id) {
                 if(!id.node || !id.node->decl) id = name_lookup(id.name);
                 return id.node && id.node->decl.is<tree::typedef_decl_t>();
               },
               [](keyword kw) {
                 switch(kw) {
                   case keyword::void_:
                   case keyword::int_:
                   case keyword::char_:
                   case keyword::long_:
                   case keyword::short_:
                   case keyword::typeof_:
                   case keyword::__auto_type_:
                   case keyword::__attribute___:
                   case keyword::register_:
                   case keyword::const_:
                   case keyword::volatile_:
                   case keyword::float_:
                   case keyword::double_:
                     return true;
                   default:
                     return false;
                 }
               }
  });
}

tree::expression parser::primary_expression() {
  location_t loc = peek_token().loc;
  return visit(peek_token(), overload {
    [&](sema::id &id) -> tree::expression {
      consume();
      if(!id.node || !id.node->decl)
        id = name_lookup(id.name);
      if(id.node->decl)
        return build_decl_expression(loc, id.node->decl);

      error(loc, {}, "use undeclared '{}'", id.name);
      return {};
    },
    [&](lex::numeric_constant nc) {
      consume();
      lex::interpret_status stat;
      auto v = lex::interpret_nc(nc, stat);
      return visit(v, overload {
        [&](lex::integer int_) -> tree::expression {
          uint64_t value;
          if(stat == lex::interpret_status::out_of_range)
            error(loc, {}, "integer literal is too large to be represented in any integer type");
          else
            value = int_.value;

          return tree::int_cst_expression{{value,tree::int_type_node, loc}};
        },
        [&](lex::floating float_) -> tree::expression {
          tree::floating_type type;
          visit(float_.suffix, overload {
            [&](decltype("f"_s)) { type = tree::float_type_node; },
                [&](decltype(""_s))  { type = tree::double_type_node; },
                [&](decltype("l"_s)) { type = tree::double_type_node; },
                [&](decltype("L"_s)) { type = tree::long_double_type_node; },
          });
          return tree::float_cst_expression{{ float_.value, type, loc }};
        }
      });
    },
    [&](lex::char_literal cl) -> tree::expression {
      consume();
      variant_t<""_s, "U"_s, "u"_s, "u8"_s, "L"_s, "L"_s> prefix;

      auto s = cl.begin();
      lex::scan_impl(s, prefix, variant_types(prefix), 0_c, ""_s);
      ++s;

      return tree::int_cst_expression{{*s, tree::char_type_node, loc}};
    },
    [&](decltype("("_s)) -> tree::expression {
      consume();
      // statement expression
      if(*this <= "{"_s) {
        tree::compound_statement stmts;
        *this <= &parser::compound_statement % stmts >> ")"_req;
        // TODO Loc
        return build_statement_expression(loc, stmts);
      }
      auto expr = expression();
      if(expr) {
        *this <= ")"_req;
        return expr;
      }
      return {};
    },
    [&]<class T>(T &v) -> tree::expression {
      location_t loc = peek_token().loc;
      consume();
      if constexpr(__is_same(T, keyword))
        switch(v) {
          case keyword::sizeof_: {
            tree::sizeof_expression_t s;
            if(peek_token() == "("_s && starts_typename(peek_2nd_token())) {
              consume();
              s.arg = tree::type_decl(type_name());
              *this <= ")"_req;
            } else
              s.arg = unary_expression();
            return s;
          }
          default: break;
        }
        error(loc, {}, "primary expected");
        return {};
    }
  });
}
tree::expression parser::postfix_expression() {
  tree::expression primary = primary_expression();
  location_t loc = peek_token().loc;
  return visit(peek_token(), overload {
    [&](auto &) { return primary; },
               [&](decltype("["_s)) -> tree::expression {
                 consume();
                 tree::expression with = expression();
                 *this <= "]"_req;
                 return build_subscript_expression({loc, peek_token().loc}, primary, with);
               },
               [&](decltype("("_s)) -> tree::expression {
                 consume();
                 std::vector<tree::expression> args;
                 if(peek_token() != ")"_s)
                   do args.emplace_back(assignment_expression()); while(*this <= ","_s);
                   *this <= ")"_req;
                 return build_function_call({loc, peek_token().loc}, primary, mov(args));
               },
               [&](decltype("."_s)) -> tree::expression {
                 consume();
                 auto tok = peek_token();
                 if(require(type_c<sema::id>))
                   return build_access_member_expression<tree::access_member>(primary, sema::id(tok).name);

                 return {};
               },
               [&](decltype("->"_s)) -> tree::expression {
                 consume();
                 auto tok = peek_token();
                 if(require(type_c<sema::id>))
                   return build_access_member_expression<tree::pointer_access_member>(primary, sema::id(tok).name);
                 return {};
               },
               [&]<char ...c>(string_seq<c...> s) -> tree::expression requires (lex::is_crement(s)) {
                 consume();
                 return tree::postcrement_expression{{.op = s, .expr = primary}};
               }
  });
}
tree::expression parser::unary_expression() {
  return visit(peek_token(), overload {
    [&]<char ...c>(string_seq<c...> s) -> tree::expression
    requires (lex::unary_puncs(contains(s)) || lex::is_crement(s) || s == "*"_s || s == "&"_s) {
      location_t loc = peek_token().loc;
      consume();
      auto expr = unary_expression();
      return build_unary_expression(source_range(loc) + expr->loc, s, expr);
    },
    [&](auto &) { return postfix_expression(); }
  });
}
tree::expression parser::cast_expression() {
  if(peek_token() == "("_s && starts_typename(peek_2nd_token())) {
    consume();
    auto type = type_name();
    *this <= ")"_req;
    if(tree::initializer_list init; *this <= "{"_s >> &parser::initializer_list % init)
      return tree::compound_literal{{.typec = type, .init = init}};

      return tree::cast_expression{{ .cast_from = cast_expression(),  .cast_to = type, }};
  }
  return unary_expression();
}

/* https://github.com/gcc-mirror/gcc/blob/f80db5495d5f8455b3003951727eb6c8dc67d81d/gcc/c/c-parser.cc#L9372
 * Pretty beautiful stack based binary expression parser */
tree::expression parser::binary_expression() {
  struct {
    tree::expression expr;
    lex::binary_tok op;
    lex::binary_prec prec{};
  } stack[lex::num_of + 1];

  size_t sp{};
  auto pop = [&]{
    stack[sp - 1].expr = build_binary_expression(stack[sp].op, stack[sp - 1].expr, stack[sp].expr);
    --sp;
  };
  stack[0].expr = cast_expression();
  stack[0].prec = lex::binary_prec::none;

  while(visit(peek_token(), overload {
    [](auto &&) { return false; },
              [&]<char ...c>(string_seq<c...> s) requires (lex::is_binop(s)) {
                size_t prec;
                lex::binary_tok bt;
                lex::binary_op_tab(contains_if([&](auto x) {
                  bool r = x.key == s;
                  if(r) { bt = x.key; prec = x.prec; }
                  return r;
                }));

                while (prec <= stack[sp].prec)
                  pop();

                consume();
                ++sp;
                stack[sp].expr = cast_expression();
                stack[sp].prec = (lex::binary_prec) prec;
                stack[sp].op = bt;
                return true;
              }
  }));

  while (sp > 0)
    pop();
  return stack[0].expr;
}

tree::expression parser::conditional_expression() {
  tree::expression cond, lhs, rhs;
  *this <= &parser::binary_expression % cond
  >> -("?"_s >> &parser::expression % lhs
  >> ":"_req >> &parser::conditional_expression % rhs
  );

  if(lhs)
    return build_ternary_expression(cond, lhs, rhs);
  return cond;
}
tree::expression parser::assignment_expression() {
  auto lhs = conditional_expression();
  visit(peek_token(), overload {
    [](auto &&) {},
        [&]<char ...c>(string_seq<c...> s) requires (lex::is_assign(s)) {
          consume();
          lhs = build_assign_expression(s, lhs, assignment_expression());
        }
  });
  return lhs;
}
tree::expression parser::expression() {
  auto tree = assignment_expression();
  while(*this <= ","_s)
    tree = [&]{
      tree::comma_expression_t expr{.lhs = tree, .rhs = assignment_expression()};
      expr.loc = expr.lhs->loc +expr. rhs->loc;
      expr.type = expr.rhs->type;
      return expr;
    }();
  return tree;
}

void parser::process_type_spec(type_spec_state tss, tree::type_decl &type) {
  if(tss.long_ && tss.short_)
    error(tss.long_.loc, {tss.short_.loc}, "long and short specifiers cannot be used both");
  else if(tss.short_)
    type(overload {
      [&](tree::char_type_t &) {},
         [&](tree::int_type_t &)   { type = tree::short_type_node;  },
         [&](tree::empty_node_t &)   { type = tree::short_type_node;  },
         [&](auto &) { error(tss.short_.loc, {}, "short specifier must be used with integer type");  }
    });
  else if(tss.long_)
    type(overload {
      [&](tree::int_type_t &) {
        switch(tss.long_.times) {
          case 1: type = tree::long_type_node; break;
          case 2: type = tree::long_long_type_node; break;
        }
      },
      [&](tree::empty_node_t &) {
        switch(tss.long_.times) {
          case 1: type = tree::long_type_node; break;
          case 2: type = tree::long_long_type_node; break;
        }
      },
      [&](tree::double_type_t &) { type = tree::long_double_type_node; },
         [&](auto &) { error(tss.short_.loc, {}, "long specifier must be used with 'int' or 'double' specifier");  }
    });


    if(tss.unsigned_ && tss.signed_)
      error(tss.signed_.loc, {tss.unsigned_.loc}, "unsigned and signed specifiers cannot be used both");
  else if(tss.unsigned_)
    type(overload {
      [&](tree::empty_node_t &) { type = tree::unsigned_int_type_node; },
         [&](tree::char_type_t &) {},
         [&](narrow<tree::signed_integral_type_t> auto &tree) { type = make_unsigned(tree); },
         [&](auto &) { error(tss.unsigned_.loc, {}, "unsigned must be used with integer type");  }
    });
  else if(tss.signed_)
    type(overload {
      [&](tree::empty_node_t &) { type = tree::int_type_node; },
         [&](tree::char_type_t &) { type = tree::signed_char_type_node; },
         [&](narrow<tree::signed_integral_type_t> auto &tree) {},
         [&](auto &) { error(tss.signed_.loc, {}, "signed must be used with integer type");  }
    });
}


bool parser::type_qualifer(tree::type_name &type) {
  return *this <= ((keyword::const_,     [&] { type->is_const = true;    })
  | (keyword::volatile_, [&] { type->is_volatile = true; })
  | (keyword::restrict_ | keyword::__restrict_, [&] { type->is_restrict = true; }));
}

bool parser::storage_class_specifier(storage_class_spec &scs) {
  return *this <= ((keyword::typedef_,   [&] { scs = "typedef"_s;  })
  | (keyword::extern_,   [&] { scs = "extern"_s;   })
  | (keyword::auto_,     [&] { scs = "auto"_s;     })
  | (keyword::static_,   [&] { scs = "static"_s;   })
  | (keyword::register_, [&] { scs = "register"_s; }));
}

bool parser::struct_or_union_specifier(tree::type_decl &td) {
  bool is_struct;

  location_t start_loc = peek_token().loc;
  if(!(*this <= ((keyword::struct_, [&] { is_struct = true; })
    | (keyword::union_, [&] { is_struct = false;  }))))
    return false;

    sema::id name;
    if(is<sema::id>(peek_token())) {
      name = peek_token();
      start_loc = peek_token().loc;
      consume();
    }

    auto &node = get_or_def_node(name);;
    if(is_struct) {
      if(!node.struct_decl) node.struct_decl = tree::struct_decl_t{};
      td = node.struct_decl;
    }
    else {
      if(!node.union_decl) node.union_decl = tree::union_decl_t{};
      td = node.union_decl;
    }
    if(*this <= "{"_s) {
      tree::record_decl_t s{};
      while(peek_token() && peek_token() != "}"_s) {
        decl_specifier_seq dss;
        declspec(dss);

        if(dss.storage_class != ""_s)
          error(dss.storage_class_loc, {}, "storage class specifier in structural field");

        do {
          sema::id id;
          std::vector<tree::attribute> attrs;
          auto type = declarator(id, dss.type, attrs);

          type->type(overload {
            [&](tree::function_type_t &) {
              error(peek_token().loc, {}, "cannot declarate function within structure");
            },
            [&](auto &) {
              s.fields.push_back({{ .name = id.name, .type = type, .attrs = mov(attrs) }});
            }
          });

          if(id.name.empty()) break;
        } while(*this <= ","_s);

          if(!require(";"_s))
            break;
      }
      *this <= "}"_req;
      if(name.name.size()
        && ((is_struct && node.struct_decl->definition) || (!is_struct && node.union_decl->definition))
      )
        error(start_loc, {}, "redeclaration of {} '{}'", is_struct ? "struct" : "union", name.name);
        else {
          if(is_struct) node.struct_decl->definition = s;
          else          node.union_decl->definition = s;
        }
    }

    return true;
}

bool parser::typedef_spec(tree::type_name &type) {
  if(is<sema::id>(peek_token())) {
    sema::id &id = peek_token();
    if(!id.node || !id.node->decl) id = name_lookup(id.name);
    if(id.node && id.node->decl.is<tree::typedef_decl_t>()) {
      type = ((tree::typedef_decl_t &) id.node->decl).type;
      consume();
      return true;
    }
  }
  return false;
}

bool parser::type_specifier(tree::type_name &type, type_spec_state &tcs) {
  location_t loc = peek_token().loc;
  return *this <= (
    (keyword::unsigned_  , [&] { ++tcs.unsigned_.times; tcs.unsigned_.loc = loc; })
    | (keyword::signed_, [&] { ++tcs.signed_.times; tcs.signed_.loc     = loc; })
    | (keyword::long_  , [&] { ++tcs.long_.times; tcs.long_.loc         = loc; })
    | (keyword::short_ , [&] { ++tcs.short_.times; tcs.short_.loc       = loc; })
    | (keyword::char_  , [&] { type->type = tree::char_type_node;              })
    | (keyword::int_   , [&] { type->type = tree::int_type_node;               })
    | (keyword::void_  , [&] { type->type = tree::void_type_node;              })
    | (keyword::float_ ,  [&] { type->type = tree::float_type_node;            })
    | (keyword::double_,  [&] { type->type = tree::double_type_node;           })
    | &parser::struct_or_union_specifier / type->type
    | &parser::typedef_spec / type);
}


bool parser::attribute_list(std::vector<tree::attribute> &attr_list) {
  bool r{};
  for(bool tmp; tmp = *this <= keyword::__attribute___; *this <= ")"_req >> ")"_req) {
    r |= tmp;
    for(*this <= "("_req >> "("_req;;) {
      auto tok = peek_token();
      if(!require(type_c<sema::id>))
        break;

      tree::attribute attr {.name = sema::id(tok).name };

      if(*this <= "("_s) {
        if((attr.name == "access" || attr.name == "__format__") && is<sema::id>(peek_token())) {
          sema::id &id = peek_token();
          attr.arguments.emplace_back(tree::identifier_token{{  .loc = peek_token().loc, .str = id.name }});
          consume();
          *this <= ","_s;
        }

        if(peek_token() != ")"_s)
          for(;;) {
            if(auto expr = expression())
              attr.arguments.emplace_back(expr);
            if(!(*this <= ","_s))
              break;
          }

          *this <= ")"_req;
      }
      attr_list.emplace_back(mov(attr));

      if(*this <= ","_s)
        continue;;
      break;
    }
  }
  return r;
}

bool parser::declspec(decl_specifier_seq &dss, bool scs_ok) {
  dss.type = tree::type_name_t{};
  type_spec_state tss;

  bool r{};
  while(type_specifier(dss.type, tss) || type_qualifer(dss.type) || attribute_list(dss.attrs)
    || [&] {
      location_t loc = peek_token().loc;
      storage_class_spec scs{};
      bool r = scs_ok && storage_class_specifier(scs);
      if(r) {
        if(dss.storage_class.index())
          error(loc, {}, "multiple storage class specifier appears");
        dss.storage_class = scs;
        dss.storage_class_loc = loc;
      }
      return r;
    }())
    r = true;

  process_type_spec(tss, dss.type->type);
  return r;
}




auto parser::parameter_declaration() {
  decl_specifier_seq dss{};
  if(!declspec(dss))
    error(peek_token().loc, {}, "expected declaration specifier in parameter");

  sema::id id;

  dss.type = declarator(id, dss.type, dss.attrs);

  if(dss.type->type.is<tree::function_type_t>())
    dss.type = tree::make_pointer(dss.type);
  return tree::declarator{ .name = id.name, .type = dss.type };
}


void parser::function_parameters(tree::function_type_t &fun) {
  if(peek_token() != ")"_s) {
    scopes.push_scope();
    do {
      if(*this <= "..."_s) {
        fun.is_variadic = true;
        break;
      }
      auto dctor = parameter_declaration();
      if(dctor.name.size()
        && std::ranges::find_if(fun.params, [&](auto &d) {
          return dctor.name == d.name;
        }) != fun.params.end()) {
        error(peek_token().loc, {}, "dublicate parameter declaration '{}'", dctor.name);
      continue;
        }
        fun.params.emplace_back(dctor);
    } while(*this <= ","_s);
    scopes.pop_scope();
  }
  *this <= ")"_req;
}
tree::type_name parser::direct_declarator(sema::id &id, tree::type_name base, std::vector<tree::attribute> &attrs) {
  if(is<sema::id>(peek_token()) && id.name.empty()) {
    if(id.name.size()) {
      error(peek_token().loc, {}, "dublicate name appears in declaration");
      return {};
    }
    id = peek_token();
    consume();
  }

  if(*this <= "("_s) {
    attribute_list(attrs);
    if(starts_typename(peek_token()) || peek_token() == ")"_s) {
      tree::function_type_t fun{.return_type = base};
      function_parameters(fun);
      fun.return_type = direct_declarator(id, fun.return_type, attrs);
      tree::function_type ptr = mov(fun);
      ptr->ptr_type = {{.type = ptr}};
      return tree::type_name{{.type = ptr}};
    } else {
      tree::type_name stub = tree::type_name_t{};
      auto type = declarator(id, stub, attrs);
      *this <= ")"_req;
      *stub = *direct_declarator(id, base, attrs);
      return type;
    }
  }
  if(*this <= "["_s) {
    tree::expression numof;
    if(peek_token() != "]"_s)
      numof = assignment_expression();
    *this <= "]"_req;
    return tree::type_name{{.type = tree::array{{.type = base, .numof = numof}}}};
  }
  return base;
}


tree::type_name parser::declarator(sema::id &id, tree::type_name base, std::vector<tree::attribute> &attrs) {
  tree::type_name type = base;
  auto make_pointer = [&] {
    type = tree::type_name{{.type = tree::pointer{{.type = type}}}};
  };
  while(*this <= ("*"_s, make_pointer) >> *(&parser::type_qualifer / type | &parser::attribute_list / type->attrs));
  return direct_declarator(id, type, attrs);
}




tree::initializer_list parser::initializer_list() {
  tree::initializer_list_t init_list;

  for(; peek_token() && peek_token() != "}"_s; *this <= ","_s) {
    tree::initializer_list_t::initializer init;
    bool dchain{};
    while(*this <= ((("["_s, [&] {
      init.dchain.emplace_back(
        tree::initializer_list_t::array_designator{conditional_expression()}
      );
    }) >> "]"_req)
      | ("."_s, [&] {
        auto tok = peek_token();
        if(!require(type_c<sema::id>))
          return false;
        init.dchain.emplace_back(
          tree::initializer_list_t::struct_designator{ sema::id(tok).name}
        );
        return true;
      })
    ))
      dchain = true;
      if(dchain) *this <= "="_s;
      init.init = initializer();

    init_list.list.emplace_back(mov(init));
  }
  *this <= "}"_req;
  return init_list;
}

tree::expression parser::initializer() {
  tree::expression r;
  *this <= ("{"_s >> &parser::initializer_list % r | &parser::assignment_expression % r);
  return r;

}

tree::decl parser::init_decl(decl_specifier_seq &dss, bool tail) {
  sema::id dector_name;
  auto dector_type = declarator(dector_name, dss.type, dss.attrs);
  location_t loc = peek_token().loc;
  *this <= &parser::attribute_list / dss.attrs;
  tree::decl decl;

  if(!dector_type->type) {
    error(peek_token().loc, {}, "expected type in declaration");
    return {};
  }

  dector_name.node = &get_or_def_node(dector_name);
  dector_name.level = scopes.stack.size() - 1;
  decl = build_decl({loc}, dector_name, dector_type, dss.storage_class);
  bool block_decl_accept = decl(overload {
    [&](tree::function_t &fun) {
      bool body = !tail && *this <= ("{"_s, [&] {
      if(fun.scs == "extern"_s)
        error(peek_token().loc, {}, "external function definition is not allowed");

        scopes.push_scope<sema::fn_scope>({fun.type});
      tree::compound_statement_t compound;
      while(peek_token() && peek_token() != "}"_s)
        compound.emplace_back(block_item());
        scopes.pop_scope();
      *this <= "}"_req;
      fun.definition = compound;
      });

      if(!body && !tail)
        return !(*this <= ";"_s);
      return !body && !tail;
    },
    [&](tree::variable_t &var) {
      if(*this <= "="_s) {
        auto init = initializer();
        if(get_common_type(lex::assign_tok{"="_s}, init->loc, strip_type(var.type), strip_type(init->type)))
          var.definition = init;
      }
      if(!tail)
        return !(*this <= ";"_s);
      return !tail;
    },
    [&](auto &s) {
      return !(*this <= ";"_s);
    }
  });

  if(block_decl_accept) {
    while(*this <= ","_s) {
      tree::block_decl_t block{{decl}};
      do block.emplace_back(init_decl(dss, true)); while(*this <= ","_s);
      decl = block;
    }
    *this <= ";"_req;
  }
  return decl;
}
tree::decl parser::declaration() {
  decl_specifier_seq dss{};
  if(!declspec(dss)) {
    consume();
    error(peek_token().loc, {}, "declaration specifier expected");
  } else
    return init_decl(dss);
  return {};
}
tree::type_name parser::type_name() {
  decl_specifier_seq dss{};
  declspec(dss, false);
  sema::id id;
  dss.type = declarator(id, dss.type, dss.attrs);

  if(id.name.size())
    error(peek_token().loc, {}, "name appear in abstract declarator");

  return dss.type;
}
tree::if_statement parser::if_statement() {
  tree::expression cond;
  *this <= "("_req >> &parser::expression % cond >> ")"_req;

  tree::statement if_stmt = statement(), else_stmt;
  if(*this <= keyword::else_)
    else_stmt = statement();

  return {{.cond = cond, .if_stmt = if_stmt, .else_stmt = else_stmt}};
}
tree::statement parser::block_item() {
  decl_specifier_seq dss{};
  if(declspec(dss))
    return init_decl(dss);

  return statement();
}
tree::compound_statement parser::compound_statement() {
  scopes.push_scope();
  tree::compound_statement_t compound;
  while(peek_token() != "}"_s)
    compound.emplace_back(block_item());
  scopes.pop_scope();
  *this <= "}"_req;
  return compound;
}


tree::switch_statement parser::switch_statement() {
  tree::expression cond;
  *this <= "("_s >> &parser::expression % cond >> ")"_req;

  tree::switch_statement switch_{{.cond = cond}};
  switch_->stmt = secondary_block(sema::switch_scope{switch_});
  return switch_;
}
tree::while_statement parser::while_statement() {
  tree::expression cond;
  *this <= "("_s >> &parser::expression % cond >> ")"_req;

  return {{ .cond = cond, .body = secondary_block<sema::control_scope>() }};
}
tree::do_while_statement parser::do_while_statement() {
  tree::statement body = secondary_block<sema::control_scope>();
  tree::expression cond;
  *this <= keyword::while_ >> "("_s >> &parser::expression % cond >> ")"_req;

  return {{.cond = cond, .body = body}};
}
tree::for_statement parser::for_statement() {
  tree::for_statement_t for_;

  *this <= "("_req;

  if(starts_declspec(peek_token())) {
    decl_specifier_seq dss;
    for_.clause = declaration();
  } else {
    if(peek_token() != ";"_s)
      for_.clause = expression();
    *this <= ";"_req;
  }
  if(peek_token() != ";"_s)
    for_.cond = expression();
  *this <= ";"_req;
  if(peek_token() != ")"_s)
    for_.step = expression();
  *this <= ")"_req;

  for_.body = secondary_block<sema::control_scope>();
  return for_;
}


tree::statement parser::expression_statement() {
  tree::expression expr = expression();
  if(!expr) {
    size_t curly_paren_count = 0;
    for(;;) {
      if(*this <= "{"_s)
        ++curly_paren_count;
        if(*this <= "}"_s && !curly_paren_count--)
          break;
      if(*this <= ";"_s || !peek_token())
        break;
      consume();
    }
    return {};
  }
  if(peek_token() != ";"_s) {
    error(peek_token().loc, {}, "2");
  }
  *this <= ";"_req;
  return expr;
}

tree::goto_statement parser::goto_statement() {
  return visit(peek_token(), overload {
    [&](sema::id id)  {
      consume();
      tree::goto_statement r = tree::goto_statement_t{};
      scopes.ctx_scope_get<sema::fn_scope>()
      .top().get().labels.lookup_label(id.name, r);
      return r;
    },
    [&](decltype("*"_s))  {
      return tree::goto_statement{{.target = assignment_expression()}};
    },
    [&](auto &) -> tree::goto_statement {
      error(peek_token().loc, {}, "expected label or pointer to goto to");
      consume();
      return {};
    }
  });
}
tree::statement parser::statement() {
  location_t loc = peek_token().loc;
  if(*this <= keyword::if_)
    return if_statement();
  if(*this <= keyword::switch_)
    return switch_statement();
  if(*this <= keyword::while_)
    return while_statement();
  if(*this <= keyword::do_)
    return do_while_statement();
  if(*this <= keyword::for_)
    return for_statement();
  if(*this <= keyword::goto_)
    return goto_statement();
  if(*this <= keyword::case_) {
    auto &switch_scopes = scopes.ctx_scope_get<sema::switch_scope>();
    if(switch_scopes.empty()) {
      error(peek_token().loc, {}, "case label without according switch");
      return {};
    }

    auto cond = conditional_expression();
    *this <= ":"_req;

    tree::case_statement case_{{ .cond = cond, .stmt = statement()}};
    switch_scopes.top().get().tree->cases.emplace_back(case_);
    return case_;
  }
  if(*this <= keyword::break_) {
    if(!scopes.ctx_scope_get<sema::switch_scope>().empty()
      && !scopes.ctx_scope_get<sema::control_scope>().empty()
    ) {
      error(peek_token().loc, {loc}, "stray break statement");
      return {};
    }

    *this <= ";"_req;
    return tree::break_statement_t{};
  }
  if(*this <= keyword::continue_) {
    if(!scopes.ctx_scope_get<sema::switch_scope>().empty()
      && !scopes.ctx_scope_get<sema::control_scope>().empty()
    ) {
      error(peek_token().loc, {}, "stray break statement");
      return {};
    }

    *this <= ";"_req;
    return tree::continue_statement_t{};
  }
  if(*this <= "{"_s)
    return compound_statement();


    if(is<sema::id>(peek_token()) && peek_2nd_token() == ":"_s) {
      auto tok = peek_token();
      sema::id id = tok;
      consume();
      location_t colon_loc = peek_token().loc;
      consume();

      auto stmt = block_item();

      tree::label label{{.name = id.name, .stmt = stmt}};
      if(!scopes.ctx_scope_get<sema::fn_scope>()
        .top().get().labels.process_label(label)
      ) {
        error(tok.loc, {colon_loc}, "redeclaration of label named '{}'", id.name);
        return {};
      }
      return label;
    }

    if(peek_token() != ";"_s) return expression_statement();
    *this <= ";"_s;
  return {};
}
}}
