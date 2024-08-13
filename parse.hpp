#pragma once

#include "pp.hpp"
#include "tree.hpp"
#include "lex-spirit.hpp"
#include "tree-trait.hpp"
#include "tree-dump.hpp"
#include "variant.hpp"

namespace c9 { namespace parse {
using lex::keyword;
enum class storage_class_spec {
  none,
  typedef_,
  extern_,
  static_,
  auto_,
  register_
};

struct decl_specifier_seq {
  tree::type_name type;
  storage_class_spec storage_class;
  std::vector<tree::attribute> attrs;

  location_t storage_class_loc;
};

sv to_sv(storage_class_spec scs) {
  constexpr static sv tab[] = {
    "", "typedef", "extern", "static", "auto", "register"
  };
  return tab[size_t(scs)];
}





struct parser : lex_spirit {
  driver &d;

  sema::semantics sm;

  parser(driver &d, auto&& ...x) : d{d}, lex_spirit{*this, (decltype(x)) x...} {}

  bool require(auto &&tok) {
    bool b;
    if(b = (peek_token() == tok))
      consume();
    else
      error(peek_token().loc - 1, {}, "'{}' expected", lex::stringnize(tok));

    return b;
  }
  bool require(type_<sema::id>) {
    bool b;
    if(b = is<sema::id>(peek_token()))
      consume();
    else
      error(peek_token().loc, {}, "identifier expected");
    return b;
  }
  bool require(type_<lex::numeric_constant>) {
    bool b;
    if(b = is<lex::numeric_constant>(peek_token()))
      consume();
    else
      error(peek_token().loc, {}, "numeric constant expected");
    return b;
  }


  bool starts_typename(lex::token tok) {
    return visit(tok, overload {
      [](auto &) { return false; },
      [](sema::id &id) { return id.node && is<tree::typedef_decl_t>(id.node->node->decl); },
      [](keyword kw) {
        switch(kw) {
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

  tree::expression primary_expression() {
    return visit(peek_token(), overload {
      [&](sema::id &id) -> tree::expression {
        location_t loc = peek_token().loc;
        consume();
        if(id.node) {
          if(is<tree::variable_t>(id.node->node->decl))
            return tree::variable(id.node->node->decl);
          if(is<tree::function_t>(id.node->node->decl))
            return tree::function(id.node->node->decl);
        }
        error(loc, {}, "primary expected");
        return {};
      },
      [&](lex::numeric_constant nc) {
        consume();
        lex::interpret_status stat;
        auto v = lex::interpret_nc(nc, stat);
        return visit(v, overload {
          [&](lex::integer int_) -> tree::expression{
            uint64_t value;
            if(stat == lex::interpret_status::out_of_range) {

            } else {
              value = int_.value;
            }
            return tree::int_cst_expression{{.value = value, .type = tree::int_type_node}};
          },
          [&](lex::floating)-> tree::expression {}
        });
      },
      [&](lex::char_literal cl) -> tree::expression {
        consume();
        variant_t<""_s, "U"_s, "u"_s, "u8"_s, "L"_s, "L"_s> prefix;

        auto s = cl.begin();
        lex::scan_impl(s, prefix, variant_types(prefix), 0_c, ""_s);
        ++s;

        return tree::int_cst_expression{{ .value = *s, .type = tree::char_type_node}};
      },
      [&](decltype("("_s)) -> tree::expression {
      //  if(*this <= "{"_s)
        //  return tree::statement_expression{{ .stmts = compound_statement({ .function = })}};

        consume();
        auto expr = expression();
        if(expr) {
          *this <= ")"_req;
          return expr;
        }
        return {};
      },
      [&](auto &) -> tree::expression {
        location_t loc = peek_token().loc;
        consume();
        error(loc, {}, "primary expected"); return {};
      }
    });
  }
  tree::expression postfix_expression() {
    tree::expression primary = primary_expression();
    return visit(peek_token(), overload {
      [&](auto &) { return primary; },
      [&](decltype("["_s)) -> tree::expression {
        consume();
        tree::expression with = expression();
        *this <= "]"_req;
        return tree::subscript_expression{{ .of = primary, .with = with }};
      },
      [&](decltype("("_s)) -> tree::expression {
        consume();
        std::vector<tree::expression> args;
        if(peek_token() != ")"_s)
          do args.emplace_back(assignment_expression()); while(*this <= ","_s);
        *this <= ")"_req;
        return tree::function_call{{ .calee = primary, .args = mov(args)}};
      },
      [&](decltype("."_s)) -> tree::expression {
        consume();
        auto tok = peek_token();
        if(!require(type_c<sema::id>))
          return tree::access_member{{ .expr = primary, .member_name = sema::id(tok).name }};

        return {};
      },
      [&](decltype("->"_s)) -> tree::expression {
        consume();
        auto tok = peek_token();
        if(!require(type_c<sema::id>))
          return tree::pointer_access_member{{ .expr = primary, .member_name = sema::id(tok).name }};
        return {};
      },
      [&](decltype("++"_s)) -> tree::expression {
        consume();
        return tree::post_increment{{.expr = primary }};
      },
      [&](decltype("--"_s)) -> tree::expression {
        consume();
        return tree::post_decrement{{.expr = primary }};
      }
    });
  }
  tree::expression unary_expression() {
    return visit(peek_token(), overload {
      [&]<char ...c>(string_seq<c...> s) -> tree::expression  requires (lex::unary_puncs(contains(s))) {
        consume();
        return tree::unary_expression{{ .op = s, .expr = unary_expression() }};
      },
      [&](auto &) { return postfix_expression(); }
    });
  }
  tree::expression cast_expression() {
    if(peek_token() == "("_s && starts_typename(peek_2nd_token())) {
      consume();
      auto type = type_name();
      *this <= ")"_req;
      return tree::cast_expression{{ .cast_from = cast_expression(),  .cast_to = type, }};
    }
    return unary_expression();
  }

  /* https://github.com/gcc-mirror/gcc/blob/f80db5495d5f8455b3003951727eb6c8dc67d81d/gcc/c/c-parser.cc#L9372
   * Pretty beautiful stack based binary expression parser */
  tree::expression binary_expression() {
    struct {
      tree::expression expr;
      lex::binary_tok op;
      lex::binary_prec prec{};
    } stack[lex::num_of + 1];

    size_t sp{};
    auto pop = [&]{
      visit(stack[sp].op, [&](auto s) {
        stack[sp - 1].expr = tree::binary_expression{{
          .op = s, .lhs = stack[sp - 1].expr, .rhs = stack[sp].expr
        }};
      });
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

  tree::expression conditional_expression() {\
    tree::expression cond, lhs, rhs;
    *this <= &parser::binary_expression % cond
          >> -("?"_s >> &parser::expression % lhs
               >> ":"_req >> &parser::conditional_expression % rhs
              );

    if(lhs)
      return tree::ternary_expression{{ .cond = cond, .lhs = lhs, .rhs = rhs  }};
    return cond;
  }
  tree::expression assignment_expression() {
    auto lhs = conditional_expression();
    visit(peek_token(), overload {
      [](auto &&) {},
      [&]<char ...c>(string_seq<c...> s) requires (lex::is_assign(s)) {
        consume();
        lhs = tree::assign_expression{{.op = s, .lhs = lhs, .rhs = assignment_expression()}};
      }
    });
    return lhs;
  }
  tree::expression expression() {
    auto tree = assignment_expression();
    while(*this <= ","_s)
     tree = tree::comma_expression{{.lhs = tree, .rhs = assignment_expression()}};
    return tree;
  }


  struct type_spec_state {
    struct {
      size_t times{};
      location_t loc;
    } unsigned_, signed_, long_, short_;
  };


  bool type_qualifer(tree::type_name &type) {
    return *this <= ((keyword::const_,     [&] { type->is_const = true;    })
                    | (keyword::volatile_, [&] { type->is_volatile = true; })
                    | (keyword::restrict_, [&] { type->is_restrict = true; }));
  }

  bool storage_class_specifier(enum storage_class_spec &scs) {
    return *this <= ((keyword::typedef_,   [&] { scs = storage_class_spec::typedef_;  })
                    | (keyword::extern_,   [&] { scs = storage_class_spec::extern_;   })
                    | (keyword::auto_,     [&] { scs = storage_class_spec::auto_;     })
                    | (keyword::static_,   [&] { scs = storage_class_spec::static_;   })
                    | (keyword::register_, [&] { scs = storage_class_spec::register_; }));
  }

   bool struct_or_union_specifier(tree::type_decl &td) {
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

    auto &node = sm.get_or_def_node(name);

    if(*this <= "{"_s) {
      tree::structural_decl_t s{.name = name.name};
      while(peek_token() && peek_token() != "}"_s) {
        decl_specifier_seq dss;
        declspec(dss);

        if(dss.storage_class != storage_class_spec::none)
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

      if(is_struct) {
        tree::struct_decl_t d{{s}};
        if(name.name.size())
          td = do_definition(name, &sema::node_t::struct_decl, d);
        else
          td = d;
      } else {
        tree::union_decl_t d{{s}};
        if(name.name.size())
          td = do_definition(name, &sema::node_t::union_decl, d);
        else
          td = d;
      }
      return true;
    }

    if(is_struct) td = node.struct_decl;
    else          td = node.union_decl;
    return true;
  }

  bool typedef_spec(tree::type_name &type) {
    if(is<sema::id>(peek_token())) {
      sema::id &id = peek_token();
      if(id.node && is<tree::typedef_decl_t>(id.node->node->decl)) {
        type = ((tree::typedef_decl_t &) id.node->node->decl).type;
        consume();
        return true;
      }
    }
    return false;
  }

  bool type_specifier(tree::type_name &type, type_spec_state &tcs) {
    return *this <= (
      (keyword::unsigned_  , [&] { ++tcs.unsigned_.times; tcs.unsigned_.loc = peek_token().loc; })
        | (keyword::signed_, [&] { ++tcs.signed_.times; tcs.signed_.loc = peek_token().loc;     })
        | (keyword::long_  , [&] { ++tcs.long_.times; tcs.long_.loc = peek_token().loc;         })
        | (keyword::short_ , [&] { ++tcs.short_.times; tcs.short_.loc = peek_token().loc;       })
        | (keyword::char_  , [&] { type->type = tree::char_type_node;                       })
        | (keyword::int_   , [&] { type->type = tree::int_type_node;                        })
        | &parser::struct_or_union_specifier / type->type
        | &parser::typedef_spec / type);
   }

  void process_type_spec(type_spec_state &tss, tree::type_name type) {

  }

  bool attribute_list(std::vector<tree::attribute> &attr_list) {
    bool r{};
    for(bool tmp; tmp = *this <= keyword::__attribute___; *this <= ")"_req >> ")"_req) {
      r |= tmp;
      for(*this <= "("_req >> "("_req;;) {
        auto tok = peek_token();
        if(!require(type_c<sema::id>))
          break;

        tree::attribute attr {.name = sema::id(tok).name };

        if(*this <= "("_s) {
          if(attr.name == "access" && is<sema::id>(peek_token())) {
            sema::id &id = peek_token();
            attr.arguments.emplace_back(tree::identifier_token{{  .loc = peek_token().loc, .str = id.name }});
            consume();
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

  bool declspec(decl_specifier_seq &dss, bool scs_ok = true) {
    dss.type = tree::type_name_t{};
    dss.storage_class = storage_class_spec::none;
    type_spec_state tss;

    bool r{};
    while(type_specifier(dss.type, tss) || type_qualifer(dss.type) || attribute_list(dss.attrs)
      || [&] {
        location_t loc = peek_token().loc;
        storage_class_spec scs{};
        bool r = scs_ok && storage_class_specifier(scs);
        if(r) {
          if(bool(dss.storage_class))
            error(loc, {}, "multiple storage class specifier appears");
          dss.storage_class = scs;
          dss.storage_class_loc = loc;
        }
        return r;
      }())
       r = true;

    process_type_spec(tss, dss.type);
    return r;
  }


  bool starts_declspec(lex::token tok) {
    return starts_typename(tok) ||
      *this <= (keyword::typedef_ | keyword::extern_ | keyword::auto_
                 | keyword::static_ | keyword::register_);
  }


  auto parameter_declaration() {
    decl_specifier_seq dss{};
    if(!declspec(dss))
      error(peek_token().loc, {}, "expected declaration specifier in parameter");

    sema::id id;

    dss.type = declarator(id, dss.type, dss.attrs);

    if(is<tree::function_type_t>(dss.type->type))
      dss.type = tree::make_pointer(dss.type);
    return tree::declarator{ .name = id.name, .type = dss.type };
  }


  void function_parameters(tree::function_type_t &fun) {
    if(peek_token() != ")"_s) {
      sm.scopes.push_scope();
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
      sm.scopes.pop_scope();
    }
    *this <= ")"_req;
  }
  tree::type_name direct_declarator(sema::id &id, tree::type_name base, std::vector<tree::attribute> &attrs) {
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
        return tree::type_name{{.type = fun}};
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


  tree::type_name declarator(sema::id &id, tree::type_name base, std::vector<tree::attribute> &attrs) {
    tree::type_name type = base;
    auto make_pointer = [&] {
      type = tree::type_name{{.type = tree::pointer{{.type = type}}}};
    };
    while(*this <= ("*"_s, make_pointer) >> *(&parser::type_qualifer / type | &parser::attribute_list / type->attrs));
    return direct_declarator(id, type, attrs);
  }


  template<class D> D do_definition(sema::id id, D sema::node_t::*field, auto decl)  {
    auto &node = sm.get_or_def_node(id);
    if(node.*field) {
       error(peek_token().loc, {}, "redifinition of '{}'", id.name);
      return {};
    } else
      return node.*field = decl;
  }


  tree::expression designator() {
    tree::designator_list_t dlist;
    while(peek_token() != "}"_s) {
      if(*this <= "["_s) {
        tree::expression index, init;
        *this <= &parser::conditional_expression % index
                  >> "]"_req >> "="_req >> &parser::initializer % init;
        dlist.list.emplace_back(tree::designator_list_t::array_designator{ index, init});
      } else if(*this <= "."_s) {
        auto tok = peek_token();
        if(!require(type_c<sema::id>))
          break;
        tree::expression init;
        *this <= "="_req >> &parser::initializer % init;
        dlist.list.emplace_back(tree::designator_list_t::struct_designator{ sema::id(tok).name, init});
      } else
        consume();

      if(!(*this <= ","_s))
        break;
    }

    *this <= "}"_req;

    return dlist;
  }

  tree::expression initializer() {
    tree::expression r;
    *this <= ("{"_s >> &parser::designator % r | &parser::assignment_expression % r);
    return r;
  }

  tree::decl init_decl(decl_specifier_seq &dss, bool tail = false) {
    sema::id dector_name;
    auto dector_type = declarator(dector_name, dss.type, dss.attrs);
    tree::decl decl;


    auto declarate = [&](auto tree, auto type) {
      if(dss.storage_class == storage_class_spec::typedef_)
         decl = tree::typedef_decl{{ .name = dector_name.name, .type = type}};
      else
         decl = tree;

      decl = do_definition(dector_name, &sema::node_t::decl, decl);
    };

    dector_type->type(overload {
      [&](tree::empty_node_t &) {
        error(peek_token().loc, {}, "expected type in declaration");
      },
      [&](tree::function_type_t &f) {
        tree::function fun {{
          .name = dector_name.name,
          .type =  tree::function_type(dector_type->type)
        }};
        declarate(fun, dector_type);
        bool body = !tail && *this <= ("{"_s, [&] {
          ;
          sm.scopes.push_scope<sema::fn_scope>();
          for(auto &dector : f.params)
             do_definition(sema::id{dector.name}, &sema::node_t::decl, tree::variable_t{
               .name = dector.name, .type = dector_type
             });

          tree::compound_statement_t compound;
          while(peek_token() && peek_token() != "}"_s)
            compound.emplace_back(block_item());
          sm.scopes.pop_scope();
          *this <= "}"_req;
          fun->definition = compound;
        });


        if(!tail && !body && *this <= ","_s) {
          tree::block_decl_t block{{decl}};
          do block.emplace_back(init_decl(dss, true)); while(*this <= ","_s);
          decl = block;
        }

        if(!body && !tail)
          *this <= ";"_req;
      },
      [&](auto &) {
        tree::variable var {{.name = dector_name.name, .type = dector_type,
          .is_global = sm.scopes.in_global()
        }};

        declarate(var, dector_type);

        if(*this <= "="_s)
          var->definition = initializer();


        if(!tail && *this <= ","_s) {
          tree::block_decl_t block{{decl}};
          do block.emplace_back(init_decl(dss, true)); while(*this <= ","_s);
          decl = block;
        }
        if(!tail )
          *this <= ";"_req;
      }
    });
    return decl;
  }
  tree::decl declaration() {
    decl_specifier_seq dss{};
    if(!declspec(dss)) {
      consume();
      error(peek_token().loc, {}, "declaration specifier expected");
    } else
      return init_decl(dss);
    return {};
  }
  tree::type_name type_name() {
    decl_specifier_seq dss{};
    declspec(dss, false);
    sema::id id;
    dss.type = declarator(id, dss.type, dss.attrs);

    if(id.name.size())
      error(peek_token().loc, {}, "name appear in abstract declarator");

    return dss.type;
  }
  tree::if_statement if_statement() {
    tree::expression cond;
    *this <= "("_req >> &parser::expression % cond >> ")"_req;

    tree::statement if_stmt = statement(), else_stmt;
    if(*this <= keyword::else_)
      else_stmt = statement();

    return {{.cond = cond, .if_stmt = if_stmt, .else_stmt = else_stmt}};
  }
  tree::statement block_item() {
    decl_specifier_seq dss{};
    if(declspec(dss))
      return init_decl(dss);

    return statement();
  }
  tree::compound_statement compound_statement() {
    sm.scopes.push_scope();
    tree::compound_statement_t compound;
    while(peek_token() != "}"_s)
      compound.emplace_back(block_item());
    sm.scopes.pop_scope();
    *this <= "}"_req;
    return compound;
  }

  template<class Scope> tree::statement secondary_block(Scope scope = Scope{}) {
    sm.scopes.push_scope(mov(scope));
    tree::statement stmt;
    if(*this <= "{"_s) {
      tree::compound_statement_t compound;
      while(peek_token() != "}"_s)
        compound.emplace_back(block_item());
      *this <= "}"_req;
      stmt = compound;
    } else
      stmt = statement();
    sm.scopes.pop_scope();
    return stmt;
  }
  tree::switch_statement switch_statement() {
    tree::expression cond;
    *this <= "("_s >> &parser::expression % cond >> ")"_req;

    tree::switch_statement switch_{{.cond = cond}};
    switch_->stmt = secondary_block(sema::switch_scope{switch_});
    return switch_;
  }
  tree::while_statement while_statement() {
    tree::expression cond;
    *this <= "("_s >> &parser::expression % cond >> ")"_req;

    return {{ .cond = cond, .body = secondary_block<sema::control_scope>() }};
  }
  tree::do_while_statement do_while_statement() {
    tree::statement body = secondary_block<sema::control_scope>();
    tree::expression cond;
    *this <= keyword::while_ >> "("_s >> &parser::expression % cond >> ")"_req;

    return {{.cond = cond, .body = body}};
  }
  tree::for_statement for_statement() {
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


  tree::statement statement_expression() {
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
    *this <= ";"_req;
    return expr;
  }

  tree::goto_statement goto_statement() {
    return visit(peek_token(), overload {
      [&](sema::id id)  {
        consume();
        tree::goto_statement r = tree::goto_statement_t{};
        sm.scopes.ctx_scope_get<sema::fn_scope>()
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
  tree::statement statement() {
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
      auto &switch_scopes = sm.scopes.ctx_scope_get<sema::switch_scope>();
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
      if(!sm.scopes.ctx_scope_get<sema::switch_scope>().empty()
        && !sm.scopes.ctx_scope_get<sema::control_scope>().empty()
      ) {
        error(peek_token().loc, {loc}, "stray break statement");
        return {};
      }

      *this <= ";"_req;
      return tree::break_statement_t{};
    }
    if(*this <= keyword::continue_) {
      if(!sm.scopes.ctx_scope_get<sema::switch_scope>().empty()
        && !sm.scopes.ctx_scope_get<sema::control_scope>().empty()
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
      if(!sm.scopes.ctx_scope_get<sema::fn_scope>()
          .top().get().labels.process_label(label)
      ) {
        error(tok.loc, {colon_loc}, "redeclaration of label named '{}'", id.name);
        return {};
      }
      return label;
    }

    if(peek_token() != ";"_s) return statement_expression();
    *this <= ";"_s;
    return {};
  }


  template<class ...T>
  void error(location_t loc, std::initializer_list<location_t> locs, std::format_string<T...> fmt, T&& ...args) {
    rich_location rcl{loc, locs};
    d.diag.diagnostic_impl(stderr, rcl, "error"_s, fmt, (decltype(args)) args...);
  }
};

sema::id lex_spirit::lookup(lex::identifier s) { return parse.sm.lookup(s); }

template<class T>
bool lex_spirit::operator<=(require_value<T> v) {
  return parse.require(v.value);
}

}}
