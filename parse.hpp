#pragma once

#include "pp.hpp"
#include "tree.hpp"
#include "lex-spirit.hpp"
#include "tree-dump.hpp"
#include "variant.hpp"

namespace c9 { namespace parse {
using lex::keyword;
using sema::storage_class_spec;

struct decl_specifier_seq {
  tree::type_name type;
  storage_class_spec storage_class;
  std::vector<tree::attribute> attrs;

  location_t storage_class_loc;
};





struct parser : sema::semantics, lex_spirit {
  driver &d;

  parser(driver &d, auto&& ...x) : d{d}, sema::semantics{d}, lex_spirit{*this, (decltype(x)) x...} {}

  bool require(auto &&tok) {
    bool b;
    if(b = (peek_token() == tok))
      consume();
    else
      error(peek_token().loc, {}, "'{}' expected", lex::stringnize(tok));

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


  bool starts_typename(lex::token tok);

  tree::expression primary_expression();
  tree::expression postfix_expression();
  tree::expression cast_expression();
  tree::expression unary_expression();
  tree::expression binary_expression();

  tree::expression conditional_expression();
  tree::expression assignment_expression();
  tree::expression expression();

  template<class D> D do_definition(sema::id id, D sema::node_t::*field, auto decl)  {
    auto &node = get_or_def_node(id);
    if(node.*field) {
      error(peek_token().loc, {}, "redifinition of '{}'", id.name);
      return {};
    } else
      return node.*field = decl;
  }


  struct type_spec_state {
    struct {
      size_t times{};
      location_t loc;
      operator bool() { return times; }
    } unsigned_, signed_, long_, short_;
  };

  void process_type_spec(type_spec_state tss, tree::type_decl &type);


  bool type_qualifer(tree::type_name &type);

  bool storage_class_specifier(storage_class_spec &scs);

   bool struct_or_union_specifier(tree::type_decl &td);

  bool typedef_spec(tree::type_name &type);
  bool type_specifier(tree::type_name &type, type_spec_state &tcs);


  bool nested_attribute_list(std::vector<tree::attribute> &attr_list, tree::type_name type);
  bool attribute_list(std::vector<tree::attribute> &attr_list) { return nested_attribute_list(attr_list, {}); }

  bool declspec(decl_specifier_seq &dss, bool scs_ok = true);


  bool starts_declspec(lex::token tok) {
    return starts_typename(tok) ||
      *this <= (keyword::typedef_ | keyword::extern_ | keyword::auto_
                 | keyword::static_ | keyword::register_);
  }


  auto parameter_declaration();


  void function_parameters(tree::function_type_t &fun);
  tree::type_name direct_declarator(sema::id &id, tree::type_name base, std::vector<tree::attribute> &attrs) ;
  tree::type_name declarator(sema::id &id, tree::type_name base, std::vector<tree::attribute> &attrs);



  tree::initializer_list initializer_list();

  tree::expression initializer();

  tree::decl init_decl(decl_specifier_seq &dss, bool tail = false);
  tree::decl declaration() ;
  tree::type_name type_name();
  tree::if_statement if_statement();
  tree::statement block_item() ;
  tree::compound_statement compound_statement() ;

  template<class Scope> tree::statement secondary_block(Scope scope = Scope{}) {
    scopes.push_scope(mov(scope));
    tree::statement stmt;
    if(*this <= "{"_s) {
      tree::compound_statement_t compound;
      while(peek_token() != "}"_s)
        compound.emplace_back(block_item());
      *this <= "}"_req;
      stmt = compound;
    } else
      stmt = statement();
    scopes.pop_scope();
    return stmt;
  }
  tree::switch_statement switch_statement();
  tree::while_statement while_statement() ;
  tree::do_while_statement do_while_statement() ;
  tree::for_statement for_statement();


  tree::statement expression_statement() ;

  tree::goto_statement goto_statement() ;
  tree::return_statement return_statement();
  tree::statement statement();


  template<class ...T>
  void error(location_t loc, std::initializer_list<location_t> locs, std::format_string<T...> fmt, T&& ...args) {
    rich_location rcl{loc, locs};
    d.diag.diagnostic_impl(stderr, rcl, "error"_s, fmt, (decltype(args)) args...);
  }
};



template<class T>
bool lex_spirit::operator<=(require_value<T> v) {
  return parse.require(v.value);
}

}}
