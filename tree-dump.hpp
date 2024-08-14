#pragma once

#include "tree.hpp"

namespace c9 { namespace tree {


struct dumper {
  FILE *out;

  template<class ...T> void dump_print(size_t ntab, std::format_string<T...> fmt, T&& ...args) {
    fprint(out, "{:>{}}", "", ntab);
    fprintln(out, fmt, (decltype(args)) args...);
  }

  template<class T> void dump(tree_value<T> tv, size_t ntab = 0) { tv([&](auto &tree) { dump(tree, ntab); }); }
private:
  void dump(auto&, size_t) {
    fprintln(out, "{}", __PRETTY_FUNCTION__);
  }

  void dump(empty_node_t &, size_t ntab) { dump_print(ntab, "empty node");  }
// types
  void dump(char_type_t &, size_t ntab) { dump_print(ntab, "char");}
  void dump(unsigned_char_type_t &, size_t ntab) { dump_print(ntab, "unsigned char");}
  void dump(signed_char_type_t &, size_t ntab) { dump_print(ntab, "signed char");}
  void dump(int_type_t &, size_t ntab) { dump_print(ntab, "int");}
  void dump(long_type_t &, size_t ntab) { dump_print(ntab, "long");}
  void dump(long_long_type_t &, size_t ntab) { dump_print(ntab, "long long");}
  void dump(unsigned_int_type_t &, size_t ntab) { dump_print(ntab, "unsigned int");}
  void dump(unsigned_long_type_t &, size_t ntab) { dump_print(ntab, "unsigned long");}
  void dump(unsigned_long_long_type_t &, size_t ntab) { dump_print(ntab, "unsigned long long");}

  void dump(typedef_decl_t &tf, size_t ntab) {
    dump_print(ntab, "typedef: {}", tf.name);
    dump(tf.type, ntab + 2);
  }
  template<derived_from<structural_decl_t> T> void dump(T &structural, size_t ntab) {
    dump_print(ntab, "{}: {}", __is_same(T, struct_decl_t) ? "struct" : "union", structural.name);
    // TODO dump struct
  }
  void dump(function_type_t fun_type, size_t ntab) {
    dump_print(ntab, "function-type: ");
    dump_print(ntab + 2, "return-type: ");
    dump(fun_type.return_type, ntab + 4);

    if(fun_type.params.size()) {
      dump_print(ntab + 2, "params: ");
      for(auto param : fun_type.params) {
        dump_print(ntab + 4, "name: {}", param.name);
        dump(param.type, ntab + 4);
      }
    }
  }
  void dump(pointer_t &ptr, size_t ntab) {
    dump_print(ntab, "pointer: ");
    dump(ptr.type, ntab + 2);
  }
  void dump(type_name_t &t, size_t ntab) {
    std::string str;
    if(t.is_const)
      str += "const ";
    if(t.is_volatile)
      str += "volatile ";
    if(t.is_restrict)
      str += "restrict ";
    dump_print(ntab, "type: {}", str);
    dump(t.type, ntab + 2);
  }
  void dump(array_t &arr, size_t ntab) {
    dump_print(ntab, "array:");
    dump_print(ntab + 2, "size: ");
    dump(arr.numof, ntab + 4);
    dump_print(ntab + 2, "of: ");
    dump(arr.type, ntab + 4);
  }
// declarations
  void dump(variable_t &var, size_t ntab) {
    dump_print(ntab, "var-decl: {}", var.name);
    dump(var.type, ntab);
    dump_print(ntab, "definition:");
    dump(var.definition, ntab + 2);
  }
  void dump(function_t &fun, size_t ntab) {
    dump_print(ntab, "function: {}", fun.name);
    dump(fun.type, ntab + 2);
    dump_print(ntab + 2, "definition: ");
    dump(fun.definition, ntab + 4);
  }
// statement
  void dump(compound_statement_t &compound, size_t ntab) {
    dump_print(ntab, "compound-statement:");
    for(auto &stmt : compound) dump(stmt, ntab + 2);
  }
  void dump(if_statement_t &if_, size_t ntab) {
    dump_print(ntab, "if-statement: ");
    dump_print(ntab + 2, "cond:");
    dump(if_.cond, ntab + 2);
    dump_print(ntab + 2, "if:");
    dump(if_.if_stmt, ntab + 4);
    if_.else_stmt(overload {
      [](empty_node_t &) {},
      [&](auto &else_) { dump_print(ntab + 2, "else:"); dump(else_, ntab + 4); }
    });
  }
// expressions
  void dump(int_cst_expression_t &expr, size_t ntab) {
    dump_print(ntab, "integer-constant: value: '{}'", expr.value);
  }
  void dump(binary_expression_t &expr, size_t ntab) {
    visit(expr.op, [&](auto s) {
      dump_print(ntab, "binary-expression: op: {}", s.c_str());
    });
    dump(expr.lhs, ntab + 2);
    dump(expr.rhs, ntab + 2);
  }
  void dump(unary_expression_t &expr, size_t ntab) {
    visit(expr.op, [&](auto s) {
      dump_print(ntab, "unary-expression: op: {}", s.c_str());
    });
    dump(expr.expr, ntab + 2);
  }
  void dump(cast_expression_t &expr, size_t ntab) {
    dump_print(ntab, "cast-expression:");
    dump(expr.cast_from, ntab + 2);
    dump(expr.cast_to, ntab + 2);
  }
  void dump(compound_literal_t &expr, size_t ntab) {
    dump_print(ntab, "compound-literal:");
    dump(expr.type, ntab + 2);
    dump(expr.init, ntab + 2);
  }
  void dump(ternary_expression_t &ternary, size_t ntab) {
    dump_print(ntab, "ternary-expression:");
    dump_print(ntab + 2, "cond:");
    dump(ternary.cond, ntab + 4);
    dump_print(ntab + 2, "lhs");
    dump(ternary.lhs, ntab + 4);
    dump_print(ntab + 2, "rhs:");
    dump(ternary.rhs, ntab + 4);
  }
  void dump(initializer_list_t &dlist, size_t ntab) {
    dump_print(ntab, "initializer-list:");
    for(auto &init : dlist.list) {
      for(auto &designator : init.dchain)
        visit(designator, overload {
          [&](expression_t &expr) { dump(expr, ntab + 4); },
          [&](tree::initializer_list_t::array_designator arr) {
            dump_print(ntab + 2, "array-designator:");
            dump(arr.index, ntab + 4);
          },
          [&](tree::initializer_list_t::struct_designator struct_) {
            dump_print(ntab + 2, "struct-designator: {}", struct_.field_name);;
          },
        });
      dump_print(ntab + 2, "initializer:");
      dump(init.init, ntab + 4);
    }
  }
};





}}
