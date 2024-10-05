#pragma once

#include "meta.hpp"
#include "string.hpp"
#include "variant.hpp"
#include <vector>
#include <set>
#include "diagnostics.hpp"
#include "flat-set.hpp"

#include "x86/isa.hpp"

namespace c9 {
namespace cfg { struct basic_block; };
namespace sema { struct node_t;  };
namespace lex {
constexpr auto assign_puncs = tuple("="_s, "+="_s, "-="_s, "*="_s, "/="_s, "&="_s, "|="_s, "<<="_s, ">>="_s, "^="_s);
constexpr auto binary_puncs = tuple(
    "&"_s, "*"_s, "+"_s, "-"_s,
    "/"_s, "%"_s, "<<"_s, ">>"_s,
    "<"_s, ">"_s, "<="_s, ">="_s,
    "=="_s, "!="_s, "^"_s, "|"_s,
    "&&"_s, "||"_s
);
constexpr auto unary_puncs = tuple("+"_s, "-"_s, "~"_s, "!"_s);
constexpr auto crement_puncs = tuple("++"_s, "--"_s);
using binary_tok = decltype(binary_puncs([](auto ...x) { return variant_t<x...>{}; }));
using assign_tok = decltype(assign_puncs([](auto ...x) { return variant_t<x...>{}; }));
using crement_tok = decltype(crement_puncs([](auto ...x) { return variant_t<x...>{}; }));

constexpr bool is_assign(auto x) {
  return assign_puncs([=](auto ...tok) { return ((tok == x) || ...); });
}
constexpr bool is_binop(auto x) {
  return binary_puncs([=](auto ...tok) { return ((tok == x) || ...); });
}
constexpr bool is_unary(auto x) {
  return unary_puncs([=](auto ...tok) { return ((tok == x) || ...); });
}
constexpr bool is_crement(auto x) {
  return crement_puncs([=](auto ...tok) { return ((tok == x) || ...); });
}
constexpr auto is_relational(auto x) {
  return tuple("<"_s, "<="_s, ">"_s, ">="_s, "=="_s, "!="_s)
                ([=](auto ...tok) { return ((tok == x) || ...); });
}
}}


namespace c9 { namespace tree {
template<size_t idx> struct tag { friend constexpr auto get(tag<idx>); };
template<class T> struct idx_tag { friend constexpr auto get(idx_tag<T>); };
template<size_t idx, class T> struct define {
  friend constexpr auto get(tag<idx>) { return type_c<T>; }
  friend constexpr auto get(idx_tag<T>) { return idx; }
};

template<size_t idx = 0, auto _ = []{}> constexpr auto find() {
  if constexpr(requires { get(tag<idx>{}); }) return find<idx + 1, _>();
  else return idx;
}

struct count_data {
  size_t count, index;
  char data[];
};


struct tree_arena {
  std::vector<count_data *> data;

  template<class T> count_data *allocate() {
    data.emplace_back((count_data *) malloc(sizeof(count_data) + sizeof(T)));
    return data.back();
  }

  void collect() { for(auto p : data) if(!p->count) free(p); }
} extern a;

static count_data default_;

template<class T> struct tree_type {
  constexpr static size_t type_index = find<0, []{}>();
  constexpr static define<type_index, T> def{};
};
template<class T_t> class tree_value {
  count_data *data;

  template<class U> friend class tree_value;
public:
  using value_type = T_t;

  tree_value() : data{&default_} {}
  tree_value(const struct empty_node_t &) : data{&default_} {}
  tree_value(T_t value) requires requires { get(idx_tag<T_t>{}); } {
    data = a.allocate<T_t>();
    data->count = 1;
    data->index = tree_type<T_t>::type_index;
    new(data->data) auto{mov(value)};
  }
  template<narrow<T_t> U_t> tree_value(U_t value) requires requires { get(idx_tag<U_t>{}); } {
    data = a.allocate<U_t>();
    data->count = 1;
    data->index = tree_type<U_t>::type_index;
    new(data->data) auto{mov(value)};
  }
  template<narrow<T_t> U_t> tree_value(const tree_value<U_t> &value) {
    data = value.data;
    ++data->count;
  }
  tree_value(const tree_value &value) {
    data = value.data;
    ++data->count;
  }

  ~tree_value() { --data->count; }

  template<class T> bool is_narrow();
  template<class T> bool is() const;

  tree_value cpy() { return (*this)([&](auto &x) { return tree_value{x}; }); }

  void *get_data() { return data->data; }

  explicit operator bool() const;

  decltype(auto) operator()(auto &&f);

  template<class T> constexpr size_t indexof() const { return get(idx_tag<__remove_cvref(T)>{}); }


  template<narrow<T_t> U> explicit operator U &() {
    c9_assert(index() == indexof<U>());
    return (*this)[size_c<get(idx_tag<U>{})>];
  }
  template<narrow<T_t> U> explicit operator tree_value<U>();

  auto operator->();
  T_t &operator*() requires requires { get(idx_tag<T_t>{}); } {
    c9_assert(index() == get(idx_tag<T_t>{}));
    return (T_t &) *this;
  }

  decltype(auto) operator[](auto idx) { return *std::launder((decltype(+get(tag<idx()>{})) *) data->data); }
  size_t index() const { return data->index; }

  constexpr auto notypes();
};



#define TREE_NARROW_DEF(tree, body...) \
  struct tree##_t; \
  using tree = tree_value<tree##_t>; \
  struct tree##_t body
#define TREE_DEF(tree, body...) \
  struct tree##_t; \
  struct tree##_loophole_def : tree_type<tree##_t>{}; \
  using tree = tree_value<tree##_t>; \
  struct tree##_t body



TREE_NARROW_DEF(base, {});
TREE_DEF(empty_node, : base_t {});

TREE_NARROW_DEF(statement, : base_t {});
TREE_DEF(compound_statement, : statement_t, std::vector<statement> {});

TREE_NARROW_DEF(decl, : statement_t { });
TREE_DEF(block_decl, : std::vector<decl>, decl_t {  });



TREE_NARROW_DEF(type_decl, : decl_t { });



TREE_NARROW_DEF(expression, : statement_t { type_decl type; source_range loc; });
TREE_NARROW_DEF(lvalue, : expression_t {});
TREE_NARROW_DEF(rvalue, : expression_t {});


TREE_NARROW_DEF(op, : expression_t {}); // IR

TREE_DEF(statement_expression, : rvalue_t { compound_statement stmts; });
TREE_DEF(comma_expression, : rvalue_t { expression lhs, rhs; });


TREE_DEF(binary_expression, :rvalue_t {
  decltype(lex::binary_puncs([](auto ...x) { return variant_t<x...>{}; })) op;
  expression lhs, rhs;
});
TREE_DEF(unary_expression, : rvalue_t {
  decltype(lex::unary_puncs([](auto ...x) { return variant_t<x...>{}; })) op;
  expression expr;
});
TREE_DEF(assign_expression, : lvalue_t {
  decltype(lex::assign_puncs([](auto ...x) { return variant_t<x...>{}; })) op;
  expression lhs, rhs;
});
TREE_DEF(ternary_expression, : rvalue_t { expression cond, lhs, rhs; });


TREE_DEF(postcrement_expression, : rvalue_t { lex::crement_tok op; expression expr; });
TREE_DEF(addressof, : rvalue_t { lvalue of; });
TREE_DEF(dereference, : lvalue_t { expression expr; });

TREE_DEF(function_call, : rvalue_t { expression calee; std::vector<expression> args; });
TREE_DEF(subscript_expression, : lvalue_t { expression of; expression with; });


TREE_DEF(initializer_list, : rvalue_t {
  struct array_designator { expression index; };
  struct struct_designator { string field_name; };

  struct initializer {
    std::vector<variant<array_designator, struct_designator>> dchain;
    expression init;
  };

  std::vector<initializer> list;
});

struct attribute {
  string name;
  std::vector<base> arguments;
};
TREE_DEF(type_name, : type_decl_t {
  type_decl type;
  bool is_const {}, is_volatile {}, is_restrict {};
  std::vector<attribute> attrs;
});
struct declarator {
  string name;
  type_name type;
};
TREE_DEF(variable, : decl_t, op_t {
  string name;
  type_decl type;
  expression definition;

  bool is_global {};
  variant_t<""_s, "extern"_s, "static"_s, "auto"_s, "register"_s> scs;

  std::vector<attribute> attrs;
  size_t ssa_count{}, ssa_tab_n{};
});
TREE_DEF(access_member, : lvalue_t { expression expr; variable member; });
TREE_DEF(pointer_access_member, : lvalue_t { expression expr; variable member; });

TREE_DEF(decl_expression, : lvalue_t {
  decl declref;
});

TREE_DEF(record_decl, : base_t {
  std::vector<variable> fields;

  variable find(string name);
});
TREE_NARROW_DEF(structural_decl, : type_decl_t { string name; record_decl definition; } );
TREE_DEF(struct_decl, :  structural_decl_t { });
TREE_DEF(union_decl,  :  structural_decl_t { });

TREE_DEF(typedef_decl, : type_decl_t {
  string name;
  type_name type;
});

TREE_DEF(array, : type_decl_t { type_decl type; expression numof; });

TREE_DEF(cast_expression, : rvalue_t { expression cast_from; type_decl cast_to; });
TREE_DEF(sizeof_expression, : rvalue_t {  variant<type_decl, expression> arg; });

TREE_NARROW_DEF(builtin_type, : type_decl_t {});
template<narrow<builtin_type_t> T> tree_value<T> type_node;
#define BUILTIN_TYPE_DEF(name, a...) \
  TREE_DEF(name, a); \
  static auto &name##_node =   type_node<name##_t>;
BUILTIN_TYPE_DEF(void_type, : builtin_type_t {});
TREE_NARROW_DEF(scalar_type, : builtin_type_t { constexpr bool is_scalar() { return true; } });

TREE_DEF(pointer, : scalar_type_t { type_decl type; constexpr bool is_pointer(); });
TREE_DEF(function_type, : type_decl_t {
  std::vector<declarator> params;
  type_name return_type;
  std::vector<attribute> attrs;

  bool is_variadic {};

  pointer ptr_type;
});
TREE_DEF(function, : decl_t, op_t {
  string name;
  function_type type;
  compound_statement definition;
  variant_t<""_s, "extern"_s, "static"_s> scs;
});

TREE_NARROW_DEF(arithmetic_type, : scalar_type_t { constexpr bool is_arithmetic() { return true; } });
TREE_NARROW_DEF(integer_type, : arithmetic_type_t { size_t size;  constexpr bool is_integer() { return true; }; });

TREE_NARROW_DEF(unsigned_integral_type, : integer_type_t { constexpr bool is_unsigned() { return true; } });
TREE_NARROW_DEF(signed_integral_type,   : integer_type_t { constexpr bool is_signed()   { return true;} });

BUILTIN_TYPE_DEF(char_type, : unsigned_integral_type_t {
  constexpr static sv name = "unsigned char";
  constexpr static size_t rank = 0;
});
BUILTIN_TYPE_DEF(unsigned_char_type, : unsigned_integral_type_t {
  constexpr static sv name = "unsigned char";
  constexpr static size_t rank = 0;
});
BUILTIN_TYPE_DEF(signed_char_type, : signed_integral_type_t {
  constexpr static sv name = "char";
  constexpr static size_t rank = 0;
});
BUILTIN_TYPE_DEF(short_type, : signed_integral_type_t {
  constexpr static sv name = "short";
  constexpr static size_t rank = 1;
});
BUILTIN_TYPE_DEF(unsigned_short_type, : unsigned_integral_type_t {
  constexpr static sv name = "unsigned short";
  constexpr static size_t rank = 1;
});
BUILTIN_TYPE_DEF(int_type, : signed_integral_type_t {
  constexpr static sv name = "int";
  constexpr static size_t rank = 2;
});
BUILTIN_TYPE_DEF(unsigned_int_type, : unsigned_integral_type_t {
  constexpr static sv name = "unsigned int";
  constexpr static size_t rank = 2;
});
BUILTIN_TYPE_DEF(long_type, : signed_integral_type_t {
  constexpr static sv name = "long int";
  constexpr static size_t rank = 3;
});
BUILTIN_TYPE_DEF(unsigned_long_type, : unsigned_integral_type_t {
  constexpr static sv name = "unsigned long int";
  constexpr static size_t rank = 3;
});
BUILTIN_TYPE_DEF(long_long_type, : signed_integral_type_t {
  constexpr static sv name = "long long int";
  constexpr static size_t rank = 4;
});
BUILTIN_TYPE_DEF(unsigned_long_long_type, : unsigned_integral_type_t {
  constexpr static sv name = "unsigned long int";
  constexpr static size_t rank = 4;
});


TREE_NARROW_DEF(floating_type, : arithmetic_type_t { });

BUILTIN_TYPE_DEF(float_type,       : floating_type_t {});
BUILTIN_TYPE_DEF(double_type,      : floating_type_t {});
BUILTIN_TYPE_DEF(long_double_type, : floating_type_t {});

TREE_DEF(int_cst_expression, : rvalue_t {
  uint64_t value;
  int_cst_expression_t(uint64_t value, integer_type type, source_range loc)
    : value{value}, rvalue_t{{.type = type, .loc = loc}} {}
});
TREE_DEF(float_cst_expression, : rvalue_t {
  long double value;
  float_cst_expression_t(long double value, floating_type type, source_range loc)
    : value{value}, rvalue_t{{.type = type, .loc = loc}} {}
});
TREE_DEF(compound_literal, : rvalue_t {
  type_decl typec; // TODO REMOVE
  initializer_list init;
});
TREE_DEF(if_statement, : statement_t {
  expression cond;
  statement if_stmt;
  statement else_stmt;
});
TREE_DEF(case_statement, : statement_t { expression cond; statement stmt; });
TREE_DEF(switch_statement, : statement_t {
  expression cond;
  statement stmt;
  std::vector<case_statement> cases;
});
TREE_DEF(while_statement, : statement_t { expression cond; statement body; });
TREE_DEF(for_statement, : statement_t {
  variant<empty_node, decl, expression> clause;
  expression cond, step;
  statement body;
});
TREE_DEF(do_while_statement, : statement_t {
  expression cond;
  statement body;
});
TREE_DEF(label, : decl_t {
  string name;
  statement stmt;
});
TREE_DEF(break_statement, : statement_t {});
TREE_DEF(continue_statement, : statement_t {});
TREE_DEF(goto_statement, : statement_t) { struct empty {}; variant<empty, label, expression> target; };
TREE_DEF(return_statement, : statement_t { expression expr; });
TREE_DEF(identifier_token, : base_t { location_t loc; string str; });


// IR
TREE_DEF(mov, : statement_t { expression src; op dst; });
TREE_DEF(temporary, : op_t { size_t idx; });
TREE_DEF(ssa_variable, : op_t { tree::variable var; size_t ssa_n, ssa_tab_n; });
TREE_DEF(cst, : op_t {  variant<__uint128_t, long double> data; });
TREE_DEF(target_op, : op_t { variant<x86::op> data; });
TREE_DEF(phi,  : expression_t { flat_set<op> elts; });
TREE_DEF(jump, : statement_t { cfg::basic_block &target; });
TREE_DEF(br,   : statement_t { tree::op cond; cfg::basic_block &true_, &false_; });
TREE_DEF(ssa,  : decl_t      { tree::variable var; size_t ssa_count; });


template<class T_t> tree_value<T_t>::operator bool() const { return !is<empty_node_t>(); }

template<class T_t> decltype(auto) tree_value<T_t>::operator()(auto &&f) {
  using R = std::invoke_result_t<decltype(f), T_t &>;
  return ::c9::visit(*this, overload {
    [&](narrow<T_t> auto &&value) -> R{ return f((decltype(value)) value); },
    [&](auto &&) -> R { c9_assert(0);},
    [&](empty_node_t en) -> R {
      if constexpr(requires { f(en); }) return f(en);
      else c9_assert(0);
    }
  });
}
template<class T, class Y>
constexpr decltype(auto) visit(tree_value<T> x, tree_value<Y> y, auto &&f) {
  using R = std::invoke_result_t<decltype(f), T &, Y &>;
  return ::c9::visit(x, y, overload {
    [&](narrow<T> auto &&x, narrow<Y> auto &&y) -> R {
      return f((decltype(x)) x, (decltype(y)) y);
    },
    [&](empty_node_t x, narrow<Y> auto &&y) -> R {
      return f(x, (decltype(y)) y);
    },
    [&](narrow<T> auto &&x, empty_node_t y) -> R {
       return f((decltype(x)) x, y);
     },
    [](auto &&, auto &&) -> R { c9_assert(0); },
  });
}

template<class T_t> auto tree_value<T_t>::operator->() {
  return (*this)([&](narrow<T_t> auto &tree) {return &static_cast<T_t &>(tree); });
}

template<class Q> constexpr auto tree_value<Q>::notypes() { return size_c<find()>;   }


template<class T_t> template<class T> bool tree_value<T_t>::is_narrow() {
  return (*this)([]<class TQ>(TQ &) { return narrow<TQ, T>; });
}
template<class T_t> template<class T> bool tree_value<T_t>::is() const {
  return index() == indexof<T>();
}
template<class T_t> template<narrow<T_t> U> tree_value<T_t>::operator tree_value<U>() {
  if(!is_narrow<U>())
    return {};
  tree_value<U> r;
  r.data = data;
  ++r.data->count;
  return r;
}


inline variable record_decl_t::find(string name) {
  c9_assert(name.size());
  variable r;
  for(auto field : fields) {
    if(field->name.empty())
      field(overload {
        [&](auto &) {},
        [&](narrow<structural_decl_t> auto &tree) { r = tree.def.find(name); }
      });
    else if(field->name == name) r = field;

    if(r) break;
  }
  return r;
}
}}



#undef BUILTIN_TYPE_DEF
