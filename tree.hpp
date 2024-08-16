#pragma once

#include <tree.hpp>
#include <tree.hpp>

#include "meta.hpp"
#include "string.hpp"
#include "variant.hpp"
#include <vector>
#include <list>
#include "diagnostics.hpp"
#include "flat-map.hpp"

namespace c9 {
namespace parse { enum class storage_class_spec; sv to_sv(storage_class_spec); }
namespace sema { struct node_t; };
namespace lex {
constexpr auto assign_puncs = tuple("="_s, "+="_s, "-="_s, "*="_s, "/="_s, "&="_s, "|="_s, "<<="_s, ">>="_s, "^="_s);
constexpr auto binary_puncs = tuple(
    "&"_s, "*"_s, "+"_s, "-"_s,
    "/"_s, "%"_s, "<<"_s, ">>"_s,
    "<"_s, ">"_s, "<="_s, ">="_s,
    "=="_s, "!="_s, "^"_s, "|"_s,
    "&&"_s, "||"_s
);
constexpr auto unary_puncs = tuple("&"_s, "*"_s, "+"_s, "-"_s, "~"_s, "!"_s, "++"_s, "--"_s);
using binary_tok = decltype(binary_puncs([](auto ...x) { return variant_t<x...>{}; }));

constexpr bool is_assign(auto x) {
  return assign_puncs([=](auto ...tok) { return ((tok == x) || ...); });
}
constexpr bool is_binop(auto x) {
  return binary_puncs([=](auto ...tok) { return ((tok == x) || ...); });
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

  tree_value cpy() { return (*this)([&](auto &x) { return tree_value{x}; }); }

  operator bool() const;

  decltype(auto) operator()(auto &&f);

  template<class T> constexpr size_t indexof() const { return get(idx_tag<__remove_cvref(T)>{}); }


  template<narrow<T_t> U> explicit operator U &() {
    c9_assert(index() == indexof<U>());
    return (*this)[size_c<get(idx_tag<U>{})>];
  }
  template<narrow<T_t> U> explicit operator tree_value<U>() {
    c9_assert(index() == indexof<U>());
    tree_value<U> r;
    r.data = data;
    ++r.data->count;
    return r;
  }

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


TREE_NARROW_DEF(expression, : statement_t {});
TREE_DEF(statement_expression, : expression_t { compound_statement stmts; });
TREE_DEF(comma_expression, : expression_t { source_range locus; expression lhs, rhs; });


TREE_DEF(binary_expression, : expression_t {
  decltype(lex::binary_puncs([](auto ...x) { return variant_t<x...>{}; })) op;
  expression lhs, rhs;

  source_range locus;
});
TREE_DEF(unary_expression, : expression_t {
  decltype(lex::unary_puncs([](auto ...x) { return variant_t<x...>{}; })) op;
  expression expr;

  source_range locus;
});
TREE_DEF(assign_expression, : expression_t {
  decltype(lex::assign_puncs([](auto ...x) { return variant_t<x...>{}; })) op;
  expression lhs, rhs;
});
TREE_DEF(ternary_expression, : expression_t { expression cond, lhs, rhs; });


TREE_DEF(post_increment, : expression_t { expression expr; });
TREE_DEF(post_decrement, : expression_t { expression expr; });
TREE_DEF(access_member, : expression_t { expression expr; string member_name; });
TREE_DEF(pointer_access_member, : expression_t { expression expr; string member_name; });
TREE_DEF(function_call, : expression_t { expression calee; std::vector<expression> args; });
TREE_DEF(subscript_expression, : expression_t { expression of; expression with; });


TREE_DEF(initializer_list, : expression_t {
  struct array_designator { expression index; };
  struct struct_designator { string field_name; };

  struct initializer {
    std::vector<variant<array_designator, struct_designator>> dchain;
    expression init;
  };

  std::vector<initializer> list;
});



TREE_NARROW_DEF(type_decl, : decl_t { });

struct attribute {
  string name;
  std::vector<base> arguments;
};
TREE_DEF(type_name, : base_t {
  type_decl type;
  bool is_const {}, is_volatile {}, is_restrict {};
  std::vector<attribute> attrs;
});
struct declarator {
  string name;
  type_name type;
};
TREE_DEF(function_type, : type_decl_t {
  std::vector<declarator> params;
  type_name return_type;
  std::vector<attribute> attrs;

  bool is_variadic {};
});
TREE_DEF(function, : decl_t, expression_t {
  string name;
  function_type type;
  compound_statement definition;
});
TREE_DEF(variable, : decl_t, expression_t {
  string name;
  type_name type;
  expression definition;

  bool is_global {};
  std::vector<attribute> attrs;
});

TREE_DEF(record_decl, : type_decl_t {
  std::vector<variable> fields;

  variable find(string name);
});
TREE_NARROW_DEF(structural_decl, : type_decl_t { string name; record_decl def; } );
TREE_DEF(struct_decl, :  structural_decl_t { });
TREE_DEF(union_decl,  :  structural_decl_t { });

TREE_DEF(typedef_decl, : type_decl_t {
  string name;
  type_name type;
});
TREE_DEF(pointer, : type_decl_t { type_name type; });
TREE_DEF(array, : type_decl_t { type_name type; expression numof; });

TREE_DEF(cast_expression, : expression_t { expression cast_from; type_name cast_to; });

TREE_DEF(sizeof_expression, : expression_t {  variant<type_name, expression> arg; });

TREE_NARROW_DEF(builtin_type, : type_decl_t {});
template<narrow<builtin_type_t> T> static inline tree_value<T> type_node;
#define BUILTIN_TYPE_DEF(name, a...) \
  TREE_DEF(name, a); \
  static auto &name##_node =   type_node<name##_t>;
BUILTIN_TYPE_DEF(void_type, : builtin_type_t {});
TREE_NARROW_DEF(integer_type, : builtin_type_t { size_t size; });


TREE_NARROW_DEF(unsigned_integral_type, : integer_type_t { void unsigned_tag(); });
TREE_NARROW_DEF(signed_integral_type,   : integer_type_t { void signed_tag(); });

BUILTIN_TYPE_DEF(char_type, : unsigned_integral_type_t {});
BUILTIN_TYPE_DEF(unsigned_char_type, : unsigned_integral_type_t {});
BUILTIN_TYPE_DEF(signed_char_type, : signed_integral_type_t {});
BUILTIN_TYPE_DEF(short_type, : signed_integral_type_t {});
BUILTIN_TYPE_DEF(unsigned_short_type, : unsigned_integral_type_t {});
BUILTIN_TYPE_DEF(int_type, : signed_integral_type_t {});
BUILTIN_TYPE_DEF(unsigned_int_type, : unsigned_integral_type_t {});
BUILTIN_TYPE_DEF(long_type, : signed_integral_type_t {});
BUILTIN_TYPE_DEF(unsigned_long_type, : unsigned_integral_type_t {});
BUILTIN_TYPE_DEF(long_long_type, : signed_integral_type_t {});
BUILTIN_TYPE_DEF(unsigned_long_long_type, : unsigned_integral_type_t {});

static integer_type ptrdiff_type_node;

TREE_NARROW_DEF(floating_type, : builtin_type_t { size_t size; });

BUILTIN_TYPE_DEF(float_type,       : floating_type_t {});
BUILTIN_TYPE_DEF(double_type,      : floating_type_t {});
BUILTIN_TYPE_DEF(long_double_type, : floating_type_t {});

TREE_DEF(int_cst_expression, : expression_t {
  uint64_t value;
  integer_type type;
});
TREE_DEF(float_cst_expression, : expression_t {
  long double value;
  floating_type type;
});
TREE_DEF(compound_literal, : expression_t {
  type_name type;
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
  base clause;
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
TREE_DEF(identifier_token, : base_t { location_t loc; string str; });

template<class T_t> tree_value<T_t>::operator bool() const { return !is<empty_node_t>(*this); }

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
    [&](empty_node x, narrow<Y> auto &&y) -> R {
      return f(x, (decltype(y)) y);
    },
    [&](narrow<T> auto &&x, empty_node y) -> R {
       return f((decltype(x)) x, y);
     },
    [](auto &&, auto &&) -> R { c9_assert(0); },
  });
}

template<class T_t> auto tree_value<T_t>::operator->() {
  return (*this)([&](narrow<T_t> auto &tree) {return &static_cast<T_t &>(tree); });
}
template<class T_t> template<class T> bool tree_value<T_t>::is_narrow() {
  return (*this)([]<class TQ>(TQ &) { return narrow<TQ, T>; });
}

template<class Q> constexpr auto tree_value<Q>::notypes() { return size_c<find()>;   }

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
