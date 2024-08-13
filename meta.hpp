#pragma once
#include "system.hpp"


namespace c9 {

#define BINOPS BINOP(+)  BINOP(-) BINOP(*)  BINOP(%) BINOP(/)  BINOP(==) BINOP(!=) BINOP(<) \
               BINOP(<=) BINOP(>) BINOP(>=) BINOP(&) BINOP(&&) BINOP(|)  BINOP(||) BINOP(^)

template<class F> struct placeholder {
  F f;

  constexpr decltype(auto) operator()(auto x) const { return f(x); }

  constexpr auto op(auto r, auto op) const {
    return ::c9::placeholder{[=, *this](auto x) {
      auto b = f(x);
      if constexpr(requires { r(x); })
        return op(b, r(x));
      else return op(x, r);
    }};
  }

  constexpr auto operator!() const {
    return [=, *this](auto x) {
      return !f(x);
    };
  }
# define BINOP(binop) \
  constexpr auto operator binop(auto r) const {  return op(r, [](auto x, auto y) { return x binop y; });  }
  BINOPS
# undef BINOP
  /*

  constexpr auto operator+(auto r) const {  return op(r, [](auto x, auto y) { return x + y; });  }
  constexpr auto operator-(auto r) const {  return op(r, [](auto x, auto y) { return x - y; });  }
  constexpr auto operator*(auto r) const {  return op(r, [](auto x, auto y) { return x * y; });  }
  constexpr auto operator%(auto r) const {  return op(r, [](auto x, auto y) { return x % y; });  }
  constexpr auto operator/(auto r) const {  return op(r, [](auto x, auto y) { return x / y; });  }
  constexpr auto operator==(auto r) const {  return op(r, [](auto x, auto y) { return x == y; });  }
  constexpr auto operator!=(auto r) const {  return op(r, [](auto x, auto y) { return x != y; });  }
  constexpr auto operator<(auto r) const {  return op(r, [](auto x, auto y) { return x < y; });  }
  constexpr auto operator<=(auto r) const {  return op(r, [](auto x, auto y) { return x <= y; });  }
  constexpr auto operator>(auto r) const {  return op(r, [](auto x, auto y) { return x > y; });  }
  constexpr auto operator>=(auto r) const {  return op(r, [](auto x, auto y) { return x >= y; });  }
  constexpr auto operator&(auto r) const {  return op(r, [](auto x, auto y) { return x & y; });  }
  constexpr auto operator&&(auto r) const {  return op(r, [](auto x, auto y) { return x && y; });  }
  constexpr auto operator|(auto r) const {  return op(r, [](auto x, auto y) { return  x | y; });  }
  constexpr auto operator||(auto r) const {  return op(r, [](auto x, auto y) { return x || y; });  }
  constexpr auto operator^(auto r) const {  return op(r, [](auto x, auto y) { return x ^ y; });  }
*/

  constexpr auto operator*() const { return [=](auto x) { return *x; }; }
  constexpr auto operator++() const { return [=](auto x) { return ++x; }; }
};

placeholder(auto f) -> placeholder<decltype(f)>;

placeholder constexpr inline _ { [](auto &&) { return true; } };

template<auto x> struct constant {
  constexpr static auto operator()() { return x; }
  
  explicit constexpr operator decltype(x)() { return x; }
  
  
  constexpr decltype(auto) operator()(auto&& ...tuple) const;

# define BINOP(binop) \
  constexpr auto operator binop(auto y) const { return constant<(x binop y())>{}; }
  /*
  constexpr auto operator==(auto y) const { return constant<(x == y())>{}; }
  constexpr auto operator!=(auto y) const { return constant<(x != y())>{}; }
  constexpr auto operator<(auto y) const { return constant<(x < y())>{}; }
  constexpr auto operator<=(auto y) const { return constant<(x <= y())>{}; }
  constexpr auto operator>=(auto y) const { return constant<(x >= y())>{}; }
  constexpr auto operator>(auto y) const { return constant<(x > y())>{}; }
  constexpr auto operator||(auto y) const { return constant<(x || y())>{}; }
  constexpr auto operator|(auto y) const { return constant<(x | y())>{}; }
  constexpr auto operator&&(auto y) const { return constant<(x && y())>{}; }
  constexpr auto operator&(auto y) const { return constant<(x & y())>{}; }
  constexpr auto operator^(auto y) const { return constant<(x ^ y())>{}; }
  constexpr auto operator+(auto y) const { return constant<(x + y())>{}; }
  constexpr auto operator-(auto y) const { return constant<(x - y())>{}; }
  constexpr auto operator*(auto y) const { return constant<(x * y())>{}; }
  constexpr auto operator/(auto y) const { return constant<(x / y())>{}; }
  constexpr auto operator<<(auto y) const { return constant<(x << y())>{}; }
  constexpr auto operator>>(auto y) const { return constant<(x >> y())>{}; }
  */
  BINOPS
# undef BINOPS
  
  constexpr auto operator!() const { return constant<!x>{}; }
  
  template<auto y> constexpr operator constant<y>() requires (x == y) {  return constant<y>{};  }

  struct times_t {
    constexpr void operator()(auto &&f) const;
    constexpr static void with_idx(auto &&f);
  } constexpr static times{};
};


using true_t = constant<true>;
using false_t = constant<false>;

constexpr inline constant<true> true_{};
constexpr inline constant<false> false_{};

template<size_t idx> constexpr inline constant<idx> size_c{};


template<char ...c> constexpr auto operator""_c() {
  return constant<(int64_t) []{
    char arr[] = {c...};

    size_t base = 10;
    size_t i = 0;

    if(std::size(arr) && arr[0] == '0') {
      ++i;
      base = 8;
      if(1 < std::size(arr)) {
        switch(arr[1]) {
          case 'b': base = 2; ++i; break;
          case 'x': base = 16; ++i; break;
        }
      }
    }



    double num{};
    double mul = 0.1;
    for(; i != std::size(arr); ++i)
      if(arr[i] != '\'') {
        num += (arr[i] - '0') * mul;
        num *= base;
       }

    return num;
  }()>{};
}



template<class T> struct type_ {
  using type = T;
  
  T operator*() const;
  __remove_cvref(T) operator+() const;
  

  constexpr auto operator==(auto type) const { return constant<__is_same(type_, decltype(type))>{}; }
  constexpr auto operator!=(auto type) const { return constant<!__is_same(type_, decltype(type))>{}; }
};


template<class T> constexpr type_<T> type_c{};
template<class T> using type_t = typename type_<T>::type;

template<class T> constexpr auto typeof_(T) { return type_c<T>; }


#if __has_builtin(__type_pack_element)
# define type_pack_element __type_pack_element
#else
template <size_t N, typename = std::make_index_sequence<N>> struct type_pack_element_impl;

template <size_t N, size_t ...ignore>
struct type_pack_element_impl<N, std::index_sequence<ignore...>> {
  template <typename T> T operator()(decltype((void *) ignore)..., T *, ...);
};

template<size_t N, class ...T> using type_pack_element = decltype(*type_pack_element_impl<N>{}(((type_<T> *) 0)...));
#endif
  
  
constexpr auto tuple(auto ...x) {
  return [=](auto &&f) -> decltype(auto) { return f(x...); };
}
constexpr auto ref_tuple(auto& ...x) {
  return [&](auto &&f) -> decltype(auto) { return f(x...); };
}


constexpr auto size = [](auto&& ...x) {
  return constant<sizeof...(x)>{};
};
constexpr auto len = [](auto&& ...x) {
  return sizeof...(x);
};

constexpr auto for_each(auto&& f) {
  return [&](auto&& ...x) {
    ((f(x)), ...);
  };
}

constexpr auto contains(auto value) {
  return [=](auto&& ...x) {
    return ((x == value) || ...);
  };
}
constexpr auto contains_if(auto &&pred) {
  return [&](auto&& ...x) {
    return ((pred(x)) || ...);
  };
}


template<class T, T ...c> struct sequence {
  constexpr static T data[sizeof...(c)] = {c...};
  
  constexpr decltype(auto) operator()(auto &&f) const {
    return f(constant<c>{}...);
  }
  
  constexpr static auto size() { return size_c<sizeof...(c)>; }
  
  template<auto i> constexpr auto operator[](constant<i>) const { return constant<data[i]>{}; }
  
  template<T ...x> constexpr auto operator+(sequence<T, x...>) const { return sequence<T, c..., x...>{}; }

  template<T ...x> constexpr auto operator==(sequence<T, x...> s) const { return constant<__is_same(sequence, decltype(s))>{}; }
};



template<char ...c> struct string_seq {
  constexpr static char data[] = {c..., '\0'};
  
  template<char ...cc>
  constexpr bool operator==(string_seq<cc...> s) const { return __is_same(string_seq, decltype(s)); }
  
  template<auto i> constexpr  auto operator[](constant<i>) const { return constant<data[i]>{}; }

  template<char ...cc>
  constexpr string_seq<c..., cc...> operator+(string_seq<cc...>) { return {}; }
  
  template<class T, T ...n>
  constexpr auto sub(sequence<T, n...>) { return string_seq<data[n]...>{}; }
  

  constexpr auto operator()(auto f) requires requires { f(c...); } { return f(c...); }
   
  constexpr static const char *c_str() { return data; }

// operator sv() const { return data; }
};


template<class T, T ...c> constexpr string_seq<c...> operator""_s() { return {}; }

constexpr auto make_seq(auto c) {
  using T = decltype(c());
#if __has_builtin(__make_integer_seq)
  return __make_integer_seq<sequence, T, c()>{};
#else
  return sequence<T, __integer_pack(c())...>{};
#endif
}

namespace detail {
struct any_casted { any_casted() = default; any_casted(auto &&) {} };
}

template<auto x> constexpr decltype(auto) constant<x>::operator()(auto&& ...tuple) const {
  return make_seq(*this)([&](auto ...idx) -> decltype(auto) {
    return [](decltype(idx, detail::any_casted{})..., auto &&r, ...) -> decltype(auto) {
      return r;
    }(tuple...);
  });
}

template<auto x> constexpr void constant<x>::times_t::operator()(auto &&f) const {
  return (make_seq(*this))
           (for_each([&](auto) { f(); }));
}
template<auto x> constexpr void constant<x>::times_t::with_idx(auto &&f) {
  return (make_seq(constant{}))
           (for_each([&](auto idx) { f(idx); }));
}




static struct iter_r {} iter_range;

template<class T> struct iter_r_p : T {
  T &operator*() { return *this; }
};


constexpr auto operator|(auto &&r, struct iter_r) {
  struct {
    iter_r_p<decltype(r.begin())> begin_, end_;

    auto begin() { return begin_; }
    auto end() { return end_; }
  } out{r.begin(), r.end()};

  return out;
}

constexpr decltype(auto) if_(auto expr, auto true_, auto false_) {
  if constexpr(expr())
    return true_;
  else
    return false_;
}

template<class T> auto merge() {
  return range<T>{{}, {}};
}

constexpr auto merge(auto x) {
  return x;
}
constexpr auto merge(auto x, auto y, auto ...rest) {
  return x([=](auto ...x) {
    return y([=](auto ...y) {
      return merge(tuple(x..., y...), rest...);
    });
  });
}

#define __remove_cvref(T) std::remove_cvref_t<T>
template<class Derived, class Base> concept derived_from = std::is_base_of_v<__remove_cvref(Base), __remove_cvref(Derived)>
     && !__is_same(__remove_cvref(Derived), __remove_cvref(Base));
template<class T, class U> concept narrow = __is_same(__remove_cvref(T), __remove_cvref(U)) || derived_from<T, U>;
#undef __remove_cvref

template<class T> class shared_ptr {
  struct nl {
    size_t count;
    T data;
  } *n{};
public:
  shared_ptr(T V) {
    n = (nl *) malloc(sizeof(nl));
    n->count = 1;
    new(&n->data) auto{mov(V)};
  }
  shared_ptr(const shared_ptr &sp) {
    n = sp.n;
    n->count++;
  }
  shared_ptr( shared_ptr &&sp) {
    n = sp.n;
    sp.n = nullptr;
  }
  shared_ptr() : n{} {}
  auto &operator=(shared_ptr s) {
    n = s.n;
    s.n = nullptr;
    return *this;
  }
  ~shared_ptr() {
    if(n ) {
    --n->count;
    if(!n->count)
      free(n);
    }
  }
  operator bool() { return n; }
  T &operator*() { return n->data; }
  T *operator->() { return &n->data; }
};




}
