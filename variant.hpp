#pragma once

#include "meta-macro.hpp"
#include "meta.hpp"
#include <algorithm>
//#include <boost/preprocessor.hpp>


#define __is_trivially_constructible(T, X) std::is_trivially_constructible_v<T,X>
#define __is_trivially_assignable(T, X) std::is_trivially_constructible_v<T,X>
#define __has_trivial_destructor(T) std::is_trivially_destructible_v<T>

namespace c9 {
decltype(auto) visit(auto &&v, auto &&f);

template<class ...T> class variant {
  constexpr static size_t max_idx = [] {
    size_t arr[] = {sizeof(T)...};
    return std::max_element(arr, arr + sizeof...(T)) - arr;
  }();

  using max_type = type_pack_element<max_idx, T...>;


  uint8_t index_ = 0;
  alignas(max_type) char storage[sizeof(max_type)];
public:
  variant() { new(&storage) type_pack_element<0, T...>{}; }
  variant(const variant &v) requires (__is_trivially_constructible(T, T)  && ... ) = default;
  variant(variant &&) requires (__is_trivially_constructible(T, T)  && ... ) = default;

  constexpr static auto notypes() { return size_c<sizeof...(T)>; }


  variant(const variant &v) {
    visit(v, [&](const auto &s) {
      index_ = indexof<decltype(s)>();
      new(&storage) auto{s};
    });
  }
  variant(variant &&v) {
    visit(mov(v), [&](auto s) {
      index_ = indexof<decltype(s)>();
      new(&storage) auto{mov(s)};
    });
  }

  size_t index() const { return index_; }

  template<class U> consteval static size_t find_idx(type_<U> = {}) {
    bool b[] = { __is_same(__remove_cvref(T), __remove_cvref(U))... };

    return std::find(b, std::end(b), true) - b;
  }

  template<class U> constexpr static size_t indexof() requires (find_idx<U>() != sizeof...(T)) {
    return find_idx<U>();
  }
  constexpr static size_t indexof(auto x) requires (find_idx<decltype(x)>() != sizeof...(T)) {
    return find_idx<decltype(x)>();
  }

  template<class U>
  variant(U &&x) requires requires { indexof(x); } {
    index_ = indexof<U>();
    new(&storage) __remove_cvref(U){(decltype(x)) x};
  }

  decltype(auto) operator[](auto idx) {
    c9_assert(idx() == index());
    return *std::launder((type_pack_element<idx(), T...> *) storage);
  }
  decltype(auto) operator[](auto idx) const {
    c9_assert(idx() == index());
    return *std::launder((type_pack_element<idx(), T...> *) storage);
  }



  variant &operator=(const variant &) requires (__is_trivially_assignable(T, T) && ...) = default ;
  variant &operator=(variant &&) requires (__is_trivially_assignable(T, T) && ...) = default;

  variant &operator=(const variant &v) {
    visit(v, [&](const auto &x) { operator=(x); });

    return *this;
  }
  variant &operator=(variant &&v) {
    visit(v, [&](auto &&x) { operator=(mov(x));  });
    v.index_ = 0;
    new(&v.storage) type_pack_element<0, T...>{};

    return *this;
  }

  variant &operator=(auto &&x) requires requires { indexof(x); } {
    if constexpr((__is_pod(T) && ... ))
      new(storage) auto{x};
    else {
      if(index_ == indexof(x)) {
        (__remove_cvref(decltype(x)) &) *this = (decltype(x)) x;
      } else {
        this->~variant();
        new(this) variant{(decltype(x)) x};
      }
    }
    index_ = indexof(x);
    return *this;
  }


  template<class U> ALWAYS_INLINE operator /*const FIXME ERROR*/ U &() const noexcept requires requires { indexof<U>(); } {
    c9_assert(indexof<U>() == index_);
    return *std::launder((U *) storage);
  }
  template<class U> ALWAYS_INLINE explicit operator U() const noexcept {
    c9_assert(indexof<U>() == index_);
    return *std::launder((U *) storage);
  }

  template<class U> ALWAYS_INLINE operator U &() noexcept requires requires { indexof<U>(); } {
    c9_assert(indexof<U>() == index_);
    return *std::launder((U *) storage);
  }

  bool operator==(auto x) {
    return index_ == indexof(x) && (decltype(x) &) *this == x;
  }

  template<class U> bool is() { return index() == indexof<U>(); }

  ~variant() requires (__has_trivial_destructor(T) && ...) = default;
  ~variant() {
    visit(*this, []<class V>(V &x) { std::destroy_at(std::addressof(x)); });
  }
};

decltype(auto) visit_or(auto &&v, auto value, auto &&f) {
  return visit(v, overload {
    f,
    [&](auto &&) -> decltype(auto) { return (decltype(value)) value; }
  });
}


ALWAYS_INLINE decltype(auto) visit(auto &&v, auto &&f) {
  using F = decltype(f(v[0_c]))(*)(decltype(v), decltype(f));
#define VISIT_CASE(N, D) \
        case N: {    \
          if constexpr(N < decltype(v.notypes()){}()) \
            return f(v[N ## _c])  ;                                                                               \
        }                                                                                           \

    switch(v.index())
    {
      REPEAT(256, VISIT_CASE, )

      default:
        c9_assert(0);
    }
}

ALWAYS_INLINE decltype(auto) visit(auto &&v1, auto &&v2, auto &&f) {
#if 0

#undef _VISIT_CASE
#undef _VISIT_CASE_CNT

    #define _VISIT_CASE_CNT 200
#define _VISIT_CASE(N, D) \
      case N:  \
  if constexpr(N < decltype(v1.notypes()){}()) { \
        switch(v2.index()) { \
       REPEAT_1( \
            _VISIT_CASE_CNT, \
            _VISIT_CASE_1, N) \
        } \
  }

#define _VISIT_CASE_1(N2, N) \
        case N2:   \
              \
          if constexpr(N2 < decltype(v2.notypes()){}())  \
            return f(v1[N ## _c], v2[N2##_c]) ;                                                                        \


  switch(v1.index())
    {
        REPEAT(
            _VISIT_CASE_CNT,
            _VISIT_CASE, _)
    }


#else
  constexpr static auto tab = [] {
    using F = decltype(f(v1[0_c], v2[0_c]))(*)(decltype(v1), decltype(v2), decltype(f));

    constexpr decltype(v1.notypes()) sz1{};
    constexpr decltype(v2.notypes()) sz2{};

    std::array<std::array<F, sz2()>, sz1()> tab;

    (make_seq(sz1)([&](auto ...x) {
      (make_seq(sz2)([&](auto ...y) {
        auto xx = x;
        ((tab[xx()][y()] = [](auto v1, auto v2, auto f) {
          return f(v1[decltype(xx){}], v2[decltype(y){}]);
        }), ...);
      }), ...);
    }));

    return tab;
  }();

  return tab[v1.index()][v2.index()]((decltype(v1)) v1, (decltype(v2)) v2, (decltype(f)) f);
#endif
}


template<class T> constexpr bool is(auto &&v) {
  return v.index() == v.template indexof<T>();
}



template<class ...T> constexpr auto variant_types(variant<T...> v) {
  return tuple(T{}...);
}

template<auto ...x> using variant_t = variant<decltype(x)...>;

}
#undef __is_trivially_constructible(T, T) std::is_trivially_constructible_v<T,T>
#undef __is_trivially_assignable(T, T) std::is_trivially_constructible_v<T,T>
#undef __has_trivial_destructor(T) std::has_trivial_destructor_v<T>
