#pragma once

#include <string_view>
#include <filesystem>
#include <cassert>
#include <cstring>
#include <climits>
#include <cstdint>
#include <optional>


#ifdef NDEBUG
#define c9_assert(expr) ({ if(!(expr)) __builtin_unreachable(); })
#else
#define c9_assert(expr) ({ assert(expr); })
#endif
  
#if __has_attribute(always_inline)
#define ALWAYS_INLINE __always_inline
#else
#define ALWAYS_INLINE inline
#endif


namespace c9 {

namespace fs = std::filesystem;
namespace rv = std::ranges::views;

using namespace std::string_view_literals;
using namespace std::string_literals;

using sv = std::string_view;
template<class T> using opt = std::optional<T>;
template<class T> using refw = std::reference_wrapper<T>;


template<class T> decltype(auto) mov(T &&value) { return (T &&) value; }

template<class ...T> struct overload : T... { using T::operator()...; }; 


template<class T> struct range {
  T *first, *last;
  auto begin() { return first; }
  auto end() { return last; }
};

constexpr bool is_digit(char c) { 
  return c >= '0' && c <= '9';
}
constexpr bool is_space(char c) {
  return c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\v' || c == '\f';
}
constexpr bool is_alpha(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}
constexpr bool is_id(char c) {
  return is_alpha(c) || c == '_' || c == '$';
}
constexpr bool is_idnum(char c) {
  return is_id(c) || is_digit(c);
}
constexpr bool is_hex(char c) {
  return is_digit(c) || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
}
constexpr bool is_octal(char c) {
  return c >= '0' && c <= '7';
}

constexpr size_t hexi(char c) {
  switch(c) {
    case '0' ... '9': return c - '0';
    case 'a' ... 'f': return c - 'a' + 10;
    case 'A' ... 'Z': return c - 'A' + 10;
    default: 
      c9_assert(0);
  }
}
}
