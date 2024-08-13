#pragma once

#include "meta.hpp"
#include <algorithm>


namespace c9 {

struct wide_int {
  constexpr static size_t N = 2;
  uint64_t data[N]{};


  wide_int(uint64_t num) : data{num} {}

#define WIDE_INT_BINARY(op) \
  wide_int &operator op##=(wide_int rhs) { \
    make_seq(size_c<N>)([&](auto ...n) { \
      ((data[n()] op ## = rhs.data[n()]), ...); \
    }); \
    return *this; \
  }

  WIDE_INT_BINARY(+) WIDE_INT_BINARY(-)
  WIDE_INT_BINARY(*) WIDE_INT_BINARY(/)
  WIDE_INT_BINARY(&) WIDE_INT_BINARY(^)
  WIDE_INT_BINARY(|) WIDE_INT_BINARY(%)


  bool operator==(wide_int rhs) const { return std::ranges::equal(data, rhs.data); }
};

};
