#pragma once

#include "meta.hpp"
#include <vector>


namespace c9 {
template<class T> class flat_set {
  std::vector<T> value;
public:
  auto begin() const { return value.begin(); }
  auto end()   const { return value.end(); }

  void emplace(auto &&v) {
    if(!std::ranges::any_of(value, _ == v))
      value.emplace_back((decltype(v)) v);
  }
  void append(const flat_set<T> &flat_set) {
    for(auto f : flat_set) emplace(f);
  }

  flat_set() = default;
  flat_set(std::initializer_list<T> ilist) : value{ilist} {}
};
}
