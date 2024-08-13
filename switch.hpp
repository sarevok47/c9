#pragma once

#include "meta.hpp"
#include <optional>

namespace c9 {


template<class T> struct case_switch {
  T value;
  bool matched = false;

  case_switch &operator()(auto &&pred, auto &&f) {
    if(!matched && value == pred) {
      matched = true;
      f();
    }
    return *this;
  }

  void default_(auto &&f) { if(!matched) f(); }

};


}
