#pragma once

#include "system.hpp"
#include "tree.hpp"
#include "simple.hpp"

namespace c9 {
struct target {
  sv name;

  virtual simple::rtype tree_type_simplify(tree::builtin_type) = 0;
};
}
