#pragma once

#include "system.hpp"
#include "tree.hpp"
#include "simple.hpp"

namespace c9 {
struct target {
  sv name;

  tree::integer_type ptrdiff_type_node, size_type_node;

  sv predefined_macro;

  virtual simple::rtype tree_type_simplify(tree::builtin_type) = 0;
};
}
