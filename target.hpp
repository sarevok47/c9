#pragma once

#include "system.hpp"
#include "tree.hpp"
#include "simple.hpp"

namespace c9 {
struct target {
  sv name;

  tree::integer_type ptrdiff_type_node, size_type_node;

  sv predefined_macro;

  virtual size_t mark_arg_regs(size_t regnum, bool xmm, tree::function_call fcall)  = 0;
  virtual tree::pointer make_ptr(tree::type_decl type) = 0;
};
}
