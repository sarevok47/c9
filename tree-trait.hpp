#pragma once

#include "tree.hpp"

namespace c9 { namespace tree {
type_name make_pointer(auto type) {
  return type_name{{.type = pointer_t{.type = type}}};
};
namespace make_unsigend_detail {
auto operator>(auto signed_, auto unsigned_) { return [=](decltype(signed_)::value_type &) { return unsigned_; }; }
}

auto make_unsigned(auto tree) {
  using namespace make_unsigend_detail;
  return overload {
    char_type_node > unsigned_char_type_node,
    signed_char_type_node > unsigned_char_type_node,
    short_type_node > unsigned_short_type_node,
    int_type_node > unsigned_int_type_node,
    long_type_node > unsigned_long_type_node,
    long_long_type_node > unsigned_long_long_type_node
  }(tree);
}

namespace make_signed_detail {
auto operator>(auto signed_, auto unsigned_) { return [=](decltype(signed_)::value_type &) { return signed_; }; }
}
auto make_signed(auto tree) {
  using namespace make_signed_detail;
  return overload {
    char_type_node > unsigned_char_type_node,
    signed_char_type_node > unsigned_char_type_node,
    short_type_node > unsigned_short_type_node,
    int_type_node > unsigned_int_type_node,
    long_type_node > unsigned_long_type_node,
    long_long_type_node > unsigned_long_long_type_node
  }(tree);
}


}}
