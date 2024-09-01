#pragma once

#include "../target.hpp"

namespace c9 {
struct x86_target : target {
  virtual simple::rtype tree_type_simplify(tree::builtin_type b) override {
    using namespace tree; using namespace simple;
    return b(overload {
      [](void_type_t)                  { return rtype::none; },

      [](signed_char_type_t)           { return rtype::i8;   },
      [](unsigned_char_type_t)         { return rtype::i8;   },
      [](char_type_t)                  { return rtype::i8;   },
      [](short_type_t)                 { return rtype::i16;  },
      [](unsigned_short_type_t)        { return rtype::i16;  },
      [](int_type_t)                   { return rtype::i32;  },
      [](unsigned_int_type_t)          { return rtype::i32;  },
      [](long_type_t)                  { return rtype::i64;  },
      [](unsigned_long_type_t)         { return rtype::i64;  },
      [](long_long_type_t)             { return rtype::i64;  },
      [](unsigned_long_long_type_t)    { return rtype::i64;  },

      [](float_type_t)                 { return rtype::f32;  },
      [](double_type_t)                { return rtype::f64;  },
      [](long_double_type_t)           { return rtype::f128; },

      [](builtin_type_t) -> rtype { c9_assert(0); }
    });
  }

  x86_target() {
    ptrdiff_type_node = tree::long_type_node;
    size_type_node = tree::unsigned_long_type_node;
    predefined_macro = R""(
            #define __SIZEOF_DOUBLE__ 8
            #define __SIZEOF_FLOAT__ 4
            #define __SIZEOF_INT__ 4
            #define __SIZEOF_LONG_DOUBLE__ 8
            #define __SIZEOF_LONG_LONG__ 8
            #define __SIZEOF_LONG__ 8
            #define __SIZEOF_POINTER__ 8
            #define __SIZEOF_PTRDIFF_T__ 8
            #define __SIZEOF_SHORT__ 2
            #define __SIZEOF_SIZE_T__ 8
            #define __SIZE_TYPE__ unsigned long int
            #define __x86_64 1
            #define __x86_64__ 1
            #define __CHAR_BIT__ 8
            #define __amd64 1
            #define __amd64__ 1
    )"";
  }
};
}
