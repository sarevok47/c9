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
};
}
