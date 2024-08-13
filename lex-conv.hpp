#pragma once

#include "token.hpp"


namespace c9 { namespace lex {


using udl_string = sv;
using integer_suffix = variant_t<""_s, "ll"_s, "l"_s, "L"_s, "u"_s,
                                 "LL"_s, "Ll"_s, "lL"_s, "ul"_s, "Ul"_s, "UL"_s, "lU"_s,
                                 "LLU"_s, "LLu"_s, "llU"_s, "llu"_s, "ULL"_s, "Ull"_s, "uLL"_s,
                                 "ull"_s, "z"_s, "zu"_s>;
struct integer {
  uint64_t value;


  variant<integer_suffix, udl_string> suffix;


  integer &operator=(uint64_t value) { this->value = value; return *this; }
};

using floating_suffix = variant_t<"f"_s, "l"_s, "L"_s, "f16"_s, "F16"_s, "f32"_s, "F32"_s, "f64"_s, "F64"_s, "f128"_s, "F128"_s, "bf16"_s, "BF16"_s>;
struct floating {
  long double value;

  variant<floating_suffix, udl_string> suffix;
};

struct character {
  uint64_t value;

  variant_t<""_s, "U"_s, "u"_s, "u8"_s, "L"_s, "L"_s> prefix;
};
/*
 * The numeric value of character constants in preprocessor expressions.
 * The preprocessor and compiler interpret character constants in the same way; i.e. escape sequences such as `\a' are given the values they would have on the target machine.
 * The compiler values a multi-character character constant a character at a time, shifting the previous value left by the number of bits per target character, and then or-ing in the bit-pattern of the new
 * character truncated to the width of a target character. The final bit-pattern is given type int, and is therefore signed, regardless of whether single characters are signed or not (a slight change from
 * versions 3.1 and earlier of GCC). If there are more characters in the constant than would fit in the target int the compiler issues a warning, and the excess leading characters are ignored.
*/
character interpret_char(char_literal cl) {
  character c;

  auto cur = cl.begin();
  scan_impl(cur, c.prefix, variant_types(c.prefix), 0_c, ""_s);

  c.value = *cur;
  return c;
}

enum class interpret_status {
  out_of_range = 1,
  exponent_missing
};


integer interpret_integer(numeric_constant nc, interpret_status &ok) {
  auto p = nc.begin();


  integer r{};


  size_t base = 10;

  if(*p == '0' && p + 1 != nc.end()) {
    base = 8;
    switch(*++p) {
      case 'b': base = 2; ++p; break;
      case 'x': base = 16; ++p; break;
    }
  }


  auto [suffix, ec] = std::from_chars(p, nc.end(), r.value, base);

  if(ec == std::errc::result_out_of_range)
    ok = interpret_status::out_of_range;



  integer_suffix isuffix;
  scan_impl(p = suffix, nc.end(), isuffix, variant_types(isuffix), 0_c, ""_s);

  if(p == nc.end())
    r.suffix = isuffix;
  else
    r.suffix = udl_string{suffix, nc.end()};
  return r;
}

floating interpret_floating(numeric_constant nc, interpret_status &ok) {
  auto p = nc.begin();
  bool hex = *p == '0' && p + 1 != nc.end() && (p[1] == 'x' || p[1] == 'X');


  if(hex && nc.exponent)
    ok = interpret_status::exponent_missing;

  floating r{};

  auto [suffix, ec] = std::from_chars(p, nc.end(), r.value);

  floating_suffix fsuffix;
  scan_impl(p = suffix, nc.end(), fsuffix, variant_types(fsuffix), 0_c, ""_s);

  if(p == nc.end())
    r.suffix = fsuffix;
  else
    r.suffix = udl_string{suffix, p};

  return r;
}
variant<integer, floating> interpret_nc(numeric_constant nc, interpret_status &ok) {
  if(!nc.floating) return interpret_integer(nc, ok);
  else             return interpret_floating(nc, ok);
}

}}
