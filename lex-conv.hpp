#pragma once

#include "token.hpp"
#include "tree.hpp"

namespace c9 { namespace lex {


using integer_suffix = variant_t<""_s, "ll"_s, "l"_s, "L"_s, "u"_s,
                                 "LL"_s, "Ll"_s, "lL"_s, "ul"_s, "Ul"_s, "UL"_s, "lU"_s,
                                 "LLU"_s, "LLu"_s, "llU"_s, "llu"_s, "ULL"_s, "Ull"_s, "uLL"_s,
                                 "ull"_s, "z"_s, "zu"_s>;
struct integer {
  uint64_t value;


  integer_suffix suffix;


  integer &operator=(uint64_t value) { this->value = value; return *this; }
};

using floating_suffix = variant_t<""_s, "f"_s, "l"_s, "L"_s>;
struct floating {
  long double value;

  floating_suffix suffix;
};

struct character {
  uint64_t value;

  variant_t<""_s, "U"_s, "u"_s, "u8"_s, "L"_s, "L"_s> prefix;
};
struct string {
  std::string value;

  variant_t<""_s, "U"_s, "u"_s, "u8"_s, "L"_s> prefix;
};
enum class interpret_status {
  out_of_range = 1,
  exponent_missing,
  invalid_suffix,
  invalid_hex,
};
uint64_t read_escaped_char(auto &&p, interpret_status &is) {
  if ('0' <= *p && *p <= '7') {
    // Read an octal number.
    uint64_t c = *p++ - '0';
    if ('0' <= *p && *p <= '7') {
      c = (c << 3) + (*p++ - '0');
      if ('0' <= *p && *p <= '7')
        c = (c << 3) + (*p++ - '0');
    }
    return c;
  }

  if (*p == 'x') {
    // Read a hexadecimal number.
    p++;
    if (!isxdigit(*p))
      is = interpret_status::invalid_hex;


    uint64_t c = 0;
    for (; isxdigit(*p); p++)
      c = (c << 4) + hexi(*p);
    return c;
  }

  switch (*p++) {
    case 'a': return '\a';
    case 'b': return '\b';
    case 't': return '\t';
    case 'n': return '\n';
    case 'v': return '\v';
    case 'f': return '\f';
    case 'r': return '\r';
    // [GNU] \e for the ASCII escape character is a GNU C extension.
    case 'e': return 27;
    default: return p[-1];
  }
}

static tree::integer_type get_prefix_type(auto prefix) {
  return visit(prefix, overload {
    [&](decltype(""_s))  { return tree::unsigned_char_type_node; },
    [&](decltype("U"_s)) { return tree::unsigned_char_type_node; },
    [&](decltype("u"_s)) { return tree::unsigned_char_type_node; },
    [&](decltype("u8"_s)){ return tree::unsigned_char_type_node; },
    [&](decltype("L"_s)) { return tree::unsigned_char_type_node; },
  });
}
static string interpret_string(string_literal sl, interpret_status &is) {
  string r;
  auto src = sl.begin();
  lex::scan_impl(src, r.prefix, variant_types(r.prefix), 0_c, ""_s);
  ++src;

  size_t type_size = get_prefix_type(r.prefix)->size;
  r.value.resize((sl.size() + 1) * type_size);
  auto dst = r.value.begin().base();

  for(; src != sl.end() - 1; dst += type_size) {
    if(*src == '\\') {
      ++src;
      auto c = read_escaped_char(src, is);
      memcpy(dst, &c, type_size);
    } else
      *dst = *src++;
  }
  return r;
}
/*
 * The numeric value of character constants in preprocessor expressions.
 * The preprocessor and compiler interpret character constants in the same way; i.e. escape sequences such as `\a' are given the values they would have on the target machine.
 * The compiler values a multi-character character constant a character at a time, shifting the previous value left by the number of bits per target character, and then or-ing in the bit-pattern of the new
 * character truncated to the width of a target character. The final bit-pattern is given type int, and is therefore signed, regardless of whether single characters are signed or not (a slight change from
 * versions 3.1 and earlier of GCC). If there are more characters in the constant than would fit in the target int the compiler issues a warning, and the excess leading characters are ignored.
*/
static character interpret_char(char_literal cl) {
  character c;

  auto cur = cl.begin();
  scan_impl(cur, c.prefix, variant_types(c.prefix), 0_c, ""_s);

  c.value = *cur;
  return c;
}
static integer interpret_integer(numeric_constant nc, interpret_status &ok) {
  ok = {};
  const char *p = nc.begin();


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
   ok = interpret_status::invalid_suffix;
  return r;
}

static floating interpret_floating(numeric_constant nc, interpret_status &ok) {
  ok = {};
  const char *p = nc.begin();
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
    ok = interpret_status::invalid_suffix;

  return r;
}
static variant<integer, floating> interpret_nc(numeric_constant nc, interpret_status &ok) {
  if(!nc.floating) return interpret_integer(nc, ok);
  else             return interpret_floating(nc, ok);
}

}}
