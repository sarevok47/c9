#pragma once

#include "system.hpp"
#include "meta.hpp"
#include <nmmintrin.h>
namespace c9 {



static uint32_t decode_utf8(auto &it, auto end) {
  auto f = [&](auto len) {
    return (make_seq(len - 1_c))([&](auto ...x) {
      return (((*it & ((1 << (8 - len())) - 1)) << ((len() - 1) * 6))) | ((((({*++it;})) & 0x3F) << x() * 6) | ...);
    });
  };

  uint32_t c;
  if(it != end && (*it & 0x80) == 0) [[likely]]
    c = *it;
  else if(it + 2 <= end && (*it & 0xE0) == 0xC0)
    c = f(2_c);
  else if(it + 3 <= end && (*it & 0xF0) == 0xE0)
    c = f(3_c);
  else if(it + 4 <= end && (*it & 0xF8) == 0xF0)
    c = f(4_c);
  else
    c9_assert(0);

  ++it;
  return c;
}

static auto encode_utf8(uint32_t c, auto out){
  if(c <= 0x7F) [[likely]]
    *out = c;
  else if (c <= 0x7FF) {
    *out = 0b11000000 | (c >> 6);
    *++out = 0b10000000 | (c & 0b00111111);
  } else if (c <= 0xFFFF) {
    *out = 0b11100000 | (c >> 12);
    *++out = 0b10000000 | ((c >> 6) & 0b00111111);
    *++out = 0b10000000 | (c & 0b00111111);
  } else {
    *out = 0b11110000 | (c >> 18);
    *++out = 0b10000000 | ((c >> 12) & 0b00111111);
    *++out = 0b10000000 | ((c >> 6) & 0b00111111);
    *++out = 0b10000000 | (c & 0b00111111);
  }

  return out;
}

static size_t utf8_distance(auto start, auto finish) {
  size_t i = 0;
  for(; start != finish; ++i) 
    decode_utf8(start, finish);
  
  return i;
}

ALWAYS_INLINE bool in_urange(auto &&tab, uint32_t c) {
  for(auto cc : tab)
    if(c >= cc.first && c <= cc.second)
      return true;
  return false;
}


static inline bool is_extd_start(uint32_t c) {
  constexpr static std::pair<uint32_t, uint32_t> tab[] = {
    {'a', 'z'}, {'A', 'Z'}, {'$', '$'}, {'_', '_'},
    {0x00A8, 0x00A8},{ 0x00AA, 0x00AA},{ 0x00AD, 0x00AD}, { 0x00AF, 0x00AF},
    {0x00B2, 0x00B5},{ 0x00B7, 0x00BA},{ 0x00BC, 0x00BE}, { 0x00C0, 0x00D6},
    {0x00D8, 0x00F6}, {0x00F8, 0x00FF},{ 0x0100, 0x02FF}, {0x0370, 0x167F},
    {0x1681, 0x180D},{ 0x180F, 0x1DBF},{ 0x1E00, 0x1FFF},{0x200B, 0x200D},
    {0x202A, 0x202E},{ 0x203F, 0x2040},{ 0x2054, 0x2054},{0x2060, 0x206F},
    {0x2070, 0x20CF},{ 0x2100, 0x218F},{ 0x2460, 0x24FF},{ 0x2776, 0x2793},
    {0x2C00, 0x2DFF},{ 0x2E80, 0x2FFF},{ 0x3004, 0x3007},{ 0x3021, 0x302F},
    {0x3031, 0x303F},{ 0x3040, 0xD7FF},{ 0xF900, 0xFD3D},{ 0xFD40, 0xFDCF},
    {0xFDF0, 0xFE1F},{ 0xFE30, 0xFE44},{ 0xFE47, 0xFFFD},
    {0x10000, 0x1FFFD},{ 0x20000, 0x2FFFD},{ 0x30000, 0x3FFFD},{ 0x40000, 0x4FFFD},
    {0x50000, 0x5FFFD},{ 0x60000, 0x6FFFD},{0x70000, 0x7FFFD},{ 0x80000, 0x8FFFD},
    {0x90000, 0x9FFFD},{ 0xA0000, 0xAFFFD},{ 0xB0000, 0xBFFFD},{ 0xC0000, 0xCFFFD},
    {0xD0000, 0xDFFFD},{ 0xE0000, 0xEFFFD}
  };

  return in_urange(tab, c);
}
static inline bool is_extd_cont(uint32_t c) {
  constexpr static std::pair<uint32_t, uint32_t> tab[] = {
    { 0x0300, 0x036F},{ 0x1DC0, 0x1DFF},{ 0x20D0, 0x20FF}, {0xFE20, 0xFE2F},
    {'0', '9'}
  };

  return is_extd_start(c) || in_urange(tab, c);
}


ALWAYS_INLINE bool consume_utf8(auto &in, auto out, auto f, auto f_1) {
  if(f(*in)) [[likely]]  {
    *++out = *in;
    ++in;
    return true;
  } else if(*in < 0xff) {
    return false;
  } else {
    c9_assert(0);
  }

}

ALWAYS_INLINE bool validate_forms(auto &&p, auto f, auto f_1) {
  if(f(*p)) [[likely]]
    return ++p, true;
  /*else if(*p < 0xff)
    return false;*/
  else {
    auto s = p;
    uint32_t c = decode_utf8(s, s + 4);
    //fprintln(stderr, "success {} is {}", c, f_1(c));
    return f_1(c)? p = s, true : false;
  }
}

static inline bool consume_start(auto &in, auto out) {
  return consume_utf8(in, out, is_id, is_extd_start);
}
static inline bool consume_cont(auto &&in, auto out) {
  return consume_utf8(in, out, is_idnum, is_extd_cont);
}


static inline bool validate_start(auto &&p) {
  return validate_forms(p, is_id, is_extd_start);
}
static inline bool validate_cont(auto &&p) {
  return validate_forms(p, is_idnum, is_extd_cont);
}


}

