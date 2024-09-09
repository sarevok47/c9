#pragma once

#include "tree.hpp"

namespace c9 { namespace cfg { struct basic_block; } namespace simple {
enum class rtype : uint8_t { none, ptr, i8, i16, i32, i64, i128, f32, f64, f128 };

using int_cst   = __uint128_t;
using float_cst = long double;


struct temporary { size_t i; };
using op = variant<int_cst, float_cst, temporary>;


template<auto str> struct binary{ rtype type; op src1, src2, dst; };
template<auto str> struct unary { rtype type; op src1, dst; };


#define BININSN(name, op) using name = binary<#op##_s>

BININSN(add, +);
BININSN(sub, -);
BININSN(div, /);
BININSN(mod, %);
BININSN(mul, *);;
BININSN(lshift, <<);
BININSN(rshift, >>);
BININSN(bit_or, |);
BININSN(bit_and, &);
BININSN(bit_xor, ^);
BININSN(cmp, ==);
BININSN(neg_cmp, !=);
BININSN(less, <);
BININSN(less_eq, <=);
BININSN(greater, >);
BININSN(greater_eq, >=);

#define UNARYINSN(name, op) using name = unary<#op##_s>;


UNARYINSN(bang, !);
UNARYINSN(positive, +);
UNARYINSN(neg, -);
UNARYINSN(reverse, ~);

struct assign { op src1, dst; };
struct load   { tree::variable src1; op dst; };
struct store  { op src1; tree::variable dst; };
struct deref { op src1, dst; rtype type; };
struct jump   { struct cfg::basic_block &target; };
struct br     { op cond; cfg::basic_block &true_, &false_; };


using insn = variant<add, sub, div, mod, mul, lshift, rshift, bit_or, bit_and, bit_xor,
                     cmp, neg_cmp, less, less_eq, greater, greater_eq,

                     bang, positive, neg, reverse,
                     assign, load, store, jump, deref, br>;

constexpr static int_cst true_{true};
struct dumper {
  FILE *out;
private:
  void begin() { fprint(out, "\t"); }
  void end()   { fprintln(out, ";"); }

  template<class ...T> void dump(variant<T...> v) { visit(v, [&](auto v) { dump(v); }); }


  void dump(int_cst int_)       { fprint(out, "{}", int_);          }
  void dump(float_cst float_)   { fprint(out, "{}", float_);        }
  void dump(tree::variable var) { fprint(out, "{}", var->name);     }
  void dump(temporary tmp)      { fprint(out, "tmp_{}", tmp.i); }

  template<auto str> void dump(binary<str> binary) {
    begin();
    dump(binary.dst);
    fprint(out, " = ");
    dump(binary.src1);
    fprint(out, " {} ", str.c_str());
    dump(binary.src2);
    end();
  }
  template<auto str> void dump(unary<str> unary) {
    begin();
    dump(unary.dst);
    fprint(out, " = ");
    fprint(out, "{}", str.c_str());
    dump(unary.src1);
    end();
  }
  void dump(assign assign) {
    begin();
    dump(assign.dst);
    fprint(out, " = ");
    dump(assign.src1);
    end();
  }
  void dump(load load) {
    begin();
    dump(load.dst);
    fprint(out, " = ");
    dump(load.src1);
    end();
  }
  void dump(store store) {
    begin();
    dump(store.dst);
    fprint(out, " = ");
    dump(store.src1);
    end();
  }
  void dump(deref deref) {
    begin();
    dump(deref.dst);
    fprint(out, " = *");
    dump(deref.src1);
    end();
  }
  void dump(jump);
  void dump(br);
public:
  void operator()(auto v) { dump(v); }
};
}}
#undef BININSN
