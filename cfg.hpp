#pragma once

#include <vector>
#include "tree.hpp"
#include "simple.hpp"


namespace c9 { namespace cfg {
struct basic_block {
  size_t i{};
  std::vector<simple::insn> insns;

  std::vector<basic_block *> preds, succs;

  basic_block &push(basic_block bb) { return *(next = new auto{mov(bb)}); }
  template<class T> void add_insn(T insn) { insns.emplace_back(insn); }

  basic_block *step() { return next; }

  void jump(basic_block &target)                                  { add_insn(simple::jump{target}); }
  void br(auto cond, basic_block &true_bb, basic_block &false_bb) { add_insn(simple::br{cond, true_bb, false_bb}); }

  basic_block() = default;
  basic_block(size_t i, std::same_as<basic_block *> auto ...preds) : i{i}, preds{preds...} {}
private:
  basic_block *next{};
};


class cfg {public:
  size_t nlabel = 1, ntmp{};
  basic_block entry, *last_bb = &entry;

  simple::temporary make_tmp() { return {ntmp++}; }

  basic_block &create_bb(auto ...preds) {
    last_bb = &last_bb->push({nlabel++, preds...});
    ((preds->succs.emplace_back(last_bb)), ...);
    return *last_bb;
  }



  simple::op construct(tree::expression expr) {
    return expr(overload {
      [](auto &) -> simple::op { },
      [&](tree::variable_t &) -> simple::op { return tree::variable(expr); },
      [&](tree::unary_expression_t &unary) {
        return visit(unary.op, overload {
          [&](decltype("&"_s)) -> simple::op {

          },
          [&](decltype("*"_s)) -> simple::op {

          },
          [](decltype("++"_s)) -> simple::op {
          },
          [](decltype("--"_s)) -> simple::op  {

          },
          [&]<class S>(S) {
            simple::unary<S{}> insn {
              .src1 = construct(unary.expr),
              .dst = make_tmp()
            };
            last_bb->add_insn(insn);
            return insn.dst;;
          }
        });
      },
      [&](tree::binary_expression_t &expr) {
        return visit(expr.op, overload {
          [&]<char _1, char _2>(string_seq<_1, _2> s) -> simple::op requires (s == "&&"_s || s == "||"_s) {
            auto lhs = construct(expr.lhs);
            auto result = make_tmp();
            last_bb->add_insn<simple::assign>({lhs, result});
            auto bb = last_bb;

            auto &rhs_bb = create_bb(last_bb);
            last_bb->add_insn<simple::binary<string_seq<_1>{}>>({
              .src1 = lhs,
              .src2 = construct(expr.rhs),
              .dst =  result
            });
            create_bb(last_bb);
            bb->br(lhs, rhs_bb, *last_bb);
            return result;
          },
          [&]<class S>(S) -> simple::op {
            simple::binary<S{}> insn{
              .src1 = construct(expr.lhs),
              .src2 = construct(expr.rhs),
              .dst = make_tmp()
            };
            last_bb->add_insn(insn);
            return insn.dst;
          }
        });
      }
    });
  }

  void construct(tree::statement stmt) {
    stmt(overload {
      [](auto &) {},
      [&](tree::if_statement_t &if_) {
        auto cond = construct(if_.cond);

        auto pre_if = last_bb; // insert conditional jump here later
        auto &if_start = create_bb(pre_if);
        construct(if_.if_stmt);

        auto if_end = last_bb;
        if(if_.else_stmt) {
          auto &else_ = create_bb(pre_if);
          construct(if_.else_stmt);
          create_bb(last_bb, if_end);
          if_end->jump(*last_bb);
          pre_if->br(cond,  if_start, else_);
        } else {
          create_bb(if_end);
          pre_if->br(cond,  if_start, *last_bb);
        }
      },
      [&](tree::for_statement_t &for_) {
        visit(for_.clause, [&]<class Tree>(tree::tree_value<Tree> tree) {
          if constexpr(!__is_same(Tree, tree::empty_node_t)) construct(tree);
        });
        auto &loop_start = create_bb(last_bb);
        auto cond = construct(for_.cond);
        construct(for_.body);
        construct(for_.step);
        last_bb->br(cond, loop_start, create_bb(last_bb));
      },
      [&]<narrow<tree::expression_t> T>(T &) {
        construct(tree::expression{tree::tree_value<T>(stmt)});
      },
      [&](tree::compound_statement_t &stmts) { for(auto stmt : stmts) construct(stmt); },
    });
  }
};



}}

void c9::simple::dumper::dump(br br) {
  begin();
  fprint(out, "br cond: ");
  dump(br.cond);
  fprint(out, ", true: bb_{}, false: bb_{}", br.true_.i, br.false_.i);
  end();
}
void c9::simple::dumper::dump(jump jmp) {
  begin();
  fprint(out, "jump bb_{}", jmp.target.i);
  end();
}
