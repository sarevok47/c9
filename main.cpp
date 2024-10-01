#define MULTIPLE_INCLUDE_OPTIMIZATION
#include "system.hpp"
#include "meta.hpp"

#include "tree.hpp"
#include <cstdio>

#include <csignal>

#include <variant>

#include "pp.hpp"

#include <stdio.h>
#include <execinfo.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>
#include <sys/prctl.h>
#include  "parse.hpp"
#include "cfg.hpp"
#include "tree-opt.hpp"
#include "x86/target.hpp"

#include "regalloc.hpp"
void handler(int sig) {
  char pid_buf[30];
  sprintf(pid_buf, "%d", getpid());
  constexpr size_t name_buf_size = 512;
  char name_buf[name_buf_size];
  name_buf[readlink("/proc/self/exe", name_buf, name_buf_size - 1)]=0;
  prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY, 0, 0, 0);
  int child_pid = fork();
  if(!child_pid) {
    dup2(2,1); // redirect output to stderr - edit: unnecessary?
    execl("/usr/bin/gdb", "gdb", "--batch", "-n", "-ex", "thread", "-ex", "bt", name_buf, pid_buf, NULL);
    abort(); /* If gdb failed to start */
  } else {
    waitpid(child_pid,NULL,0);
  }
}
using namespace c9;

namespace c9 { namespace tree {
#ifdef EXPLICIT_TREE_SPEC
#define BUILTIN_TYPE_DEF(name, ...)  template<> name type_node<name##_t> = name##_t{};
BUILTIN_TYPE_DEF(char_type, : unsigned_integral_type_t {});
BUILTIN_TYPE_DEF(unsigned_char_type, : signed_integral_type_t {});
BUILTIN_TYPE_DEF(signed_char_type, : signed_integral_type_t {});
BUILTIN_TYPE_DEF(short_type, : signed_integral_type_t {});
BUILTIN_TYPE_DEF(unsigned_short_type, : unsigned_integral_type_t {});
BUILTIN_TYPE_DEF(int_type, : signed_integral_type_t {});
BUILTIN_TYPE_DEF(unsigned_int_type, : unsigned_integral_type_t {});
BUILTIN_TYPE_DEF(long_type, : signed_integral_type_t {});
BUILTIN_TYPE_DEF(unsigned_long_type, : unsigned_integral_type_t {});
BUILTIN_TYPE_DEF(long_long_type, : signed_integral_type_t {});
BUILTIN_TYPE_DEF(unsigned_long_long_type, : unsigned_integral_type_t {});
#endif
  tree_arena a{};
}}

int main(int argc, char **argv) {
  signal(SIGBUS, handler);   // install our handler
  signal(SIGABRT, handler);   // install our handler
  signal(SIGSEGV, handler);   // install our handler


x86_target t;
  driver d{.t = t};

  auto &ff = d.files["../../tmp.cpp"];


	{
		using namespace  tree;
#define BUILTIN_TYPE_DEF(name, ...) type_node<name##_t> = name##_t{};
BUILTIN_TYPE_DEF(void_type);
  	BUILTIN_TYPE_DEF(char_type, : unsigned_integral_type_t {});
  	BUILTIN_TYPE_DEF(unsigned_char_type, : signed_integral_type_t {});
  	BUILTIN_TYPE_DEF(signed_char_type, : signed_integral_type_t {});
  	BUILTIN_TYPE_DEF(short_type, : signed_integral_type_t {});
  	BUILTIN_TYPE_DEF(unsigned_short_type, : unsigned_integral_type_t {});
  	BUILTIN_TYPE_DEF(int_type, : signed_integral_type_t {});
  	BUILTIN_TYPE_DEF(unsigned_int_type, : unsigned_integral_type_t {});
  	BUILTIN_TYPE_DEF(long_type, : signed_integral_type_t {});
  	BUILTIN_TYPE_DEF(unsigned_long_type, : unsigned_integral_type_t {});
  	BUILTIN_TYPE_DEF(long_long_type, : signed_integral_type_t {});
  	BUILTIN_TYPE_DEF(unsigned_long_long_type, : unsigned_integral_type_t {});

  BUILTIN_TYPE_DEF(float_type,       : floating_type_t {});
  BUILTIN_TYPE_DEF(double_type,      : floating_type_t {});
  BUILTIN_TYPE_DEF(long_double_type, : floating_type_t {});


	}


  	file &file = ff;

lex::lexer lex{d, file};
lex::token f;





tree::default_ =  [] {
  tree::count_data cd;
  cd.count = 1;
  cd.index = tree::tree_type<tree::empty_node_t>::type_index;
  new(cd.data) tree::empty_node{};
  return cd;
}();


	pp::preprocessor pp{d, file};


	parse::parser parser{d, pp};

	for(; ;) {
    while(parser.peek_token() == ";"_s)
			parser.consume();
    if(parser.peek_token()) {
		  auto tree = parser.declaration();
      tree(overload {
        [](auto &) {},
        [&](tree::function_t &fun) {
          cfg::control_flow_graph cfg{d};
          cfg.construct(fun.definition);
          c9::tree_opt::constprop(cfg);
          c9::tree_opt::cse(cfg);

          cfg.unssa();
          cfg.convert_to_two_address_code();
          /*tree::ssa_variable tab[cfg.nssa + 1];
          cfg.collect_phi_operands(tab);
*/


          cfg::cfg_walker walk{cfg.entry};

          size_t tmps[cfg.ntmp], vars[cfg.nssa + 2];

          std::fill_n(tmps, cfg.ntmp, 0);
          std::fill_n(vars, cfg.nssa + 1, 0);

          regalloc::register_allocator alloc{cfg, x86::intreg{}, x86::op{}};


          alloc();
#if 1

          x86::codegen codegen;

          codegen.gen(cfg.entry);
#endif
#if 0
          for(cfg::basic_block *bb = &cfg.entry; bb; bb = bb->step()) {
            bb->dump(stderr);
           }
#endif
           fprintln(stderr, "\n\n");
        }

      });// tree::dumper{stderr}.dump(tree);


    } else break;
		//tree::dumper{stderr}.dump(tree);

	}





//s.simplify()
	return 0;

}
