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
#define INTEGRAL_BUILTIN_TYPE(name, ...)  template<> name type_node<name##_t> = name##_t{};
INTEGRAL_BUILTIN_TYPE(char_type, : unsigned_integral_type_t {});
INTEGRAL_BUILTIN_TYPE(unsigned_char_type, : signed_integral_type_t {});
INTEGRAL_BUILTIN_TYPE(signed_char_type, : signed_integral_type_t {});
INTEGRAL_BUILTIN_TYPE(short_type, : signed_integral_type_t {});
INTEGRAL_BUILTIN_TYPE(unsigned_short_type, : unsigned_integral_type_t {});
INTEGRAL_BUILTIN_TYPE(int_type, : signed_integral_type_t {});
INTEGRAL_BUILTIN_TYPE(unsigned_int_type, : unsigned_integral_type_t {});
INTEGRAL_BUILTIN_TYPE(long_type, : signed_integral_type_t {});
INTEGRAL_BUILTIN_TYPE(unsigned_long_type, : unsigned_integral_type_t {});
INTEGRAL_BUILTIN_TYPE(long_long_type, : signed_integral_type_t {});
INTEGRAL_BUILTIN_TYPE(unsigned_long_long_type, : unsigned_integral_type_t {});
#endif
  tree_arena a{};
}}

int main(int argc, char **argv) {
  signal(SIGBUS, handler);   // install our handler
  signal(SIGABRT, handler);   // install our handler
  signal(SIGSEGV, handler);   // install our handler
  driver d;

  auto &ff = d.files["../../tmp.cpp"];



	{
		using namespace  tree;
#define INTEGRAL_BUILTIN_TYPE(name, ...) type_node<name##_t> = name##_t{};

  	INTEGRAL_BUILTIN_TYPE(char_type, : unsigned_integral_type_t {});
  	INTEGRAL_BUILTIN_TYPE(unsigned_char_type, : signed_integral_type_t {});
  	INTEGRAL_BUILTIN_TYPE(signed_char_type, : signed_integral_type_t {});
  	INTEGRAL_BUILTIN_TYPE(short_type, : signed_integral_type_t {});
  	INTEGRAL_BUILTIN_TYPE(unsigned_short_type, : unsigned_integral_type_t {});
  	INTEGRAL_BUILTIN_TYPE(int_type, : signed_integral_type_t {});
  	INTEGRAL_BUILTIN_TYPE(unsigned_int_type, : unsigned_integral_type_t {});
  	INTEGRAL_BUILTIN_TYPE(long_type, : signed_integral_type_t {});
  	INTEGRAL_BUILTIN_TYPE(unsigned_long_type, : unsigned_integral_type_t {});
  	INTEGRAL_BUILTIN_TYPE(long_long_type, : signed_integral_type_t {});
  	INTEGRAL_BUILTIN_TYPE(unsigned_long_long_type, : unsigned_integral_type_t {});



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

	std::list<tree::statement> stmt;
	while(parser.peek_token()) {
		auto tree = parser.declaration();

//		tree::dumper{stderr}.dump(tree);
		while(parser.peek_token() == ";"_s)
			parser.consume();
	}




//s.simplify()
	return 0;

}
