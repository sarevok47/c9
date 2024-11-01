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
#include <cstdarg>
#include <libgen.h>
       #include <glob.h>
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

void parse_args(option &opt, int argc, char **argv) {
  std::vector<const char *> idirafter;

  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "-###")) {
      opt.hash_hash_hash = true;
      continue;
    }

    if (!strcmp(argv[i], "-cc1")) {
      opt.cc1 = true;
      continue;
    }

    if (!strcmp(argv[i], "-o")) {
      opt.o = argv[++i];
      continue;
    }

    if (!strncmp(argv[i], "-o", 2)) {
      opt.o = argv[i] + 2;
      continue;
    }

    if (!strcmp(argv[i], "-S")) {
      opt.S = true;
      continue;
    }

    if (!strcmp(argv[i], "-fcommon")) {
      opt.fcommon = true;
      continue;
    }

    if (!strcmp(argv[i], "-fno-common")) {
      opt.fcommon = false;
      continue;
    }

    if (!strcmp(argv[i], "-c")) {
      opt.c = true;
      continue;
    }

    if (!strcmp(argv[i], "-E")) {
      opt.E = true;
      continue;
    }

    if (!strncmp(argv[i], "-I", 2)) {
      opt.include_paths.emplace_back(argv[i] + 2);
      continue;
    }

    if (!strcmp(argv[i], "-D")) {
      opt.predefined_macro = "#define "s + argv[++i] + "\n"s;
      continue;
    }

    if (!strncmp(argv[i], "-D", 2)) {
       opt.predefined_macro = "#define "s + (argv[i] + 2) + "\n"s;
      continue;
    }
#if 0
    if (!strcmp(argv[i], "-U")) {
      opt.preundefined_macro = "#undef "s + argv[++i] + "\n"s;
      undef_macro(argv[++i]);
      continue;
    }

    if (!strncmp(argv[i], "-U", 2)) {
      undef_macro(argv[i] + 2);
      continue;
    }

    if (!strcmp(argv[i], "-include")) {
      strarray_push(&opt_include, argv[++i]);
      continue;
    }

    if (!strcmp(argv[i], "-x")) {
      opt_x = parse_opt_x(argv[++i]);
      continue;
    }

    if (!strncmp(argv[i], "-x", 2)) {
      opt_x = parse_opt_x(argv[i] + 2);
      continue;
    }

    if (!strncmp(argv[i], "-l", 2) || !strncmp(argv[i], "-Wl,", 4)) {
      strarray_push(&input_paths, argv[i]);
      continue;
    }

    if (!strcmp(argv[i], "-Xlinker")) {
      strarray_push(&ld_extra_args, argv[++i]);
      continue;
    }

    if (!strcmp(argv[i], "-s")) {
      strarray_push(&ld_extra_args, "-s");
      continue;
    }

    if (!strcmp(argv[i], "-fpic") || !strcmp(argv[i], "-fPIC")) {
      opt_fpic = true;
      continue;
    }
#endif
    if (!strcmp(argv[i], "-cc1-input")) {
      opt.base_file = argv[++i];
      continue;
    }

    if (!strcmp(argv[i], "-cc1-output")) {
      opt.output_file = argv[++i];
      continue;
    }
#if 0
    if (!strcmp(argv[i], "-idirafter")) {
      strarray_push(&idirafter, argv[i++]);
      continue;
    }

    if (!strcmp(argv[i], "-static")) {
      opt_static = true;
      strarray_push(&ld_extra_args, "-static");
      continue;
    }

    if (!strcmp(argv[i], "-shared")) {
      opt_shared = true;
      strarray_push(&ld_extra_args, "-shared");
      continue;
    }

    if (!strcmp(argv[i], "-L")) {
      strarray_push(&ld_extra_args, "-L");
      strarray_push(&ld_extra_args, argv[++i]);
      continue;
    }

    if (!strncmp(argv[i], "-L", 2)) {
      strarray_push(&ld_extra_args, "-L");
      strarray_push(&ld_extra_args, argv[i] + 2);
      continue;
    }

    if (!strcmp(argv[i], "-hashmap-test")) {
      hashmap_test();
      exit(0);
    }
#endif
    // These options are ignored for now.
    if (!strncmp(argv[i], "-O", 2) ||
        !strncmp(argv[i], "-W", 2) ||
        !strncmp(argv[i], "-g", 2) ||
        !strncmp(argv[i], "-std=", 5) ||
        !strcmp(argv[i], "-ffreestanding") ||
        !strcmp(argv[i], "-fno-builtin") ||
        !strcmp(argv[i], "-fno-omit-frame-pointer") ||
        !strcmp(argv[i], "-fno-stack-protector") ||
        !strcmp(argv[i], "-fno-strict-aliasing") ||
        !strcmp(argv[i], "-m64") ||
        !strcmp(argv[i], "-mno-red-zone") ||
        !strcmp(argv[i], "-w"))
      continue;

    if (argv[i][0] == '-' && argv[i][1] != '\0')
      fprintln(stderr, "unknown argument: {}", argv[i]);

    opt.input_paths.emplace_back(argv[i]);
  }
#if 0
  for (int i = 0; i < idirafter.len; i++)
    strarray_push(&include_paths, idirafter.data[i]);

  if (input_paths.len == 0)
    error("no input files");

  // -E implies that the input is the C macro language.
  if (opt_E)
    opt_x = FILE_C;
#endif
}
fs::path create_tmpfile() {
  char *path = strdup("/tmp/c9-XXXXXX");
  int fd = mkstemp(path);
  if (fd == -1)
    c9_assert(0);
  close(fd);

  return path;
}
void run_subprocess(char **argv) {
#if 0
  // If -### is given, dump the subprocess's command line.
  if (opt_hash_hash_hash) {
    fprintf(stderr, "%s", argv[0]);
    for (int i = 1; argv[i]; i++)
      fprintf(stderr, " %s", argv[i]);
    fprintf(stderr, "\n");
  }
#endif
  if (fork() == 0) {
    // Child process. Run a new command.
    execvp(argv[0], argv);
    fprintf(stderr, "exec failed: %s: %s\n", argv[0], strerror(errno));
    _exit(1);
  }

  // Wait for the child process to finish.
  int status;
  while (wait(&status) > 0);
  if (status != 0)
    exit(1);
}
void run_cc1(int argc, char **argv, fs::path input, fs::path output) {
  std::vector<const char *> args;
  for(auto a = argv; a != argv + argc; ++a) args.emplace_back(*a);

  args.emplace_back("-cc1");
  if (!input.empty()) {
    args.emplace_back("-cc1-input");
    args.emplace_back(input.c_str());
  }
  if (!output.empty()) {
    args.emplace_back("-cc1-output");
    args.emplace_back(output.c_str());
  }

  run_subprocess((char **) args.data());
}
void assemble(fs::path input, fs::path output) {
  const char *cmd[] = {"as", "-c", input.c_str(), "-o", output.c_str(), nullptr};
  run_subprocess((char **) cmd);
}
char *format(char *fmt, ...) {
  char *buf;
  size_t buflen;
  FILE *out = open_memstream(&buf, &buflen);

  va_list ap;
  va_start(ap, fmt);
  vfprintf(out, fmt, ap);
  va_end(ap);
  fclose(out);
  return buf;
}
char *find_libpath() {
  if (fs::exists("/usr/lib/x86_64-linux-gnu/crti.o"))
    return "/usr/lib/x86_64-linux-gnu";
  if (fs::exists("/usr/lib64/crti.o"))
    return "/usr/lib64";

}
char *find_file(char *pattern) {
  char *path = NULL;
  glob_t buf = {};
  glob(pattern, 0, NULL, &buf);
  if (buf.gl_pathc > 0)
    path = strdup(buf.gl_pathv[buf.gl_pathc - 1]);
  globfree(&buf);
  return path;
}
char *find_gcc_libpath() {
  char *paths[] = {
    "/usr/lib/gcc/x86_64-linux-gnu/*/crtbegin.o",
    "/usr/lib/gcc/x86_64-pc-linux-gnu/*/crtbegin.o", // For Gentoo
    "/usr/lib/gcc/x86_64-redhat-linux/*/crtbegin.o", // For Fedora
  };

  for (int i = 0; i < sizeof(paths) / sizeof(*paths); i++) {
    char *path = find_file(paths[i]);
    if (path)
      return dirname(path);
  }

}
void run_linker(option &opt, auto &&inputs, char *output) {
  std::vector<const char *> arr;

  arr.emplace_back("ld");
  arr.emplace_back("-o");
  arr.emplace_back(output);
  arr.emplace_back("-m");
  arr.emplace_back("elf_x86_64");

  char *libpath = find_libpath();
  char *gcc_libpath = find_gcc_libpath();

  if (opt.shared_) {
    arr.emplace_back(format("%s/crti.o", libpath));
    arr.emplace_back(format("%s/crtbeginS.o", gcc_libpath));
  } else {
    arr.emplace_back(format("%s/crt1.o", libpath));
    arr.emplace_back(format("%s/crti.o", libpath));
    arr.emplace_back(format("%s/crtbegin.o", gcc_libpath));
  }

  arr.emplace_back(format("-L%s", gcc_libpath));
  arr.emplace_back("-L/usr/lib/x86_64-linux-gnu");
  arr.emplace_back("-L/usr/lib64");
  arr.emplace_back("-L/lib64");
  arr.emplace_back("-L/usr/lib/x86_64-linux-gnu");
  arr.emplace_back("-L/usr/lib/x86_64-pc-linux-gnu");
  arr.emplace_back("-L/usr/lib/x86_64-redhat-linux");
  arr.emplace_back("-L/usr/lib");
  arr.emplace_back("-L/lib");

  if (!opt.static_) {
    arr.emplace_back("-dynamic-linker");
    arr.emplace_back("/lib64/ld-linux-x86-64.so.2");
  }
#if 0
  for (int i = 0; i < ld_extra_args.len; i++)
    arr.emplace_back(ld_extra_args.data[i]);
#endif
  for (size_t i = 0; i < inputs.size(); i++)
    arr.emplace_back(inputs[i]);

  if (opt.static_) {
    arr.emplace_back("--start-group");
    arr.emplace_back("-lgcc");
    arr.emplace_back("-lgcc_eh");
    arr.emplace_back("-lc");
    arr.emplace_back("--end-group");
  } else {
    arr.emplace_back("-lc");
    arr.emplace_back("-lgcc");
    arr.emplace_back("--as-needed");
    arr.emplace_back("-lgcc_s");
    arr.emplace_back("--no-as-needed");
  }

  if (opt.shared_)
    arr.emplace_back(format("%s/crtendS.o", gcc_libpath));
  else
    arr.emplace_back(format("%s/crtend.o", gcc_libpath));

  arr.emplace_back(format("%s/crtn.o", libpath));
  arr.emplace_back(nullptr);

  run_subprocess((char **) arr.data());
}
void cc1(option &opt) {

  x86_target t;
  driver d{.opt = opt, .t = t};
  auto &ff = d.files[opt.base_file];


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

  x86::codegen codegen{d, parser};
  for(;;) {
    while(parser.peek_token() == ";"_s)
      parser.consume();
    if(parser.peek_token()) {
      auto tree = parser.declaration();
#if 0
      tree(overload {
        [](auto &) {},
        [&](tree::function_t &fun) {
          cfg::control_flow_graph cfg{d, nlabel};
          cfg.construct(fun.definition);
          for(auto var : cfg.vars.map<tree::variable_t>()) section_data.emplace(var);
          c9::tree_opt::constprop(cfg);
          c9::tree_opt::cse(cfg);
          cfg.unssa();
          cfg.convert_to_two_address_code();
          cfg::cfg_walker walk{cfg.entry};
          regalloc::register_allocator alloc{cfg, x86::intreg{}, x86::op{}, x86::int_call_conv_sysv, x86::int_ret_reg};
          alloc.tab[size_t(x86::intreg::rsp)].second = false;
          alloc.tab[size_t(x86::intreg::rbp)].second = false;
          alloc.tab[size_t(x86::intreg::rip)].second = false;
          alloc();
        #if 1
          x86::codegen codegen{};
          codegen.gen(cfg.entry);
          codegen.dump(stderr);
        #else
          for(cfg::basic_block *bb = &cfg.entry; bb; bb = bb->step()) {
            bb->dump(stderr);
          }
        #endif
          fprintln(stderr, "\n\n");
        }
      });
#endif

     codegen(tree);
    }  else break;
}

    auto out = fopen(opt.output_file.c_str(), "wr");
    codegen.print(out);
}
int main(int argc, char **argv) {
  signal(SIGBUS, handler);   // install our handler
  signal(SIGABRT, handler);   // install our handler
  signal(SIGSEGV, handler);   // install our handler




  {
    using namespace  tree;
    #define BUILTIN_TYPE_DEF(name, ...) \
    { name##_t t; t.align = t.size = x86::size(x86::get_type(t)); \
      type_node<name##_t> = t; }


      void_type_node = void_type_t{};

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
  option opt;
  parse_args(opt, argc, argv);

  if (opt.cc1) {
    //add_default_include_paths(argv[0]);
    cc1(opt);
    return 0;
  }

  std::vector<const char *> ld_args;
  for (auto &&input : opt.input_paths) {

#if 0
    if (!strncmp(input, "-l", 2)) {
      strarray_push(&ld_args, input);
      continue;
    }

    if (!strncmp(input, "-Wl,", 4)) {
      char *s = strdup(input + 4);
      char *arg = strtok(s, ",");
      while (arg) {
        strarray_push(&ld_args, arg);
        arg = strtok(NULL, ",");
      }
      continue;
    }
#endif
    fs::path output;

    if (!opt.o.empty())
      output = opt.o;
    else if (opt.S)
      output = fs::path(input).replace_extension(".s");
    else
      output = fs::path(input).replace_extension(".o");
    // Handle .o or .a
    if (input.extension() == ".o" || input.extension() == ".a") {
      ld_args.emplace_back(input.c_str());
      continue;
    }

    // Handle .s
    if (input.extension() == ".s") {
      if (!opt.S)
        assemble(input, output);
      continue;
    }

    // Just preprocess
    if (opt.E ) {
      run_cc1(argc, argv, input, {});
      continue;
    }

    // Compile
    if (opt.S) {
      run_cc1(argc, argv, input, output);
      continue;
    }

    // Compile and assemble
    if (opt.c) {
      fs::path tmp = create_tmpfile();
      run_cc1(argc, argv, input, tmp);
      assemble(tmp, output);
      continue;
    }

    // Compile, assemble and link
    fs::path tmp1 = create_tmpfile();
    fs::path tmp2 = create_tmpfile();
    run_cc1(argc, argv, input, tmp1);
    assemble(tmp1, tmp2);
    char *p = (char *) malloc(tmp1.native().size());
    memcpy(p, tmp2.c_str(), tmp2.native().size());
    ld_args.emplace_back(p);
    continue;
  }
  if (ld_args.size() > 0)
    run_linker(opt, ld_args, !opt.o.empty() ? (char *) opt.o.c_str() : (char *) "a.out");

  return 0;



}
