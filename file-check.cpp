#include "diagnostics.hpp"
#include <regex>


using namespace c9;

struct check {
  size_t line;
  sv pattern;
};


bool check_file(fs::path path) {
  auto in = *open_file(path);

  const char *s = in.buf;

  std::optional<std::string> cl_run;

  std::vector<check> checks;

  for(size_t line = 1; s != in.limit; line += *s == '\n') {
     auto skip = [&](auto f) {
      for(; s != in.limit && f(*s); ++s)
        if(*s == '\n') ++line;
     };
    if(sv pos{s, in.limit}; pos.starts_with("%run")) {
      s += sizeof("%run");
      skip(is_space);
      auto start = s;
      skip(_ != '\n');
      if(cl_run) {
        fprint(stderr, "dublicate %run at {} line", line);
        return false;
      }
      cl_run = std::vformat(sv{start, s}, std::make_format_args(in.path.native()));
    } else if(pos.starts_with("%next")) {
      s += sizeof("%next");
      skip(is_space);
      auto start = s;
      skip(_ != '\n');
      checks.emplace_back(line, sv{start, s});
    } else
      ++s;
  }

  if(!cl_run)
    return true;
  system(cl_run->c_str());

  auto output = *open_file(/*"/dev/stdout"*/  "../../tmp.cpp");

  auto outp = output.buf;

  auto check = checks.begin();
  bool ok = true;
  for(;;) {
    auto skip = [&](auto f) {
      for(; outp < output.limit && f(*outp); ++outp);
    };
    skip(is_space);
    auto start = outp;
    skip(_ != '\n');


    bool ok_1;
    ok &= ok_1 = sv{start, outp} == check->pattern;
    fprintln(stderr, "{}:{} {}", path.native(), check->line, ok ? "PASSED" : "FAILED");
    if(!ok_1)
      fprintln(stderr, "\tcompare '{}' and pattern '{}'", sv{start, outp}, check->pattern);;
    ++check;
    if(!ok || check == checks.end())
      break;
  }
  return ok;
}


int main() {
  for(auto &&file : fs::recursive_directory_iterator{"../test-suite"})
    if(!file.is_directory())
      check_file(file.path());
  return 0;
}
