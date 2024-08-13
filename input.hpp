#pragma once

#include "meta.hpp"
#include <map>
#include "string.hpp"
namespace c9 {


struct file {
  const char *buf, *limit;

  fs::path path;

  bool pragma_mark : 1{};
  opt<string> miopt_name;
};



inline opt<file> open_file(fs::path path) {
  FILE *infile = fopen(path.c_str(), "r");
  size_t numbytes;


  if(!infile)
    return {};

  fseek(infile, 0L, SEEK_END);
  numbytes = ftell(infile);


  fseek(infile, 0L, SEEK_SET);


  file file { .path = mov(path) };
  auto buf = (char *) malloc(numbytes * sizeof(char) + 1);

  file.buf = buf;
  file.limit = file.buf + numbytes * sizeof(char);;

  fread(buf, sizeof(char), numbytes, infile);
  fclose(infile);

  return file;
}

struct file_set : std::map<fs::path, file> {
  using map = std::map<fs::path, file>;
  file &operator[](fs::path path) {
    auto path1 = fs::canonical(path);
    if(auto p = this->find(fs::canonical(path1)); p != this->end())
      return p->second;

    return map::operator[](path1) = *open_file(path1);


    c9_assert(0);
  }
};




}
