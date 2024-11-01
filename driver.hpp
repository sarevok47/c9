#pragma once


#include "diagnostics.hpp"
#include "input.hpp"

namespace c9 {
enum class field_opt_type { none, c, asm_, obj, ar, dso, };

struct option {
  std::vector<fs::path> include_paths;
  std::vector<fs::path> system_include_paths = {
    "/usr/local/include",
    "/usr/include",
    "/usr/include/x86_64-linux-gnu",
    "/usr/lib/gcc/x86_64-redhat-linux/14/include"
  };
  std::string predefined_macro;
  std::vector<fs::path> input_paths;
  bool fcommon = true;
  bool fpic{};
  bool E{};
  bool S{};
  bool c{};
  bool cc1{};
  bool hash_hash_hash{};
  bool static_{};
  bool shared_{};
  fs::path o,  base_file, output_file;
};

struct driver {
  location_table loc_tab;
  file_set files;
  option opt;
  diagnostic_engine diag{*this, loc_tab};
  struct target &t;
};





}
