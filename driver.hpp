#pragma once


#include "diagnostics.hpp"
#include "input.hpp"

namespace c9 {

struct option {
  std::vector<fs::path> include_paths;
  std::vector<fs::path> system_include_paths = {
    "/usr/local/include",
    "/usr/include",
    "/usr/include/x86_64-linux-gnu",
    "/usr/lib/gcc/x86_64-redhat-linux/14/include"
  };
};

struct driver {
  location_table loc_tab;
  file_set files;
  option opt;
  diagnostic_engine diag{*this, loc_tab};
  struct target &t;
};


template<class ...T> void diagnostic_engine::operator()(location_t loc, message_type mtype, std::format_string<T...> fmt, T&& ...args) {
  rich_location rl{loc};
  diagnostic_impl( stderr, rl, mtype, fmt, (decltype(args)) args...);
}



}
