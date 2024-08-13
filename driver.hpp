#pragma once


#include "diagnostics.hpp"
#include "input.hpp"

namespace c9 {

struct option {
  std::vector<fs::path> include_paths;
  std::vector<fs::path> system_include_paths = {
    "/usr/local/include",
    "/usr/include",
    "/usr/lib/gcc/x86_64-pc-linux-gnu/13.2.1/include/",
    "/usr/lib/clang/17/include/",
    "/usr/include/c++/14.1.1/",
    //"/usr/include/c++/14.1.1/bits/"
    "/usr/include/c++/14.1.1/x86_64-pc-linux-gnu/"
  };
};

struct driver {
  location_table loc_tab;
  file_set files;
  option opt;
  diagnostic_engine diag{*this, loc_tab};
};


template<class ...T> void diagnostic_engine::operator()(location_t loc, message_type mtype, std::format_string<T...> fmt, T&& ...args) {
  rich_location rl{loc};
  diagnostic_impl( stderr, rl, mtype, fmt, (decltype(args)) args...);
}



}
