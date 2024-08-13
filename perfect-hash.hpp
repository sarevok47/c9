#include "meta.hpp"

namespace c9 {


#if 0
struct perfect_map {



  size_t hash(sv str) const {
    size_t n = str.size();
    unsigned int r = 0;

    for(auto s = str.begin(); n--; )
      r = (r) * 67 + ((*s++) - 113);

    return r + str.size();
  }

  perfect_map() {}
};

#endif
constexpr auto perfect_map(auto ...args) {
  char arr[sizeof...(args)]
}

}
