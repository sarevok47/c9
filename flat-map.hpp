#pragma once

#include "meta.hpp"

namespace c9 {


template<class Key, class Value>
class flat_map {
  struct key_value { Key key; Value value; };

  std::vector<key_value> storage;
public:
  auto begin() const { return storage.begin(); }
  auto end()   const { return storage.end();   }
  auto find(const Key &key)   {
    return std::ranges::find_if(storage, [&](auto &kv) { return kv.key == key; });
  }

  void push_back(key_value kv) { storage.push_back(mov(kv)); }

  void erase(auto p) { storage.erase(p); }

  void clear() { storage.clear(); }

  Value &operator[](const Key &key) {
    if(auto p = find(key); p == end()) {
      push_back({key, {}});
      return storage.back().value;
    } else
      return p->value;
  }
};

}
