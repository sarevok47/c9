#pragma once

#include "meta.hpp"
#include <unistd.h>
#include <sys/mman.h>
#include <vector>
#include <algorithm>
namespace c9 {
#define SHARED_STR

#ifdef SHARED_STR


struct shared_string_data {
  size_t count = 1;
  size_t n{};
  char data[];
};

static shared_string_data ssd{};

struct shared_alloc {
#if 1
  std::vector<shared_string_data *> pointers;

  shared_string_data *allocate(size_t data_n) {\
    pointers.emplace_back(
      (shared_string_data *) malloc( sizeof(shared_string_data) + data_n)
    );
    c9_assert(pointers.back());
    return pointers.back();
  }
   ~shared_alloc() {
      //for(auto p : pointers)
       // if(!p->count) free(p);
   }
#endif
} static sha;
class string {
protected:
  shared_string_data *data = &ssd;

public:
  using value_type = char;
  string() noexcept = default;
  string(const string &string) {
    data = string.data;
    ++data->count;
  }
  string(const char *start, const char *finish) noexcept {
    data = sha.allocate(finish - start);

    data->count = 1;
    data->n = finish - start;
    std::copy(start, finish, data->data);
  }
  string(sv sv) {
    data = sha.allocate(sv.size());

    data->count = 1;
    data->n = sv.size();
    std::copy(sv.begin(), sv.end(), data->data);
  }
  template<size_t N>
  string(const char (&s)[N]) {
    data = sha.allocate(N);

    data->count = 1;
    data->n = N;
    std::copy_n(s, N, data->data);
  }
  string(std::ranges::range auto &&r) {
    data = sha.allocate(std::size(r));

    data->count = 1;
    data->n = std::size(r);
    std::ranges::copy(r, data->data);
  }
  string(const char *s, size_t len) {
    data = sha.allocate(len);

    data->count = 1;
    data->n = len;
    std::copy_n(s, len, data->data);
  }
  string(const char *s) {
    size_t N = strlen(s);
    data = sha.allocate(N);

    data->count = 1;
    data->n = N;
    std::copy_n(s, N, data->data);
  }
  string &operator=(const string &rhs) noexcept {
    data = rhs.data;
    ++data->count;

    return *this;
  }
  char operator[](size_t n) const { return data->data[n]; }

  const char *begin() { return data->data; }
  const char *begin() const { return data->data; }
  const char *end() { return data->data + data->n; }
  const char *end() const { return data->data + data->n; }


  char front() const { return *data->data; }
  char back()  const { return data->data[data->n]; }

  size_t size() const { return data->n; }
  bool   empty() const { return !size(); }

  operator sv() const { return {data->data, data->n}; }

  bool operator==(sv rhs) const {
    return std::equal(begin(), end(), rhs.begin(), rhs.end());
  }
  string &operator+=(string rhs) {
    size_t size_ = size() * sizeof(char) + rhs.size() * sizeof(char);
    auto data_ = sha.allocate(size_ + 1);


    std::ranges::copy(*this, data_->data);
    std::ranges::copy(rhs, data_->data + size());
    data_->data[size_] = '\0';
    data_->n = size_;
    data_->count = 1;

    this->~string();

    data =  data_;

    return *this;
  }

  struct hash {
    size_t operator()(const string &str) const {
      size_t n = str.size();
      size_t r = 0;

      for(auto s = str.begin(); n--; )
        r = (r) * 67 + ((*s++) - 113);

      return r + str.size();
    }
  };

  ~string() {
    --data->count;
  }
};



class sub_string : string {
  size_t start, finish;
public:
  sub_string(string str, size_t start, size_t finish) : start{start}, finish{finish} {
    c9_assert(start < str.size() && finish < str.size());
    string::operator=(str);
  }

  char operator[](size_t n) const { return data->data[n]; }

  const char *begin() { return data->data + start ; }
  const char *begin() const { return data->data + start; }
  const char *end() { return data->data + finish; }
  const char *end() const { return data->data + finish; }


  char front() const { return data->data[start]; }
  char back()  const { return data->data[finish]; }

  size_t size() const { return finish - start; }
  bool   empty() const { return !size(); }

  operator sv() const { return {data->data + start, data->data + finish}; }
};
#endif



}



