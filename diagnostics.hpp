#pragma once
#include "system.hpp"
#include "meta.hpp"
#include "variant.hpp"
#include "input.hpp"
#include "string.hpp"
#include <format>
#include <vector>
#include <ranges>

namespace c9 {
  template<class T> struct bold { T value; };
  namespace pp  { struct tokenbuf; }
  namespace lex { struct token;  }
}

template<class T> struct std::formatter<c9::bold<T>> {
  constexpr auto parse(auto &ctx) {
    return ctx.begin();
  }
  auto format(c9::bold<T> bold, std::format_context& ctx) const {
    return std::format_to(ctx.out(), "\e[1m{}\e[0m", bold.value);
  }
};

template<> struct std::formatter<c9::string> {
  constexpr auto parse(auto &ctx) {
    return ctx.begin();
  }
  auto format(c9::string str, std::format_context& ctx) const {
    return std::format_to(ctx.out(), "{}", (c9::sv) str);
  }
};


namespace c9 {
namespace lex {
  const char *search_end_of_phase2_line(const char *s, const char *end);
}


enum class color {
  none = 00,
  bold = 01,
  underscore = 04,
  blink = 05,
  reverse = 07,
  fg_black = 30,
  fg_red = 31,
  fg_green = 32,
  fg_yellow = 33,
  fg_blue = 34,
  fg_magenta = 35,
  fg_cyan = 36,
  fg_white = 37,
  fg_bright_black = 90,
  fg_bright_red = 91,
  fg_bright_green = 92,
  fg_bright_yellow = 93,
  fg_bright_blue = 94,
  fg_bright_magenta = 95,
  fg_bright_cyan = 96,
  fg_bright_white = 97,
  bg_black = 40,
  bg_red = 41,
  bg_green = 42,
  bg_yellow = 43,
  bg_blue = 44,
  bg_magenta = 45,
  bg_cyan = 46,
  bg_white = 47,
  bg_bright_black = 100,
  bg_bright_red = 101,
  bg_bright_green = 102,
  bg_bright_yellow = 103,
  bg_bright_blue = 104,
  bg_bright_magenta = 105,
  bg_bright_cyan = 106,
  bg_bright_white = 107
};


struct message_type : variant_t<"error"_s, "warning"_s, "note"_s> {
  using variant_t<"error"_s, "warning"_s, "note"_s>::variant;


  auto color() {
    return visit(*this, overload {
      [](decltype("error"_s))   { return color::fg_red;   },
      [](decltype("note"_s)) { return color::fg_magenta;  },
      [](decltype("warning"_s)) { return color::fg_magenta; }
    });
  }
  sv spelling() {
    return visit(*this, [](auto s) { return s.c_str(); } );
  }
};


template<class ...T> void fprint(FILE *out, std::format_string<T...> fmt, T&& ...args) {
  std::string str;
  std::format_to(std::back_inserter(str), fmt, (decltype(args)) args...);
  fwrite(str.c_str(), sizeof(char), str.size(), out);
}
template<class ...T> void fprintln(FILE *out, std::format_string<T...> fmt, T&& ...args) {
  std::string str;
  std::format_to(std::back_inserter(str), fmt, (decltype(args)) args...);
  str += "\n";
  fwrite(str.c_str(), sizeof(char), str.size(), out);
}
template<class ...T> void fcolorprint(FILE *out, color color, std::format_string<T...> fmt, T&& ...args) {
  fprint(out, "\033[{}m", (size_t) color);
  fprint(out, fmt, (decltype(args)) args...);
  fprint(out, "\033[{}m", (size_t) color::none);
}
template<class ...T> void print( std::format_string<T...> fmt, T&& ...args) {
  fprint(stdout, fmt, (decltype(args)) args...);
}
template<class ...T> void println(std::format_string<T...> fmt, T&& ...args) {
  fprintln(stdout, fmt, (decltype(args)) args...);
}



using location_t = size_t;
struct source_range {
  location_t first;
  location_t last;

  source_range() = default;
  source_range(location_t first, location_t last) : first{first}, last{last} { c9_assert(last > first); }
  source_range(location_t first) : first{first}, last{first + 1} { }

  source_range operator+(source_range rhs) const {
    c9_assert(rhs.last >= last && rhs.first >= first);
    return {first, rhs.last};
  }
};

struct location {
  size_t map_tag;
  size_t start, finish;
};
struct macro_location {
  location_t in_definition;
  source_range invoked;
};

struct line_map {
  string file_name;
  size_t line;
  file &from_file;

  const char *start_of_line;
};
struct rich_location {
  location_t main_loc;
  variant<
    std::initializer_list<location_t>,
    source_range
  > locs;
};

template<class ...T> void hdr_print(FILE *out, line_map hdr, message_type mtype, std::format_string<T...> fmt, T&& ...args) {
  fprint(out, "{}:{}: ", bold{hdr.file_name}, bold{hdr.line});
  fcolorprint(out, mtype.color(), "{}: ", mtype.spelling());
  fprintln(out, fmt, (decltype(args)) args...);
}

template<class ...T> void print_with_colon(auto x, FILE *out, std::format_string<T...> fmt, T&& ...args) {
  fprint(out, "{:>6} | ", x);
  fprintln(out, fmt, (decltype(args)) args...);
}


struct location_table {
  std::vector<variant<location, macro_location>> locs;
  std::vector<line_map> line_maps;

  void next_line(line_map line_map) { line_maps.emplace_back(mov(line_map)); }
  auto &operator[](location_t loc) { return locs[loc]; }
  location_t operator[](auto location) {
    c9_assert(line_maps.size());
    locs.emplace_back(location);
    return locs.size() - 1;
  }

  size_t get_map_tag(location_t loc) {
    return visit(locs[loc], overload {
      [](location &loc) { return loc.map_tag; },
      [&](macro_location &loc) { return get_map_tag(loc.invoked.first); }
    });
  }
};

struct diagnostic_engine {
  struct driver &d;
  location_table &loc_tab;


  template<class ...T> void operator()(location_t loc, message_type type, std::format_string<T...> fmt, T&& ...args) {
    rich_location rl{loc};
    diagnostic_impl(stderr, rl, type, fmt, (decltype(args)) args...);
  }
  template<class ...T> void operator()(source_range loc, message_type type, std::format_string<T...> fmt, T&& ...args) {
    rich_location rl{loc.first, loc};
    diagnostic_impl(stderr, rl, type, fmt, (decltype(args)) args...);
  }
  template<class ...T> void operator()(rich_location rl, message_type type, std::format_string<T...> fmt, T&& ...args) {
    diagnostic_impl(stderr, rl, type, fmt, (decltype(args)) args...);
  }
private:
  void line_render(FILE *out, size_t line_start, size_t line_finish, auto &&underline) {
    for(; line_start <= line_finish; ++line_start) {
      auto &line_map = loc_tab.line_maps[line_start];

      auto end_of_line = std::find(line_map.start_of_line, line_map.from_file.limit, '\n');

      sv line{line_map.start_of_line, end_of_line};
      print_with_colon(line_map.line, out, "{}", line);
      underline(line_start, line.size());
    }
  }

  void loc_render(FILE *out, rich_location loc) {
    diagnostic_renderer(out, loc);

    visit(loc_tab[loc.main_loc], overload {
      [](location) {},
      [&](macro_location ml) {
        (*this)(ml.in_definition, "note"_s, "in definition of macro");
      }
    });
  }

public:
  void diagnostic_renderer(FILE *out, rich_location loc) {
    auto [min, max] = visit(loc.locs, overload {
      [&](std::ranges::range auto &&r) {
        auto max_p = std::ranges::max_element(r);
        auto min_p = std::ranges::min_element(r);

        location_t max = !r.size() ? loc.main_loc : std::max(*max_p, loc.main_loc),
                   min = !r.size() ? loc.main_loc : std::min(*min_p, loc.main_loc);
        return std::pair{min, max};
      },
      [&](source_range sr) {
        return std::pair{std::min(sr.first, loc.main_loc), std::max(sr.first, loc.main_loc)};
      }
    });


    auto underline = [&](size_t cur_line, size_t line_len) {
      char buf[line_len];
      auto caret = [&](location_t loc, bool main = false) {
        auto f1 = [&](location &loc) {
          if(loc.map_tag == cur_line) {
            if(main) {
              buf[loc.start] = '^';
              std::fill(buf + loc.start + 1, buf + loc.finish, '~');
            } else
              std::fill(buf + loc.start, buf + loc.finish, '~');
          }
        };
        auto f2 = [&](auto &&f2, macro_location ml) -> void {
          for(location_t loc = ml.invoked.first; loc != ml.invoked.last; ++loc, main = false)
            visit(loc_tab[loc], overload {
              [&](location &loc) { f1(loc); },
              [&](macro_location &ml) {  f2(f2, ml); }
            });
        };
        visit(loc_tab[loc], overload {
          f1,
          [&](macro_location &ml) { f2(f2, ml); }
        });
      };
      std::fill_n(buf, line_len, ' ');

      visit(loc.locs, overload {
        [&](std::ranges::range auto &&r) {
          for(location_t l : r)
            caret(l);
        },
        [&](source_range sr) {
          for(location_t l = sr.first; l != sr.last; ++l)
            caret(l);
        }
      });
      caret(loc.main_loc, true);
      print_with_colon("", out, "{}", sv{buf, line_len});
    };
    line_render(out, loc_tab.get_map_tag(min), loc_tab.get_map_tag(max),  underline);
  }


  template<class ...T>
  void diagnostic_impl(FILE *out, rich_location loc, message_type mtype, std::format_string<T...> fmt, T&& ...args) {
    hdr_print(out, loc_tab.line_maps[loc_tab.get_map_tag(loc.main_loc)], mtype, fmt, (decltype(args)) args...);
    loc_render(out, loc);
  }
};




};
