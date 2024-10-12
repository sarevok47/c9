#pragma once

#include "lex.hpp"

namespace c9 { namespace lex {
// Copy next line concateneted with lines after backslashes
bool lexer::clean_line() {
  for(; cur_note != line_notes.end(); ++cur_note)
    ++line;

  line_notes.clear();

  auto eol = physical_start = phase2_start = buf;

  align = 0;

  fresh_line.clear();

  if(buf >= limit) [[unlikely]]
    return false;
  else
    while((eol = search_end_of_phase2_line(eol, limit))) {
      if(eol == limit || *eol == '\n') {
        d.loc_tab.next_line({
          file_name, line, src_file, physical_start
        });
        fresh_line.append(buf, eol);
        buf =  ++eol;
        break;
      } else if(*eol == '\\') {
        auto bl_process = [&] {
          fresh_line.append(buf, eol);
          line_notes.push_back(eol - phase2_start);
        };

        if(eol + 1 == limit || eol[1] == '\n') {
          bl_process();
          buf = eol += 2;
          continue;
        }
        if(isspace(eol[1])) [[unlikely]]
          for(auto cur = eol + 1; ; ++cur) {
            if(cur == limit || *cur == '\n') {
              bl_process();
              buf =  eol = cur + 1 ;
              goto continue_;
            } else if(!isspace(*cur))
              break;
          }

          ++eol;

        continue_:
        continue;
      } else
        break;
    }


    cur = fresh_line.c_str();
    cur_note = line_notes.begin();
    return true;
}

/* Process the notes created by add_line_note as far as the current
 * location. */
size_t lexer::process_line_notes(const char *pos) {
  size_t idx = pos - fresh_line.c_str();

  for(; cur_note != line_notes.end() && idx + align >= *cur_note; ++cur_note) {
    auto slash = phase2_start + *cur_note + 1;
    if(*slash != '\n') [[unlikely]]
      for(; slash != limit && *slash != '\n'; ++slash)
        ++align;
    ++line;
    physical_start = slash + 1;
    align += 2; // \\\n
    d.loc_tab.next_line({file_name, line, src_file, physical_start});
  }

  return phase2_start + idx + align - physical_start;
}


lex::token lexer::lex_number(lex::token &tok) {
  auto start = cur;

  bool floating{}, exponent{};
  for(;;) {
    if(validate_cont(cur))
      ;
    else if(((*cur == '+' || *cur == '-')
      && (cur[-1] == 'e'
      || cur[-1] == 'E'
      || cur[-1] == 'p'
      || cur[-1] == 'P'))
    ) {
      floating = exponent = true;
      ++cur;
    } else if(*cur == '.' && (cur - 1 == start || cur[-1] != '.')) {
      floating = true;
      ++cur;
    } else if(*cur == '\'')
      ++cur;
    else
      break;
  }

  tok = numeric_constant{ {start, cur}, floating, exponent};
  return tok;
}

location_t lexer::map_instance(flags flags, const char *start, const char *finish) {
  if(!(flags & flags::disable_loc))
    return d.loc_tab[location{d.loc_tab.line_maps.size() - 1, process_line_notes(start), process_line_notes(finish)}];
  return {};
}



bool lexer::next_line() {
  ++line;

  for(; buf < limit && *buf == '\n'; ++buf)
    ++line;

  return clean_line();
}

token lexer::lex_token(flags flags = {}) {
  token tok;
  start:
  tok.start_of_line |= cur == fresh_line.c_str();
  for(; isspace(*cur); tok.prev_space = true)
    ++cur;

  auto start = cur;

  switch(*cur) {
    case '\0':
      if(flags & flags::discard_next_line || !next_line()) {
        tok.loc = map_instance(flags, cur, cur + 1);
        tok = eof{};
        return tok;
      }
      goto start;
    case '.':
      if(!isdigit(cur[1])) {
        tok = scan(cur);
        break;
      }
      [[fallthrough]];
    case '0' ... '9':
      lex_number(tok);
      break;
    case '/':
      if(!(flags & flags::paste_tokens) && cur[1] == '/') {
        while(*cur && *cur != '\n')
          ++cur;
        goto start;
      }
      if(!(flags & flags::paste_tokens) && cur[1] == '*') {
        location_t start_comment_loc = map_instance(flags, cur, cur + 2);

        for(;;)
          if(*cur == '*' && cur[1] == '/') {
            cur += 2;
            break;
          } else if(*cur) {
            ++cur;
          } else if(!next_line()) {
            report(flags, start_comment_loc, "error"_s, "unterminated /* comment");
            break;
          }
          goto start;
      }

      tok = scan(cur);

      break;
    case '<':
      if(flags & flags::angled_string) [[unlikely]] {
        lex_string<angled_string>(flags, tok, start);
        return tok;
      }

      tok = scan(cur);
      break;

    default:
      variant_t<""_s, "U"_s, "u"_s, "u8"_s, "L"_s, "L"_s> prefix;

      auto s = cur;
      scan_impl(cur, prefix, variant_types(prefix), 0_c, ""_s);

      if(*cur == '\"') {
        lex_string<string_literal>(flags, tok, start);
        break;
      } else if(*cur == '\'') {
        lex_string<char_literal>(flags, tok, start);
        break;
      } else
        cur = s;


    if(validate_start(cur)) {
      while(validate_cont(cur))
        ;
      tok = identifier{{start, cur}};
      break;
    }
    tok = scan(cur);
    if(tok == eof{}) {
      ++cur;
      tok = undefined{{start, cur}};
      break;
    }
    break;
  }

  tok.loc = map_instance(flags, start, cur);
  return tok;
}
}}
