cmake_minimum_required(VERSION 3.0)



project(c9)



include_directories(c9
  PUBLIC
    ${CMAKE_CURRENT_LIST_DIR}
  )
add_compile_options(-std=gnu++23 -O0 -O2 -O0 -O0 -O0 -O0 -O0 -funsigned-char -march=native  -ftrack-macro-expansion=0  -Wall -Wextra)

add_executable(c9 main.cpp token.cpp parser.cpp macro.cpp directives.cpp pp-expr.cpp semantics.cpp)

add_executable(check file-check.cpp)
