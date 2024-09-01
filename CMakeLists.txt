cmake_minimum_required(VERSION 3.0)



project(c9)



include_directories(c9
  PUBLIC
    ${CMAKE_CURRENT_LIST_DIR}
  )
add_compile_options(-std=gnu++23 -O1 -O2 -funsigned-char -march=native -Wall -Wextra)

add_executable(c9 main.cpp token.cpp)

add_executable(check file-check.cpp)
