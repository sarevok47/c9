cmake_minimum_required(VERSION 3.0)



project(c9)


include_directories(c9
  PUBLIC
    ${CMAKE_CURRENT_LIST_DIR}
  )
add_compile_options(-std=gnu++23 -funsigned-char -march=native  -ftrack-macro-expansion=0  -Wall -Wextra)

add_executable(c9 main.cpp token.cpp parser.cpp macro.cpp directives.cpp pp-expr.cpp semantics.cpp cfg.cpp cfg-dump.cpp tree.cpp cse.cpp constprop.cpp
               x86/codegen.cpp)

set(EXCLUDED_SOURCES token.cpp)


foreach(source ${c9})
    if(NOT source IN_LIST EXCLUDED_SOURCES)
        set_source_files_properties(${source} PROPERTIES COMPILE_FLAGS CMAKE_CXX_FLAGS)
    endif()
endforeach()

set_source_files_properties(token.cpp PROPERTIES COMPILE_FLAGS "-O2")


add_executable(check file-check.cpp)
