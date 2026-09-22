cmake_minimum_required(VERSION 4.3.0)

project(fortran_raytracer CXX C Fortran)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(fortran_raytracer
  main.f90
  raytracer.f90
  c_bindings.f90
  input.f90
  scenes.f90
  engine.f90
  canvas.c
)

include(FetchContent)

set(MINIFB_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)

FetchContent_Declare(
    minifb
    GIT_REPOSITORY https://github.com/emoon/minifb.git
    GIT_TAG        master

)
FetchContent_MakeAvailable(minifb)

FetchContent_Declare(
    stb_image_write
    URL      https://raw.githubusercontent.com/nothings/stb/master/stb_image_write.h
    DOWNLOAD_NO_EXTRACT TRUE
)
FetchContent_MakeAvailable(stb_image_write)

add_library(stb_image_write INTERFACE)

target_include_directories(stb_image_write INTERFACE
    "${stb_image_write_SOURCE_DIR}"
)

target_sources(stb_image_write INTERFACE
    "${stb_image_write_SOURCE_DIR}/stb_image_write.h"
)

target_compile_options(fortran_raytracer PRIVATE
  $<$<COMPILE_LANGUAGE:Fortran>:-funsigned>
  $<$<COMPILE_LANGUAGE:Fortran>:-fno-range-check>
  -O3
  -fopenmp
  -march=native
)

target_link_libraries(fortran_raytracer PRIVATE
  minifb
  stb_image_write
)

target_link_options(fortran_raytracer PRIVATE
  -fopenmp
)

if(CMAKE_EXPORT_COMPILE_COMMANDS)
  file(CREATE_LINK
        "${CMAKE_BINARY_DIR}/compile_commands.json"
        "${CMAKE_SOURCE_DIR}/compile_commands.json"
        SYMBOLIC
    )
endif()

