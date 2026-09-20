cmake_minimum_required(VERSION 4.3.0)

project(fortran_raytracer C Fortran)

add_executable(fortran_raytracer fortran_raytracer.f90 canvas.c)

include(FetchContent)
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

target_compile_options(fortran_raytracer PRIVATE
  -funsigned
  -fno-range-check
  -O3
  -fopenmp
  -march=native
)

target_include_directories(fortran_raytracer PRIVATE
    ${stb_image_write_SOURCE_DIR}
)

target_link_libraries(fortran_raytracer PRIVATE
  minifb
)

target_link_options(fortran_raytracer PRIVATE
  -fopenmp
)

