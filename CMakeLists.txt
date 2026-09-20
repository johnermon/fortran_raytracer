cmake_minimum_required(VERSION 4.3.0)

project(fortran_raytracer C Fortran)

add_executable(fortran_raytracer fortran_raytracer.f90 canvas.c)

target_compile_options(fortran_raytracer PRIVATE
  -funsigned
  -fno-range-check
  -O3
  -fopenmp
  -march=native
)

target_link_options(app fortran_raytracer -fopenmp)

