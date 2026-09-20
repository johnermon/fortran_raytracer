#/bin/env bash
gfortran -funsigned -fno-range-check -O3 -fopenmp -march=native fortran_raytracer.f90 c_canvas.o -o raytracer && OMP_NUM_THREADS=$(nproc) OMP_PROC_BIND=true ./raytracer && timg output.png
