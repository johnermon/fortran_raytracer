add_executable(fortran_raytracer
  app/main.f90 #main

  ffi/c_bindings.f90 #ffi stuff
  ffi/canvas.c

  raytracer.f90
  input.f90
  scenes.f90
  engine.f90
)
