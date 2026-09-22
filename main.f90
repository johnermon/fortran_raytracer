program fortran_raytracer
  use engine, only: setup, run_once, close_engine
  implicit none(type, external)

  call setup()
  do
    !run once returns true on exit condition
    if(run_once()) exit
  end do
  call close_engine()
end program fortran_raytracer
