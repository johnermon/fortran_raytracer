program fortran_raytracer
  use , intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_funloc
  use, intrinsic :: iso_fortran_env, only:uint8
  use c_bindings, only :&
    close_window, open_window, generate_png, update_window, register_io_callback

  use raytracer, only:setup_params, trace_rays, scene
  use input, only:update_input

  implicit none(type, external)
  integer(uint8),allocatable ::  canvas(:,:,:)
  type(scene) :: curr_scene
  type(c_ptr) :: window
  window = c_null_ptr

  curr_scene = setup_params()

  allocate(canvas(4,curr_scene%width,curr_scene%height))

  window = open_window("Fortran Raytracer", curr_scene%width, curr_scene%height)

  call register_io_callback(window, c_funloc(update_input))

  do
    call trace_rays(curr_scene, canvas)
    call update_window(window, canvas)
    call sleep(1)
  end do

  call generate_png("output.png", curr_scene%width,curr_scene%height, canvas)

  deallocate(canvas)

  call close_window(window)
end program fortran_raytracer
