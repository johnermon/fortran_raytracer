program fortran_raytracer
  use , intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_funloc
  use, intrinsic :: iso_fortran_env, only:uint8
  use c_bindings, only :&
    close_window, open_window, generate_png, update_window, register_io_callback, usleep
  use raytracer, only:create_scene,create_plane,&
  trace_rays, scene, move_camera, rotate_camera, plane, sphere
  use input

  implicit none(type, external)
  integer(uint8), allocatable ::  canvas(:,:,:)
  type(scene) :: curr_scene
  type(c_ptr) :: window
  window = c_null_ptr

  curr_scene = create_scene(&
    1024,1024,&
    [0.0,0.0,0.0], [1.0,1.0,1.0],&
    [229, 221, 107, 255],&
    [&!planes
      create_plane(&
        [0.0,0.0,-20.0],& !point
        [0.2,0.2,1.0],& !normal

        [0, 0, 255, 255]& !color
      ),&

      create_plane(&
        [0.0, 200.0,-20.0],& !point
        [-0.2,-0.2,1.0],& !normal

        [0, 255, 0, 255]& !color
      ),&
      create_plane(&
        [0.0, 200.0,-20.0],& !point
        [0.5,-0.4,1.0],& !normal

        [255, 0, 0, 255]& !color
      )&
    ],&

    [& ! spheres
      sphere(&
        10,& ! radius
        [20.0, 0.0, 0.0],&

        [0,0,255, 255]& !color
      )&
    ]&
  )

  allocate(canvas(4,curr_scene%width,curr_scene%height))

  window = open_window("Fortran Raytracer", curr_scene%width, curr_scene%height)

  call register_io_callback(window, c_funloc(update_input))

  do
    call trace_rays(curr_scene, canvas)
    call update_window(window, canvas)

    if(is_pressed(esc)) then
      exit
    end if

    if(is_pressed(p)) then
      call generate_png("output.png", curr_scene%width,curr_scene%height, canvas)
      call usleep(500000)
    end if

    call move_camera(curr_scene, keyboard_get_dir())
    call rotate_camera(curr_scene, keyboard_get_rotation())

    call usleep(16666)
  end do


  deallocate(canvas)

  call close_window(window)
end program fortran_raytracer
