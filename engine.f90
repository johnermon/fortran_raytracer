module engine
  use , intrinsic :: iso_c_binding, only: c_ptr
  use, intrinsic :: iso_fortran_env, only:uint8
  use raytracer, only:scene
  use input
  use scenes

  implicit none(type, external)
  integer(uint8), allocatable, target::  canvas(:,:,:)
  type(scene) :: curr_scene
  type(c_ptr) :: window
  integer , parameter :: framerate = 60

  private
  public :: run_once, setup, close_engine
  contains

  subroutine setup()
    use , intrinsic :: iso_c_binding, only:c_null_ptr, c_funloc
    use c_bindings, only:mfb_set_keyboard_callback
    window = c_null_ptr


    curr_scene = rgb_test()
    allocate(canvas(4,curr_scene%width,curr_scene%height))
    call open_window("Fortran Raytracer", curr_scene%width, curr_scene%height)
    call mfb_set_keyboard_callback(window, c_funloc(update_input))
  end subroutine setup

  function run_once() result(quit)
    use, intrinsic:: iso_c_binding, only:c_int32_t
    use c_bindings, only:usleep
    integer, save :: count_prev = 0

    logical :: quit
    integer :: count, rate
    real, parameter :: framerate_usec = 1000000.0 / real(framerate)
    real :: frame_delta
    quit = .false.
    call system_clock(count, rate)
    frame_delta = (real(count) - real(count_prev)) / real(rate) * real(framerate)
    count_prev = count

    if(is_pressed(esc)) then
      quit = .true.
      return
    end if

    do
      if(frame_delta < 1.0) exit

      call curr_scene%move_camera(keyboard_get_dir())
      call curr_scene%rotate_camera(keyboard_get_rotation())
      frame_delta = frame_delta - 1.0
    end do

    call usleep(int(framerate_usec * frame_delta,kind=c_int32_t))

    call curr_scene%trace_rays(canvas)

    call update_window()

    if(is_pressed(p)) then
      call generate_png("output.png", curr_scene%width,curr_scene%height)
      call usleep(500000)
    end if
  end function run_once

  subroutine open_window(name, width, height)
    use , intrinsic ::iso_c_binding, only:&
      c_char, c_ptr, c_int, c_null_char, c_null_ptr, c_associated
    use c_bindings, only:mfb_open_signed_int

    character(len=*), intent(in) :: name
    integer(c_int), intent(in), value :: width, height
    character(kind=c_char, len=:), allocatable :: c_name

    c_name = name // c_null_char

    window = mfb_open_signed_int(c_name, width,height)

    if(.not. c_associated(window)) then
      print*, "failed to open window"
      error stop
    end if
  end subroutine open_window

  subroutine update_window()
    use , intrinsic :: iso_c_binding, only: c_ptr, c_loc
    use c_bindings, only:mfb_update

    type(c_ptr) ::  pixels
    pixels = c_loc(canvas(1,1,1))
    ! for future me, get to proper error handling here
    !
    ! MFB_STATE_OK             =  0,
    ! MFB_STATE_EXIT           = -1,
    ! MFB_STATE_INVALID_WINDOW = -2,
    ! MFB_STATE_INVALID_BUFFER = -3,
    ! MFB_STATE_INTERNAL_ERROR = -4,

    if(.not. (mfb_update(window, pixels) == 0)) then
      print *, "failed to update window\n"
      error stop
    end if
  end subroutine update_window

  subroutine close_engine()
    use , intrinsic :: iso_c_binding, only: c_ptr
    use c_bindings, only:mfb_close
    call mfb_close(window)
    deallocate(canvas)
  end subroutine close_engine

  subroutine generate_png(name, width, height)
    use , intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int, c_loc, c_null_char
    use, intrinsic :: iso_fortran_env, only:uint8
    use c_bindings, only:c_write_png

    character(len=*), intent(in) :: name
    integer(c_int), intent(in), value :: width, height

    character(kind=c_char, len=:), allocatable :: c_name
    integer(uint8), allocatable, target ::  tmp_canvas(:,:,:)
    type(c_ptr) ::  pixels

    c_name = name // c_null_char

    allocate(tmp_canvas(4,width,height))

    tmp_canvas = canvas([3,2,1,4], :,:)

    pixels = c_loc(tmp_canvas(1,1,1))

    if(c_write_png(c_name, width,height, pixels) == 0) then
      print *, "failed to generate png file\n"
      error stop
    end if

    deallocate(tmp_canvas)
  end subroutine generate_png

end module engine
