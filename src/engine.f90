module engine
  use , intrinsic :: iso_c_binding, only: c_ptr
  use, intrinsic :: iso_fortran_env, only:uint8
  use raytracer, only:scene
  use input
  use scenes
  implicit none(type, external)

  type accumulator
    integer :: rate, rem_time, i, frames_accumulated
    real :: frame_delta
    integer:: count, count_prev
    contains
      procedure :: update => update_accumulator, wait => accumulator_wait
  end type accumulator

  integer(uint8), allocatable, target::  canvas(:,:,:)
  type(scene) :: curr_scene
  type(c_ptr) :: window
  type(accumulator) :: acc

  integer , parameter :: framerate = 60
  integer , parameter :: width = 1024, height = 1024

  private


  public :: run_once, setup, close_engine
  contains

  subroutine setup()
    use , intrinsic :: iso_c_binding, only:c_null_ptr, c_funloc
    use c_bindings, only:mfb_set_keyboard_callback
    window = c_null_ptr
    curr_scene = rgb_test()
    call curr_scene%set_resolution(width, height)

    allocate(canvas(4,curr_scene%width,curr_scene%height))
    call open_window("Fortran Raytracer", curr_scene%width, curr_scene%height)
    call mfb_set_keyboard_callback(window, c_funloc(update_input))
  end subroutine setup

  function run_once() result(quit)
    use c_bindings, only:usleep
    logical :: quit
    integer :: i
    quit = .false.

    if(is_pressed(esc)) then
      quit = .true.
      return
    end if


    call acc%update()

    do i = 1, acc%frames_accumulated
      call update_state()
    end do

    call acc%wait()

    call curr_scene%trace_rays(canvas)


    call update_window()

    call handle_screenshot("output.png", width,height)
  end function run_once

  subroutine update_state()
    use input, only: keyboard_get_rotation,keyboard_get_dir
    call curr_scene%move_camera(keyboard_get_dir())
    call curr_scene%rotate_camera(keyboard_get_rotation())
  end subroutine update_state

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

  subroutine handle_screenshot(name, width, height)
    use , intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int, c_loc, c_null_char
    use, intrinsic :: iso_fortran_env, only:uint8
    use c_bindings, only:c_write_png
    use input, only:p, was_just_pressed

    character(len=*), intent(in) :: name
    integer(c_int), intent(in), value :: width, height

    character(kind=c_char, len=:), allocatable, save:: c_name
    integer(uint8), allocatable, target, save::  tmp_canvas(:,:,:)
    type(c_ptr) ::  pixels

    if(.not.was_just_pressed(p)) return

    c_name = name // c_null_char

    allocate(tmp_canvas(4,width,height))

    tmp_canvas = canvas([3,2,1,4], :,:)

    pixels = c_loc(tmp_canvas(1,1,1))

    if(c_write_png(c_name, width,height, pixels) == 0) then
      print *, "failed to generate png file\n"
      error stop
    end if

    deallocate(tmp_canvas)
  end subroutine handle_screenshot

  subroutine update_accumulator(this)
      use, intrinsic:: iso_c_binding, only:c_int32_t
      class(accumulator), intent(inout) :: this
      real :: frame_delta
      associate(&
        count => this%count,&
        rate => this%rate,&
        rem_time => this%rem_time,&
        frames_accumulated => this%frames_accumulated,&
        count_prev => this%count_prev&
      )
        call system_clock(count, rate)
        frame_delta = (real(count) - real(count_prev)) / real(rate) * real(framerate)
        frames_accumulated = floor(frame_delta)
        rem_time = int(( 1000000.0 / real(framerate))* ( frame_delta - real(frames_accumulated)), kind=c_int32_t)
        count_prev = count
    end associate
  end subroutine update_accumulator

  subroutine accumulator_wait(this)
    use c_bindings, only:usleep
    class(accumulator), intent(inout) :: this
    call usleep(this%rem_time)
  end subroutine accumulator_wait

end module engine
