module engine
  use, intrinsic :: iso_c_binding, only: c_size_t, c_int, c_ptr
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
      procedure :: update => update_accumulator, wait => accumulator_wait, setup => accumulator_setup
  end type accumulator

  unsigned(uint8), allocatable, target::  canvas(:,:,:)
  type(scene) :: curr_scene
  type(c_ptr) :: window
  type(accumulator) :: acc

  integer , parameter :: framerate = 120
  integer , parameter :: width = 1600, height = 900

  integer :: frame_count

  public
  contains
  subroutine setup()
    use , intrinsic :: iso_c_binding, only:c_null_ptr, c_funloc, c_null_char
    use c_bindings, only:mfb_set_keyboard_callback, c_write_png, usleep
    window = c_null_ptr
    curr_scene = fractal_planetarium()
    frame_count = 1
    call curr_scene%set_resolution(width, height)
    allocate(canvas(4,curr_scene%width,curr_scene%height))
    call open_window("Fortran Raytracer", curr_scene%width, curr_scene%height)
    call mfb_set_keyboard_callback(window, c_funloc(update_input))
    call acc%setup()
  end subroutine setup

  function run_once() result(quit)
    use c_bindings, only:usleep
    use raytracer, only:trace_rays
    
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

    call trace_rays(curr_scene, canvas)

    call update_window()

    if(was_just_pressed(p)) call handle_screenshot("output.png", width,height, canvas)

    if(was_just_pressed(c)) print *,&
      "x:", curr_scene%camera_pos(1), "y:", curr_scene%camera_pos(2), "z:", curr_scene%camera_pos(3)
  end function run_once

  subroutine update_state()
    use input, only: keyboard_get_rotation,keyboard_get_dir
    integer :: i
    real :: framerate_adj
    frame_count = frame_count + 1
    do i=1, size(curr_scene%animations)
      framerate_adj = (real(curr_scene%animations(i)%framerate) / real(framerate))
      curr_scene%animations(i)%frame_state = 1 +&
        int(mod(framerate_adj * real(frame_count), real(curr_scene%animations(i)%frame_count)))
      end do
    call curr_scene%move_camera(keyboard_get_dir() / real(framerate))
    call curr_scene%rotate_camera(keyboard_get_rotation() / real(framerate))
  end subroutine update_state

  subroutine close_engine()
    use , intrinsic :: iso_c_binding, only: c_ptr
    use c_bindings, only:mfb_close
    use library, only:unload_animation
    integer :: i
    call mfb_close(window)
    deallocate(canvas)
    do i = 1, size(curr_scene%animations)
      call unload_animation(curr_scene%animations(i))
    end do
  end subroutine close_engine


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

  subroutine handle_screenshot(name, width, height, canvas_in)
    use , intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int, c_loc, c_null_char
    use, intrinsic :: iso_fortran_env, only:uint8
    use c_bindings, only:c_write_png
    unsigned(uint8), target::  canvas_in(:,:,:)

    character(len=*), intent(in) :: name
    integer(c_int), intent(in), value :: width, height

    character(kind=c_char, len=:), allocatable:: c_name
    unsigned(uint8), allocatable, target, save::  tmp_canvas(:,:,:)
    type(c_ptr) ::  pixels

    c_name = name // c_null_char

    allocate(tmp_canvas(4,width,height))

    tmp_canvas = canvas_in([3,2,1,4], :,:)

    pixels = c_loc(tmp_canvas(1,1,1))

    if(c_write_png(c_name, width,height, pixels) == 0) then
      print *, "failed to generate png file\n"
      error stop
    end if

    deallocate(tmp_canvas)
  end subroutine handle_screenshot

  subroutine accumulator_setup(this)
    class(accumulator), intent(inout) :: this
    !kinda hacky but  sets it up to accumulate things up properly going forward.
    !if it works and its stupid its not stupid
    call acc%update()
    acc%frames_accumulated = 0
  end subroutine accumulator_setup

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
        rem_time = int(&
          ( 1000000.0 / real(framerate)) * ( frame_delta - real(frames_accumulated)),&
          kind=c_int32_t&
        )
        count_prev = count
    end associate
  end subroutine update_accumulator

  subroutine accumulator_wait(this)
    use c_bindings, only:usleep
    class(accumulator), intent(inout) :: this
    call usleep(this%rem_time)
  end subroutine accumulator_wait

end module engine
