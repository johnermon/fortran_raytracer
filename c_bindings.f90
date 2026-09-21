module c_bindings
  implicit none(type, external)
  interface
    function c_generate_png(name, width, height, pixels) bind(C, name="save_canvas")
      use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char
      implicit none(type, external)
      integer(c_int) :: c_generate_png
      character(c_char), intent(in) :: name(*)
      integer(c_int), value :: width, height
      type(c_ptr), value ::  pixels
    end function c_generate_png

    function c_open_window(name, width, height) bind(C, name="mfb_open_singed_int")
      use , intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int
      implicit none(type, external)
      type(c_ptr) :: c_open_window
      character(c_char), intent(in) :: name(*)
      integer(c_int), value :: width, height
    end function c_open_window

    subroutine c_close_window(window) bind(C, name="mfb_close")
      use , intrinsic :: iso_c_binding, only: c_ptr
      implicit none(type, external)
      type(c_ptr), value :: window
    end subroutine c_close_window

    function c_update_window(window, pixels) bind(C, name="mfb_update")
      use , intrinsic :: iso_c_binding, only: c_ptr, c_int
      implicit none(type, external)
      integer(c_int) :: c_update_window
      type(c_ptr), value ::  pixels, window
    end function c_update_window

    subroutine c_register_io_callback(window, callback) bind(C, name="mfb_set_keyboard_callback")
      use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_funptr
      implicit none(type, external)
      type(c_ptr), value :: window
      type(c_funptr), value :: callback
    end subroutine c_register_io_callback

    subroutine usleep(usec) bind(C, name="usleep")
      use, intrinsic :: iso_c_binding, only:c_int32_t
      integer(c_int32_t), value :: usec
    end subroutine usleep
  end interface

  contains
    !outputs the canvas to a png file via c ffi
    subroutine generate_png(name, width, height, canvas)
      use , intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int, c_loc, c_null_char
      use, intrinsic :: iso_fortran_env, only:uint8
      character(len=*), intent(in) :: name
      integer(uint8), contiguous ,target, intent(in) ::  canvas(:,:,:)
      integer(uint8), allocatable, target ::  tmp_canvas(:,:,:)

      integer(c_int), intent(in), value :: width, height
      character(kind=c_char, len=:), allocatable :: c_name
      type(c_ptr) ::  pixels

      c_name = name // c_null_char

      allocate(tmp_canvas(4,width,height))

      tmp_canvas = canvas([3,2,1,4], :,:)

      pixels = c_loc(tmp_canvas(1,1,1))

      if(c_generate_png(c_name, width,height, pixels) == 0) then
        print *, "failed to generate png file\n"
        error stop
      end if

      deallocate(tmp_canvas)
    end subroutine generate_png

    function open_window(name, width, height) result(window)
      use , intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int, c_null_char, c_null_ptr, c_associated
      character(len=*), intent(in) :: name
      integer(c_int), intent(in), value :: width, height
      character(kind=c_char, len=:), allocatable :: c_name
      type(c_ptr) :: window

      c_name = name // c_null_char
      window = c_open_window(c_name, width,height)

      if(.not. c_associated(window)) then
        print*, "failed to open window"
        error stop
      end if

    end function open_window

    subroutine close_window(window)
      use , intrinsic :: iso_c_binding, only: c_ptr
      type(c_ptr), intent(in) :: window
      call c_close_window(window)
    end subroutine close_window

    subroutine update_window(window, canvas)
      use , intrinsic :: iso_c_binding, only: c_ptr, c_loc
      use, intrinsic :: iso_fortran_env, only:uint8
      integer(uint8), contiguous ,target, intent(in) ::  canvas(:,:,:)
      type(c_ptr) ::  pixels
      type(c_ptr), intent(in) :: window

      pixels = c_loc(canvas(1,1,1))
      ! for future me, get to proper error handling here
      !
      ! MFB_STATE_OK             =  0,
      ! MFB_STATE_EXIT           = -1,
      ! MFB_STATE_INVALID_WINDOW = -2,
      ! MFB_STATE_INVALID_BUFFER = -3,
      ! MFB_STATE_INTERNAL_ERROR = -4,

      if(.not. (c_update_window(window, pixels) == 0)) then
        print *, "failed to update window\n"
        error stop
      end if
    end subroutine update_window

    subroutine register_io_callback(window, callback)
      use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_funptr
      type(c_ptr), value :: window
      type(c_funptr), value :: callback
      call c_register_io_callback(window, callback)
    end subroutine register_io_callback

end module c_bindings
