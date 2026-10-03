module c_bindings
  use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_int16_t
  implicit none(type, external)
  type , bind(c) :: c_animation
    type(c_ptr) :: data
    integer(c_int) :: height, width, frame_count
    integer(c_int16_t) :: bpp
  end type c_animation
  public
  interface
    function c_write_png(name, width, height, pixels) bind(C, name="write_png")
      use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char
      implicit none(type, external)
      character(c_char), intent(in) :: name(*)
      integer(c_int), value :: width, height
      type(c_ptr), value ::  pixels
      integer(c_int) :: c_write_png
    end function c_write_png

    function load_anim(name) bind(C, name="load_anim")
      use , intrinsic :: iso_c_binding, only: c_char
      import :: c_animation

      character(c_char), intent(in) :: name(*)
      type(c_animation) :: load_anim
    end function load_anim

    subroutine c_free(ptr) bind(C, name="free")
      use, intrinsic :: iso_c_binding, only:c_ptr
      type(c_ptr), value ::ptr
    end subroutine c_free

    function mfb_open_signed_int(name, width, height) bind(C, name="mfb_open_signed_int")
      use , intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int
      implicit none(type, external)
      character(c_char), intent(in) :: name(*)
      integer(c_int), value :: width, height
      type(c_ptr) :: mfb_open_signed_int
    end function mfb_open_signed_int

    subroutine mfb_close(window) bind(C, name="mfb_close")
      use , intrinsic :: iso_c_binding, only: c_ptr
      implicit none(type, external)
      type(c_ptr), value :: window
    end subroutine mfb_close

    function mfb_update(window, pixels) bind(C, name="mfb_update")
      use , intrinsic :: iso_c_binding, only: c_ptr, c_int
      implicit none(type, external)
      integer(c_int) :: mfb_update
      type(c_ptr), value :: window, pixels
    end function mfb_update

    subroutine mfb_set_keyboard_callback(window, callback) bind(C, name="mfb_set_keyboard_callback")
      use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_funptr
      implicit none(type, external)
      type(c_ptr), value :: window
      type(c_funptr), value :: callback
    end subroutine mfb_set_keyboard_callback

    subroutine usleep(usec) bind(C, name="usleep")
      use, intrinsic :: iso_c_binding, only:c_int32_t
      implicit none(type, external)
      integer(c_int32_t), value :: usec
    end subroutine usleep
  end interface
end module c_bindings
