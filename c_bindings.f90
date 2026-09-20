module c_bindings
  contains
    !outputs the canvas to a png file via c ffi
    subroutine generate_png(name, width, height, canvas)
      use , intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int, c_loc, c_null_char
      use, intrinsic :: iso_fortran_env, only:uint8
      character(len=*), intent(in) :: name
      integer(uint8), contiguous ,target, intent(in) ::  canvas(:,:,:)
      integer(c_int), intent(in), value :: width, height
      character(kind=c_char, len=:), allocatable :: c_name
      type(c_ptr) ::  pixels

      interface
        function c_generate_png(name, width, height, pixels) bind(C, name="save_canvas")
          use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char
          implicit none(type, external)
          integer(c_int) :: c_generate_png
          character(c_char), intent(in) :: name(*)
          integer(c_int), value, intent(in) :: width, height
          type(c_ptr), value, intent(in) ::  pixels
        end function c_generate_png
      end interface

      pixels = c_loc(canvas(1,1,1))

      c_name = name // c_null_char

      if(c_generate_png(c_name, width,height, pixels) == 0) then
        print *, "failed to generate png file\n"
      end if
    end subroutine generate_png

    function open_window(name, width, height) result(window)
      use , intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int, c_null_char, c_null_ptr, c_associated
      character(len=*), intent(in) :: name
      integer(c_int), intent(in), value :: width, height
      character(kind=c_char, len=:), allocatable :: c_name
      type(c_ptr) :: window

      interface
        function c_open_window(name, width, height) bind(C, name="open_window")
          use , intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int
          implicit none(type, external)
          type(c_ptr) :: c_open_window
          character(c_char), intent(in) :: name(*)
          integer(c_int), intent(in), value :: width, height
        end function c_open_window
      end interface
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
      interface
        subroutine c_close_window(window) bind(C, name="close_window")
          use , intrinsic :: iso_c_binding, only: c_ptr
          implicit none(type, external)
          type(c_ptr), intent(in) :: window
        end subroutine c_close_window
      end interface
      call c_close_window(window)
    end subroutine close_window

end module c_bindings
