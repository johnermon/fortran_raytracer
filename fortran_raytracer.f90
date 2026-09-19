program fortran_raytracer
  use , intrinsic :: iso_c_binding, only:c_char, c_int
  implicit none(type, external)
  integer(c_int), parameter :: width = 1024, height = 1024
  character(kind=c_char), allocatable, target ::  canvas(:,:,:)

  allocate(canvas(3,width,height))
  call draw_canvas(width, height, canvas)
  call generate_png("output.png", width, height, canvas)
  deallocate(canvas)

  contains
    subroutine generate_png(name, width, height, canvas)
      use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_loc, c_null_char
      character(len=*), intent(in):: name
      character(kind=c_char, len=:), allocatable :: c_name
      integer(c_int), intent(in):: width, height
      character(kind=c_char), allocatable, target, intent(in)::  canvas(:,:,:)
      type(c_ptr) ::  pixels
      integer(c_int):: is_valid

      interface
        function c_generate_png(name, width, height, pixels) bind(C, name="save_canvas")
          use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_char
          implicit none(type, external)
          integer(c_int) :: c_generate_png
          character(c_char), intent(in) :: name(*)
          integer(c_int), value, intent(in):: width, height
          type(c_ptr), value, intent(in) ::  pixels
        end function c_generate_png
      end interface

      pixels = c_loc(canvas(1,1,1))

      c_name = name // c_null_char
      is_valid = c_generate_png(c_name, width,height, pixels)

      if(is_valid == 0) then
        print *, "invalid png file"
      end if

    end subroutine generate_png

    subroutine draw_canvas(width, height, canvas)
      integer(c_int), intent(in) :: width, height
      character(c_char), allocatable, target ,intent(inout) :: canvas(:,:,:)
      integer :: i, j
      do concurrent(i=1:width, j=1:height)
        canvas(1, i, j) = char(255, kind=c_char)
        canvas(2, i, j) = char(255, kind=c_char)
        canvas(3, i, j) = char(255, kind=c_char)
      end do
    end subroutine draw_canvas
end program fortran_raytracer
