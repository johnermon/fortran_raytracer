module raytrace_params
  use , intrinsic :: iso_c_binding, only:c_int
  implicit none(type, external)
  integer(c_int), parameter :: width = 1024, height = 1024
  real, parameter :: camera_pos(3) = [0.0,0.0,0.0]
  real, parameter :: camera_dir(3) = [1.0,0.0,0.0]
  real :: h_vec(3)
  real :: v_vec(3)
end module raytrace_params

program fortran_raytracer
  use , intrinsic :: iso_c_binding, only:c_char, c_int
  use raytrace_params

  implicit none(type, external)
  character(kind=c_char), allocatable, target ::  canvas(:,:,:)


  allocate(canvas(3,width,height))
  call draw_canvas(canvas)
  call generate_png("output.png", canvas)
  deallocate(canvas)

  contains
    subroutine draw_canvas(canvas)
      use raytrace_params
      character(c_char), allocatable, target ,intent(inout) :: canvas(:,:,:)
      character(c_char) :: color(3)
      integer :: i, j

      do concurrent(i=1:width, j=1:height)
        color = get_raytraced_pixel(i,j)
        canvas(1, i, j) = color(1)
        canvas(2, i, j) = color(2)
        canvas(3, i, j) = color(3)
      end do
    end subroutine draw_canvas

    !generates each pixel
    pure function get_raytraced_pixel(i, j) result(color)
      use, intrinsic :: iso_c_binding, only: c_char
      use raytrace_params
      integer, intent(in) :: i, j
      character(c_char) :: color(3)

      color(1) = char(modulo(i * i * 3 + j * j * 7, 256), kind=c_char)
      color(2) = char(modulo(ieor(i, j) * 19 + (i * j), 256), kind=c_char)
      color(3) = char(modulo(ishft(i - j, 2) + ior(i, j), 256), kind=c_char)
    end function get_raytraced_pixel

    !outputs the canvas to a png file via c ffi
    subroutine generate_png(name, canvas)
      use raytrace_params
      use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_loc, c_null_char

      character(len=*), intent(in) :: name
      character(kind=c_char), allocatable, target, intent(in) ::  canvas(:,:,:)
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

end program fortran_raytracer
