module shaders
  use library, only:compute_cross_product, normalize
  implicit none(type, external)
  public
  integer, parameter :: rainbow = 1, checkerboard = 2, mandlebrot = 3, burningship = 4

  contains
    !this funcion is static dispatch for shaders. i tried function pointers for runtime
    !polymorphism it really messed up performance, this seems like a pretty good compromise
    pure function apply_shader(shader, colorin, origin, point) result(color)
      use, intrinsic :: iso_fortran_env, only:uint8
      real, intent(in) :: origin(3), point(3)
      integer, intent(in) :: shader
      integer(uint8), intent(in) :: colorin(4)
      integer(uint8) :: color(4)
      integer(uint8), parameter :: blank(4) = [0_uint8,0_uint8,0_uint8,0_uint8]
      select case (shader)
        case (rainbow)
          color = rainbow_shader(colorin, origin, point)
        case (checkerboard)
          color = checkerboard_shader(colorin, origin, point)
        case (mandlebrot)
          color = mandlebrot_shader(colorin, origin, point)
        case (burningship)
          color = burningship_shader(colorin, origin, point)
        case default
          color =  blank
      end select
    end function apply_shader

    pure function rainbow_shader(colorin, origin, point) result(color)
      use, intrinsic :: iso_fortran_env, only:uint8
      real, intent(in) :: origin(3), point(3)
      integer(uint8), intent(in) :: colorin(4)
      integer(uint8) :: color(4)
      real, parameter :: pi = 4.0 * atan(1.0)
      real :: scratch
      scratch = dot_product(origin - point, origin - point)

      color(1) = floor(sin(10.2 * scratch/10000))
      color(2) = floor(sin(10.2 * (scratch/10000 - pi/3)))
      color(3) = floor(sin(10.2 * (scratch/10000- (2*pi)/3)))
      color(4) = 255
    end function rainbow_shader

    pure function checkerboard_shader(colorin, origin, point) result(color)
      use, intrinsic :: iso_fortran_env, only:uint8
      real, intent(in) :: origin(3), point(3)
      integer(uint8), intent(in) :: colorin(4)
      integer(uint8) :: color(4)
      real :: x1, y1
      integer :: i
      color = colorin
      x1 = abs(point(1) / 1000)
      y1 = abs(point(2) / 1000)
      do i=1, 6
        x1 = mod(3 * x1, 3.0)
        y1 = mod(3 * y1, 3.0)
        if(1<x1.and.x1<2.and.1<y1.and.y1<2) then
          color = [106, 42, 101, 255]
          exit
        end if
      end do
    end function checkerboard_shader

    pure function mandlebrot_shader(colorin, origin, point) result(color)
      use, intrinsic :: iso_fortran_env, only:uint8
      real, intent(in) :: origin(3), point(3)
      integer(uint8), intent(in) :: colorin(4)
      integer(uint8) :: color(4)
      integer, parameter :: iterations = 35
      integer :: i
      complex :: z
      color = [0,0,0,0]
      z = cmplx(0, 0)
      do i=1, iterations
        z = z ** 2 + cmplx(point(1)/200, point(2)/200)
      if(4 < real(z) ** 2 + aimag(z) ** 2) then
        color(1) = int(122.0/iterations * i)
        color(2) = int(255.0/iterations * i)
        color(3) = 0
        color(4) = 255
        return
      end if
        color = [0,0,0,255]
      end do
    end function mandlebrot_shader

    pure function burningship_shader(colorin, origin, point) result(color)
      use, intrinsic :: iso_fortran_env, only:uint8
      real, intent(in) :: origin(3), point(3)
      integer(uint8), intent(in) :: colorin(4)
      integer(uint8) :: color(4)
      integer, parameter :: iterations = 35
      integer :: i
      complex :: z
      z = cmplx(0, 0)
      do i=1, iterations
        z = cmplx(abs(real(z)), abs(aimag(z))) ** 2 + cmplx(point(1)/200, point(2)/200)
      if(4 < real(z) ** 2 + aimag(z) ** 2) then
        color(1) = 0
        color(2) = int(128.0/iterations * i)
        color(3) = int(255.0/iterations * i)
        color(4) = 255
        return
      end if
        color = [0,0,0,255]
      end do
    end function burningship_shader
end module shaders
