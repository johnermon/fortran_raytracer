module shaders
  use library, only:compute_cross_product, normalize
  implicit none(type, external)
  public
  integer, parameter :: rainbow = 1

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
        case (0)
          color = colorin
        case (rainbow)
          color = rainbow_shader(colorin, origin, point)
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
      scratch = mod(&
        floor(&
          sqrt(&
            dot_product(&
              origin - point, origin - point&
            )&
          )&
        ),&
        50&
      )

      color(1) = floor(sin(10.2 * real(scratch)))
      color(2) = floor(sin(10.2 * (real(scratch)- pi/3)))
      color(3) = floor(sin(10.2 * (real(scratch) - (2*pi)/3)))
      color(4) = 255
    end function rainbow_shader
end module shaders
