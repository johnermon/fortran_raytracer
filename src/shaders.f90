module shaders
  use library, only:compute_cross_product, normalize, animation, load_animation
  use, intrinsic :: iso_fortran_env, only:uint8
  implicit none(type, external)
  public
  integer, parameter :: apply_animation = 1, rainbow = 2, checkerboard = 3, mandlebrot = 4,&
                        burningship = 5, powertower = 6, badapple = 7

  integer :: frame_cnt

  contains
    subroutine setup_shaders()
      frame_cnt = 0
    end subroutine setup_shaders

    subroutine shader_frame_cnt_incr()
      frame_cnt = frame_cnt + 1
    end subroutine shader_frame_cnt_incr

    !this funcion is static dispatch for shaders. i tried function pointers for runtime
    !polymorphism it really messed up performance, this seems like a pretty good compromise
    pure subroutine apply_shader(&
      shader, color, origin, point, h_vec, v_vec, param, animations)
      real, intent(in) :: origin(3), point(3), h_vec(3), v_vec(3)
      type(animation), contiguous, intent(in) :: animations(:)
      integer, value :: shader, param
      unsigned(uint8), intent(inout) :: color(4)
      unsigned(uint8), parameter :: blank(4) = [0u,0u,0u,0u]
      select case (shader)
        case (apply_animation)
            call animation_shader(&
              color, origin, point, h_vec, v_vec, param, animations&
            )
        case (rainbow)
           call rainbow_shader(color, origin, point)
        case (checkerboard)
          call checkerboard_shader(color, origin, point, h_vec, v_vec)
        case (mandlebrot)
          call mandlebrot_shader(color, origin, point, h_vec, v_vec)
        case (burningship)
          call burningship_shader(color, origin, point, h_vec, v_vec)
        case (powertower)
          call powertower_shader(color, origin, point, h_vec, v_vec)
        case default
          color =  blank
      end select
    end subroutine apply_shader

    pure subroutine rainbow_shader(color, origin, point)
      use, intrinsic :: iso_fortran_env, only:uint8
      real, intent(in) :: origin(3), point(3)
      unsigned(uint8), intent(inout) :: color(4)
      real, parameter :: pi = 4.0 * atan(1.0)
      real :: scratch
      scratch = dot_product(origin - point, origin - point)

      color(1) = uint(floor(sin(10.2 * scratch/10000)))
      color(2) = uint(floor(sin(10.2 * (scratch/10000 - pi/3))))
      color(3) = uint(floor(sin(10.2 * (scratch/10000- (2*pi)/3))))
      color(4) = 255u
    end subroutine rainbow_shader

    pure subroutine checkerboard_shader(color, origin, point, h_vec, v_vec)
      use, intrinsic :: iso_fortran_env, only:uint8
      real, intent(in) :: origin(3), point(3), h_vec(3), v_vec(3)
      unsigned(uint8), intent(inout) :: color(4)
      unsigned(uint8) :: colorin(4)
      real :: x1, y1
      integer :: i
      colorin = color
      x1 = abs(dot_product(origin - point, h_vec) / 1000)
      y1 = abs(dot_product(origin - point,v_vec) / 1000)
      do i=1, 5
        x1 = mod(3 * x1, 3.0)
        y1 = mod(3 * y1, 3.0)
        if(1<x1.and.x1<2.and.1<y1.and.y1<2) then
          color = colorin / 3u
          exit
        end if
      end do
    end subroutine checkerboard_shader

    pure subroutine mandlebrot_shader(color, origin, point, h_vec, v_vec)
      use, intrinsic :: iso_fortran_env, only:uint8
      real, intent(in) :: origin(3), point(3), h_vec(3), v_vec(3)
      unsigned(uint8), intent(inout) :: color(4)
      unsigned(uint8) :: colorin(4)
      integer, parameter :: iterations = 35
      integer :: i
      complex :: z
      colorin = color
      z = cmplx(0, 0)
      do i=0, iterations
        z = z ** 2 + cmplx((dot_product(origin - point, h_vec)) / 200, dot_product(origin - point,v_vec) / 200)
      if(4 < real(z) ** 2 + aimag(z) ** 2) then
        color(1) = uint(floor(real(colorin(1))/iterations * i, kind=uint8))
        color(2) = uint(floor(real(colorin(2))/iterations * i, kind=uint8))
        color(3) = uint(floor(real(colorin(3))/iterations * i, kind=uint8))
        color(4) = 255u
        return
      end if
        color(1) = 0u
        color(2) = 0u
        color(3) = 0u
        color(4) = 255u
      end do
    end subroutine mandlebrot_shader

    pure subroutine burningship_shader(color, origin, point, h_vec, v_vec)
      use, intrinsic :: iso_fortran_env, only:uint8
      real, intent(in) :: origin(3), point(3), h_vec(3), v_vec(3)
      unsigned(uint8), intent(inout) :: color(4)
      unsigned(uint8) :: colorin(4)
      integer, parameter :: iterations = 35
      integer :: i
      complex :: z
      colorin = color
      z = cmplx(0, 0)
      do i=0, iterations
        z = cmplx(abs(real(z)), abs(aimag(z))) ** 2 +&
        cmplx((dot_product(origin - point, h_vec)) / 200, dot_product(origin - point,v_vec) / 200)
      if(4 < real(z) ** 2 + aimag(z) ** 2) then
        color(1) = uint(floor(real(colorin(1))/iterations * i, kind=uint8))
        color(2) = uint(floor(real(colorin(2))/iterations * i, kind=uint8))
        color(3) = uint(floor(real(colorin(3))/iterations * i, kind=uint8))
        color(4) = 255u
        return
      end if
        color(1) = 0u
        color(2) = 0u
        color(3) = 0u
        color(4) = 255u
      end do
    end subroutine burningship_shader

    pure subroutine powertower_shader(color, origin, point, h_vec, v_vec)
      use, intrinsic :: iso_fortran_env, only:uint8
      real, intent(in) :: origin(3), point(3), h_vec(3), v_vec(3)
      unsigned(uint8), intent(inout):: color(4)
      unsigned(uint8) :: colorin(4)
      integer, parameter :: iterations = 8
      integer :: i
      complex :: z, num
      colorin = color
      num = cmplx((dot_product(origin - point, h_vec)) / 200, dot_product(origin - point,v_vec) / 200)
      z = num
      do i=0, iterations
        z = z ** num
      if(5.539574177 < real(z) ** 2 + aimag(z) ** 2) then
        color(1) = uint(floor(real(color(1))/iterations * i, kind=uint8))
        color(2) = uint(floor(real(color(2))/iterations * i, kind=uint8))
        color(3) = uint(floor(real(color(3))/iterations * i, kind=uint8))
        color(4) = 255u
        return
      end if
        color(1) = 0u
        color(2) = 0u
        color(3) = 0u
        color(4) = 255u
      end do
    end subroutine powertower_shader

    pure subroutine animation_shader(color, origin, point, h_vec, v_vec, param, anims)
      use, intrinsic :: iso_fortran_env, only:uint8
      real, intent(in) :: origin(3), point(3), h_vec(3), v_vec(3)
      integer, value :: param
      integer :: x, y
      type(animation),contiguous, intent(in) :: anims(:)
      type(animation) :: anim
      unsigned(uint8), intent(inout):: color(4)
      unsigned(uint8) :: colorin(4)
      colorin = color
      
      x = int(floor(abs(dot_product(origin - point, h_vec))))
      y = int(floor(abs(dot_product(origin - point, v_vec))))

      associate(anim => anims(param))
        color = anim%data(:,&
          1 + mod(x, anim%width),&
          1 + mod(y,anim%height),&
          1 + int(floor(real(mod(frame_cnt, anim%frame_count))))&
        )
      end associate

      end subroutine animation_shader
end module shaders
