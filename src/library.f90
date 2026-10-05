module library
  use, intrinsic :: iso_c_binding, only: c_size_t, c_int, c_int16_t, c_ptr, c_null_ptr
  use, intrinsic :: iso_fortran_env, only:uint8
  implicit none(type, external)
  real, parameter :: up_vec(3) = [0.0, 0.0, 1.0]
  type :: animation
    unsigned(uint8), contiguous, pointer :: data(:,:,:,:) => null()
    integer:: height = 0, width = 0, frame_count= 0, framerate = 0, frame_state = 1
  end type animation

  public

  contains

    function load_animation(name, framerate) result(anim)
      use , intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int, c_loc, c_null_char, c_associated, c_f_pointer
      use, intrinsic :: iso_fortran_env, only:uint8
      use c_bindings, only:load_anim, c_animation
      character(len=*), intent(in) :: name
      character(kind=c_char, len=:), allocatable:: c_name
      type(c_animation) :: c_anim
      type(animation) :: anim
      integer :: height, width, bpp, frame_count, framerate

      c_name = "assets/" // trim(name) // c_null_char
      c_anim = load_anim(c_name)

      if(.not.c_associated(c_anim%data)) then
        print *, "failed to generate animation, dir ", name, " may not exist as a directory"
        error stop
      end if

      height = int(c_anim%height)
      width = int(c_anim%width)
      frame_count = int(c_anim%frame_count)
      bpp = 4

      anim%width = width
      anim%height = height
      anim%frame_count =  frame_count
      anim%framerate = framerate
      
      call c_f_pointer(c_anim%data, anim%data,[bpp, width, height, frame_count])

    end function load_animation

    subroutine unload_animation(anim)
      use, intrinsic :: iso_c_binding, only:c_loc
      use c_bindings, only:c_free
      type(animation), target, intent(in) :: anim
      call c_free(c_loc(anim%data(1,1,1,1)))
    end subroutine unload_animation

    pure function compute_cross_product(u, v) result(p)
      real, intent(in) :: u(3), v(3)
      real :: p(3)

      p(1) = u(2) * v(3) - u(3) * v(2)
      p(2) = u(3) * v(1) - u(1) * v(3)
      p(3) = u(1) * v(2) - u(2) * v(1)
    end function compute_cross_product

    pure subroutine normalize_in_place(p, v)
      real, intent(in) :: v(3)
      real, intent(out):: p(3)
      p = v / sqrt(dot_product(v, v))
    end subroutine normalize_in_place

    pure function normalize(v) result(p)
      real, intent(in) :: v(3)
      real :: p(3)
      p = v / sqrt(dot_product(v, v))
    end function normalize
end module library
