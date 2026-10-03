module library
  use, intrinsic :: iso_c_binding, only: c_size_t, c_int, c_int16_t, c_ptr
  use, intrinsic :: iso_fortran_env, only:uint8
  implicit none(type, external)
  real, parameter :: up_vec(3) = [0.0, 0.0, 1.0]
  type :: animation
    unsigned(uint8), pointer :: data(:,:,:,:) => null()
    integer :: height, width, frame_count
  end type animation

  public

  contains

  function load_animation(name) result(anim)
    use , intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int, c_loc, c_null_char, c_associated, c_f_pointer
    use, intrinsic :: iso_fortran_env, only:uint8
    use c_bindings, only:load_anim, c_animation
    character(len=*), intent(in) :: name
    character(kind=c_char, len=:), allocatable:: c_name
    type(c_animation) :: c_anim
    type(animation) :: anim

    c_name = "assets/" // name // c_null_char
    c_anim = load_anim(c_name)

    anim%width = int(c_anim%width)
    anim%height = int(c_anim%height)
    anim%frame_count = int(c_anim%frame_count)

    call c_f_pointer(c_anim%data, anim%data, [&
      int(c_anim%bpp / 8), int(c_anim%width), int(c_anim%height),int(c_anim%frame_count)&
    ])

    if(.not.c_associated(c_anim%data)) then
      print *, "failed to generate animation, dir ", name, " may not exist as a directory"
      error stop
    end if
  end function load_animation
    pure function compute_cross_product(u, v) result(p)
      real, intent(in) :: u(3), v(3)
      real :: p(3)

      p(1) = u(2) * v(3) - u(3) * v(2)
      p(2) = u(3) * v(1) - u(1) * v(3)
      p(3) = u(1) * v(2) - u(2) * v(1)
    end function compute_cross_product

    pure function normalize(v) result(p)
      real, intent(in) :: v(3)
      real :: p(3)
    p = v / sqrt(dot_product(v, v))
    end function normalize
end module library
