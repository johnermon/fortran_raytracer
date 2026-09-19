module raytrace_params
  use , intrinsic :: iso_c_binding, only:c_int, c_char
  implicit none(type, external)
  integer(c_int), parameter :: width = 1024, height = 1024

  real, parameter :: camera_pos(3) = [0.0,0.0,0.0]
  real, parameter :: camera_vec(3) = [1.0,0.0,0.0]

  real, parameter :: up_vec(3) = [0.0, 0.0, 1.0]

  type :: sphere
    real :: radius, point(3)
    character(c_char) :: color(3)
  end type sphere

  type :: plane
    real:: point(3), normal(3)
    character(c_char) :: color(3)
  end type plane

  type :: scene
    type(plane) , allocatable :: planes(:)
    type(sphere) , allocatable :: spheres(:)
    character(c_char) :: sky_color(3)
  end type scene

  real :: h_vec(3), v_vec(3)
  type(scene) :: curr_scene
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

    !generates each pixel
    pure function get_raytraced_pixel(i, j) result(color)
      use, intrinsic :: iso_c_binding, only: c_char
      use raytrace_params
      integer, intent(in) :: i, j
      real :: i_com, j_com, ray(3)
      character(c_char) :: color(3)

      i_com = (real(2*i) / real(width)) - 1.0
      j_com = (real(2*j) / real(height)) - 1.0

      ray = normalize(camera_vec + i_com * h_vec + j_com * v_vec)

      color = get_ray_color(ray)
    end function get_raytraced_pixel

    pure function get_ray_color(r) result(color)
      use raytrace_params
      character(c_char) :: color(3)
      real, intent(in) :: r(3)
      real, parameter :: largest = huge(1.0)
      real :: smallest, curr
      integer :: i
      type(plane) :: curr_plane
      type(sphere) :: curr_sphere
      color = curr_scene%sky_color

      smallest = largest

      do i=1, sizeof(curr_scene%planes)
        curr_plane = curr_scene%planes(i)
        curr = get_plane_intersection(curr_plane, r, camera_pos)

        if(0.0 < curr .and. curr < smallest) then
          smallest = curr
          color = curr_plane%color
        end if
      end do

      !
      ! do i=1, sizeof(curr_scene%spheres)
      !   curr_sphere = curr_scene%spheres(i)
      !   curr = get_sphere_intersection(curr_sphere, r, camera_pos)
      !   if(curr < smallest) then
      !     smallest = curr
      !   end if
      ! end do
      !
    end function get_ray_color

    pure function get_sphere_intersection(curr_sphere, r, c)result(t)
      use raytrace_params, only:sphere
      type(sphere), intent(in) :: curr_sphere
      real, intent(in) :: r(3), c(3)
      real :: t

    end function get_sphere_intersection

    pure function get_plane_intersection(curr_plane, r, c)result(t)
      use raytrace_params, only:plane
      type(plane), intent(in) :: curr_plane
      real, intent(in) :: r(3), c(3)
      real :: t

      t = dot_product(curr_plane%point - c, curr_plane%normal) / dot_product(r, curr_plane%normal)
    end function get_plane_intersection

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
      p = (1 / norm2(v)) * v
    end function normalize

    !concurrently draws pixels on the canvas
    subroutine draw_canvas(canvas)
      use raytrace_params
      character(c_char), allocatable, target ,intent(inout) :: canvas(:,:,:)
      character(c_char) :: color(3)
      integer :: i, j

      h_vec = normalize(compute_cross_product(camera_vec, up_vec))
      v_vec = normalize(compute_cross_product(camera_vec, h_vec))

      curr_scene = scene(&
          [&!planes
            plane(&
              [0.0,0.0,-20.0],& !point
              [0.2,0.2,1.0],& !normal
              [achar(92, kind=c_char),achar(172, kind=c_char),achar(45, kind=c_char)]& !color
            )&
          ],&

          [& ! spheres
            sphere(&
              10,& ! radius
              [20.0, 0.0, 0.0],&
              [achar(255, kind=c_char),achar(0, kind=c_char),achar(0, kind=c_char)]&
            )&
          ],&

          !sky color
          [achar(107, kind=c_char),achar(221, kind=c_char),achar(229, kind=c_char)]&
        )

      do concurrent(i=1:width, j=1:height)
        color = get_raytraced_pixel(i,j)
        canvas(1, i, j) = color(1)
        canvas(2, i, j) = color(2)
        canvas(3, i, j) = color(3)
      end do
    end subroutine draw_canvas

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
