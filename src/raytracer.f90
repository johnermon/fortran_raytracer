module raytracer
  use , intrinsic :: iso_c_binding, only:c_int
  use, intrinsic :: iso_fortran_env, only:uint8
  implicit none(type, external)
  private
  public :: trace_rays, scene, plane, sphere

  type :: scene
    integer(c_int) :: width , height
    real :: up_vec(3), camera_pos(3), camera_vec(3)
    real :: h_vec(3), v_vec(3)
    integer(uint8) :: sky_color(4)

    type(plane) , allocatable :: planes(:)
    type(sphere) , allocatable :: spheres(:)
    contains
      procedure :: &
        move_camera, rotate_camera,&
        trace_rays, get_ray_color, get_ray,&
        set_resolution => scene_set_resolution

  end type scene
  interface scene
    module procedure :: scene_new
  end interface scene

  type :: plane
    real:: point(3), normal(3)
    integer(uint8) :: color(4)
    contains
      procedure :: get_plane_intersection
  end type plane
  interface plane
    module procedure :: plane_new
  end interface plane

  type :: sphere
    real :: radius, point(3)
    integer(uint8) :: color(4)
    contains
      procedure :: get_sphere_intersection
  end type sphere


  contains
  !override constructor for scene, allows you to not have to worry about boilerplate
    pure function scene_new(camera_pos, camera_vec, sky_color, planes, spheres) result(new_scene)
      use, intrinsic :: iso_c_binding, only:c_int
      real, intent(in) :: camera_pos(3), camera_vec(3)
      integer, intent(in) :: sky_color(4)
      type(plane), intent(in) :: planes(:)
      type(sphere), intent(in) :: spheres(:)
      type(scene) :: new_scene

      real, parameter :: up_vec(3) = [0.0, 0.0, 1.0]
      real :: h_vec(3), v_vec(3)

      h_vec = normalize(compute_cross_product(camera_vec, up_vec))
      v_vec = normalize(compute_cross_product(camera_vec, h_vec))

      new_scene%width = 0
      new_scene%height = 0
      new_scene%up_vec = up_vec
      new_scene%camera_pos = camera_pos
      new_scene%camera_vec = camera_vec
      new_scene%h_vec = h_vec
      new_scene%v_vec = v_vec
      new_scene%sky_color = sky_color
      new_scene%planes = planes
      new_scene%spheres = spheres

    end function scene_new

    subroutine scene_set_resolution(this, width, height)
      use, intrinsic :: iso_c_binding, only:c_int
      class(scene), intent(inout) :: this
      integer(c_int), value :: width, height
      this%width = width
      this%height = height
    end subroutine scene_set_resolution

    !automatically normalizes whatever normal you pass into the plane
    pure function plane_new(point, normal, color) result(new_plane)
      real, intent(in) :: point(3), normal(3)
      integer, intent(in) :: color(4)
      type(plane) :: new_plane
      new_plane%point = point
      new_plane%normal = normalize(normal)
      new_plane%color = color
    end function plane_new

    subroutine move_camera(this, dir_vec)
      class(scene), intent(inout) :: this
      real, intent(in) :: dir_vec(3)
      associate(&
        camera_vec => this%camera_vec, h_vec => this%h_vec,&
        v_vec => this%v_vec, camera_pos => this%camera_pos&
      )
        camera_pos = camera_pos + normalize(camera_vec) * dir_vec(1)
        camera_pos = camera_pos + h_vec * dir_vec(2)
        camera_pos = camera_pos + v_vec * dir_vec(3)
      end associate
    end subroutine move_camera

    subroutine rotate_camera(this, rotation_vec)
      class(scene), intent(inout) :: this
      real, intent(in) :: rotation_vec(3)
      associate(camera_vec => this%camera_vec, h_vec => this%h_vec,v_vec => this%v_vec)
        camera_vec = normalize(camera_vec + v_vec * rotation_vec(1))
        v_vec = normalize(compute_cross_product(camera_vec, h_vec))

        camera_vec = normalize(camera_vec + h_vec * rotation_vec(2))
        h_vec = normalize(compute_cross_product(v_vec, camera_vec))

        h_vec = normalize(h_vec+ v_vec * rotation_vec(3))
        v_vec = normalize(compute_cross_product(camera_vec, h_vec))
      end associate
    end subroutine rotate_camera

    subroutine trace_rays(this, canvas)
      use, intrinsic :: iso_fortran_env, only:uint8
      class(scene), intent(in) :: this
      integer(uint8), contiguous, intent(inout) ::  canvas(:,:,:)

      !$omp parallel
      block
        integer :: i, j
        real :: r(3)
        !$omp do schedule(static) collapse(2)
        do j=1, this%height
          do i = 1, this%width
            r = this%get_ray(i,j)
            canvas(:,i,j) = this%get_ray_color(r)
          end do
        end do
        !$omp end do
      end block
    !$omp end parallel

    end subroutine trace_rays

    !generates each pixel
    pure function get_ray(this, i, j) result(ray)
      use, intrinsic :: iso_fortran_env, only:uint8
      class(scene), intent(in) :: this
      integer, intent(in) :: i, j
      real :: i_com, j_com, ray(3)

      i_com = (real(2*i) / real(this%width)) - 1.0
      j_com = (real(2*j) / real(this%height)) - 1.0

      ray = normalize(this%camera_vec + i_com * this%h_vec + j_com * this%v_vec)

    end function get_ray

    pure function get_ray_color(this,r) result(color)
      use, intrinsic :: iso_fortran_env, only:uint8
      class(scene), intent(in) :: this
      real, intent(in) :: r(3)
      integer(uint8) :: color(4)

      real :: smallest, curr, scratch
      real, parameter :: pi = 4.0 * atan(1.0), largest = huge(1.0)
      integer :: i

      associate(&
        camera_vec => this%camera_vec,&
        camera_pos => this%camera_pos,&
        sky_color => this%sky_color,&
        planes => this%planes,&
        spheres => this%spheres&
      )
        color = sky_color
        smallest = largest
        curr = smallest

        do i=1, size(planes)
          curr = planes(i)%get_plane_intersection(r, camera_pos)
          if(0.0 < curr .and. curr < smallest) then
            smallest = curr
            color = planes(i)%color
            !
            scratch = mod(&
              floor(&
              norm2(&
              curr * r + camera_pos - planes(i)%point)), 50&
            )

            if (scratch < 25) then
              ! color = [255,255,255,255]
              color = [0, 0, 0, 255] + [&
                  floor(sin(10.2 * real(scratch))),&
                  floor(sin(10.2 * (real(scratch)- pi/3))),&
                  floor(sin(10.2 * (real(scratch) - (2*pi)/3))),&
                  0 &
                ] * floor(scratch)
            end if
          end if
        end do

        do i=1, size(spheres)
          curr = spheres(i)%get_sphere_intersection(r, camera_pos)
          if(0.0 < curr .and. curr < smallest) then
            smallest = curr
            color = spheres(i)%color
          end if
        end do

    end associate
    end function get_ray_color

    pure function get_sphere_intersection(this, r, cam)result(t)
      class(sphere), intent(in) :: this
      real, intent(in) :: r(3), cam(3)
      real :: t, a, b, c
      associate(radius => this%radius, point => this%point)
        a = dot_product(r,r)
        b = 2 * (dot_product(r, cam) - dot_product(r,point))
        c = dot_product(cam, cam) + (dot_product(point, point)) - (2 * dot_product(cam, point)) - (radius ** 2)
        t = ((-1*b) + sqrt((b ** 2) - (4 * a * c))) / (2 * a)
      end associate
    end function get_sphere_intersection

    pure function get_plane_intersection(this, r, c) result(t)
      class(plane), intent(in) :: this
      real, intent(in) :: r(3), c(3)
      real :: t
      t = dot_product(this%point - c, this%normal) / dot_product(r, this%normal)
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
    p = v / sqrt(dot_product(v, v))
    end function normalize
end module raytracer
