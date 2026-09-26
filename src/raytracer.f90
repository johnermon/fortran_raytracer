module raytracer
  use , intrinsic :: iso_c_binding, only:c_int
  use, intrinsic :: iso_fortran_env, only:uint8
  use library, only:compute_cross_product, normalize
  use primatives, only:sphere, plane
  use shaders, only:apply_shader
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
      integer(uint8) :: color(4), curr_color(4)
      real, parameter :: largest = huge(1.0)
      integer :: i, curr_shader
      real :: smallest, curr, curr_point(3)

      associate(&
        camera_vec => this%camera_vec,&
        camera_pos => this%camera_pos,&
        sky_color => this%sky_color,&
        planes => this%planes,&
        spheres => this%spheres&
      )

        color = sky_color
        curr = smallest
        smallest = largest
        curr_shader = 0

        do i=1, size(planes)
          curr = planes(i)%get_plane_intersection(r, camera_pos)
          if(0.0 < curr .and. curr < smallest) then
            smallest = curr
            curr_color = planes(i)%color
            curr_shader = planes(i)%shader
            curr_point = planes(i)%point
          end if
        end do

        do i=1, size(spheres)
          curr = spheres(i)%get_sphere_intersection(r, camera_pos)
          if(0.0 < curr .and. curr < smallest) then
            smallest = curr
            curr_color = spheres(i)%color
            curr_shader = spheres(i)%shader
            curr_point = spheres(i)%point
          end if
        end do

        if (.not.curr_shader == 0) then
          color = apply_shader(&
            curr_shader, curr_color, curr_point, smallest * r + camera_pos&
          )
        end if

    end associate
    end function get_ray_color
end module raytracer
