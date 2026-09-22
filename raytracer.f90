module raytracer
  use , intrinsic :: iso_c_binding, only:c_int
  use, intrinsic :: iso_fortran_env, only:uint8
  implicit none(type, external)
  public
  type :: scene
    integer(c_int) :: width , height

    real :: up_vec(3), camera_pos(3), camera_vec(3)

    real :: h_vec(3), v_vec(3)

    integer(uint8) :: sky_color(4)

    type(plane) , allocatable :: planes(:)
    type(sphere) , allocatable :: spheres(:)

  end type scene

  type :: sphere
    real :: radius, point(3)
    integer(uint8) :: color(4)
  end type sphere

  type :: plane
    real:: point(3), normal(3)
    integer(uint8) :: color(4)
  end type plane
  contains
    pure function create_scene(width, height, camera_pos, camera_vec, sky_color, planes, spheres) result(new_scene)
      use, intrinsic :: iso_c_binding, only:c_int
      type(scene) :: new_scene
      integer(c_int), intent(in) :: width, height
      real, intent(in) :: camera_pos(3), camera_vec(3)
      real :: h_vec(3), v_vec(3)
      real, parameter :: up_vec(3) = [0.0, 0.0, 1.0]
      integer, intent(in) :: sky_color(4)
      type(plane), intent(in) :: planes(:)
      type(sphere), intent(in) :: spheres(:)

      h_vec = normalize(compute_cross_product(camera_vec, up_vec))
      v_vec = normalize(compute_cross_product(camera_vec, h_vec))

      new_scene = scene(&
        width,&
        height,&
        up_vec,&
        camera_pos, camera_vec,&
        h_vec,&
        v_vec,&
        sky_color,&
        planes,&
        spheres&
      )
    end function create_scene

    pure function create_plane(point, normal, color) result(new_plane)
      real, intent(in) :: point(3), normal(3)
      integer, intent(in) :: color(4)
      type(plane) :: new_plane
      new_plane = plane(&
        point, normalize(normal), color&
      )
    end function create_plane

    subroutine move_camera(curr_scene, dir_vec)
      type(scene), intent(inout) :: curr_scene
      real, intent(in) :: dir_vec(3)
      associate(&
        camera_vec => curr_scene%camera_vec,&
        h_vec => curr_scene%h_vec,&
        v_vec => curr_scene%v_vec,&
        camera_pos => curr_scene%camera_pos&
      )
        camera_pos = camera_pos + normalize(camera_vec) * dir_vec(1)
        camera_pos = camera_pos + h_vec * dir_vec(2)
        camera_pos = camera_pos + v_vec * dir_vec(3)
      end associate
    end subroutine move_camera

    subroutine rotate_camera(curr_scene, rotation_vec)
      type(scene), intent(inout) :: curr_scene
      real, intent(in) :: rotation_vec(3)
      associate(&
        camera_vec => curr_scene%camera_vec,&
        h_vec => curr_scene%h_vec,&
        v_vec => curr_scene%v_vec&
      )
        camera_vec = normalize(camera_vec + v_vec * rotation_vec(1))
        v_vec = normalize(compute_cross_product(camera_vec, h_vec))

        camera_vec = normalize(camera_vec + h_vec * rotation_vec(2))
        h_vec = normalize(compute_cross_product(v_vec, camera_vec))

        h_vec = normalize(h_vec+ v_vec * rotation_vec(3))
        v_vec = normalize(compute_cross_product(camera_vec, h_vec))
      end associate
    end subroutine rotate_camera

    subroutine trace_rays(curr_scene, canvas)
      use, intrinsic :: iso_fortran_env, only:uint8
      type(scene), intent(in) :: curr_scene
      integer(uint8), contiguous, intent(inout) ::  canvas(:,:,:)
      integer :: i, j

      !$omp parallel do collapse(2) private(i,j)
      do j=1, curr_scene%height
        do i = 1, curr_scene%width
          canvas(:,i,j) = get_ray_color(curr_scene, get_ray(curr_scene, i,j))
        end do
      end do
      !$omp end parallel do

    end subroutine trace_rays

    !generates each pixel
    pure function get_ray(curr_scene, i, j) result(ray)
      use, intrinsic :: iso_fortran_env, only:uint8
      type(scene), intent(in) :: curr_scene
      integer, intent(in) :: i, j
      real :: i_com, j_com, ray(3)

      i_com = (real(2*i) / real(curr_scene%width)) - 1.0
      j_com = (real(2*j) / real(curr_scene%height)) - 1.0

      ray = normalize(curr_scene%camera_vec + i_com * curr_scene%h_vec + j_com * curr_scene%v_vec)

    end function get_ray

    pure function get_ray_color(curr_scene,r) result(color)
      use, intrinsic :: iso_fortran_env, only:uint8
      real, parameter :: pi = 4.0 * atan(1.0)
      integer(uint8) :: color(4)
      type(scene), intent(in) :: curr_scene
      real, intent(in) :: r(3)
      real, parameter :: largest = huge(1.0)
      real :: smallest, curr, scratch
      integer :: i

      associate(&
        camera_vec => curr_scene%camera_vec,&
        camera_pos => curr_scene%camera_pos,&
        sky_color => curr_scene%sky_color,&
        planes => curr_scene%planes,&
        spheres => curr_scene%spheres&
      )
        color = sky_color
        smallest = largest
        curr = smallest

        do i=1, size(planes)
          curr = get_plane_intersection(planes(i), r, camera_pos)
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
              color = [0, 0, 0, 255] + [&
                  floor(sin(10.2 * real(scratch))),&
                  floor(sin(10.2 * (real(scratch)- pi/3))),&
                  floor(sin(10.2 * (real(scratch) - (2*pi)/3))),&
                  0 &
                ] * floor(scratch)
            end if
          end if
        end do

        ! do i=1, sizeof(spheres)
        !   curr_sphere = spheres(i)
        !   curr = get_sphere_intersection(curr_sphere, r, camera_pos)
        !   if(curr < smallest) then
        !     smallest = curr
        !   end if
        ! end do
        !
    end associate
    end function get_ray_color

    pure function get_sphere_intersection(curr_sphere, r, c)result(t)
      type(sphere), intent(in) :: curr_sphere
      real, intent(in) :: r(3), c(3)
      real :: t

    end function get_sphere_intersection

    pure function get_plane_intersection(curr_plane, r, c) result(t)
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
      p = v / norm2(v)
    end function normalize
end module raytracer
