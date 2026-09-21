module raytracer
  use , intrinsic :: iso_c_binding, only:c_int
  use, intrinsic :: iso_fortran_env, only:uint8
    implicit none(type, external)
  type :: scene
    integer(c_int) :: width , height

    real :: up_vec(3), camera_pos(3), camera_vec(3)

    real :: h_vec(3), v_vec(3)

    type(plane) , allocatable :: planes(:)
    type(sphere) , allocatable :: spheres(:)

    integer(uint8) :: sky_color(4)
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
    function setup_params() result(curr_scene)
      use, intrinsic :: iso_fortran_env, only:uint8
      type(scene) :: curr_scene
      real :: camera_vec(3), camera_pos(3), up_vec(3), h_vec(3), v_vec(3)

      up_vec = [0.0,0.0,1.0]
      camera_pos = [500.0,50.0, -50.0]
      camera_vec = [-1.0,1.0,0.0]

      h_vec = normalize(compute_cross_product(camera_vec, up_vec))
      v_vec = normalize(compute_cross_product(camera_vec, h_vec))

      curr_scene = scene(&
        1024, 1024,& !width, height
        up_vec,&
        camera_pos, camera_vec,&
        h_vec, v_vec,&
          [&!planes
            plane(&
              [0.0,0.0,-20.0],& !point
              [0.2,0.2,1.0],& !normal

              [250, 206, 91, 255]& !color
            ),&

            plane(&
              [0.0, 200.0,-20.0],& !point
              [-0.2,-0.2,1.0],& !normal

              [184, 169, 249, 255]& !color
            )&
          ],&

          [& ! spheres
            sphere(&
              10,& ! radius
              [20.0, 0.0, 0.0],&

              [0,0,255, 255]& !color
            )&
          ],&

          !sky color
          [229, 221, 107, 255]&
        )
    end function setup_params

    subroutine move_camera(curr_scene, dir_vec)
      type(scene), intent(inout) :: curr_scene
      real, intent(in) :: dir_vec(3)
      curr_scene%camera_pos = curr_scene%camera_pos + normalize(curr_scene%camera_vec) * dir_vec(1)
      curr_scene%camera_pos = curr_scene%camera_pos + curr_scene%h_vec * dir_vec(2)
      curr_scene%camera_pos = curr_scene%camera_pos + curr_scene%v_vec * dir_vec(3)
    end subroutine move_camera

    subroutine rotate_camera(curr_scene, rotation_vec)
      type(scene), intent(inout) :: curr_scene
      real, intent(in) :: rotation_vec(3)

        curr_scene%camera_vec = normalize(curr_scene%camera_vec + curr_scene%v_vec * rotation_vec(1))
        curr_scene%v_vec = normalize(compute_cross_product(curr_scene%camera_vec, curr_scene%h_vec))

        curr_scene%camera_vec = normalize(curr_scene%camera_vec + curr_scene%h_vec * rotation_vec(2))
        curr_scene%h_vec = normalize(compute_cross_product(curr_scene%v_vec, curr_scene%camera_vec))

        curr_scene%h_vec = normalize(curr_scene%h_vec+ curr_scene%v_vec * rotation_vec(3))
        curr_scene%v_vec = normalize(compute_cross_product(curr_scene%camera_vec, curr_scene%h_vec))
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
      integer(uint8) :: color(4)
      type(scene), intent(in) :: curr_scene
      real, intent(in) :: r(3)
      real, parameter :: largest = huge(1.0)
      real :: smallest, curr, scratch
      integer :: i

      color = curr_scene%sky_color
      smallest = largest
      curr = smallest

      do i=1, size(curr_scene%planes)
        curr = get_plane_intersection(curr_scene%planes(i), r, curr_scene%camera_pos)
        if(0.0 < curr .and. curr < smallest) then
          smallest = curr
          color = curr_scene%planes(i)%color
          !
          scratch = mod(&
            floor(&
            norm2(&
            curr * r + curr_scene%camera_pos - curr_scene%planes(i)%point)), 50&
          )

          if (scratch < 25) then
            color = [&
              int(abs(sin(scratch/5)) * 255.0, uint8),&
              int(abs(cos(scratch/3)) * 255.0, uint8),&
              int(abs(sin(scratch/2)) * 255.0,uint8),&
              int(255, uint8)&
              ]
          end if
        end if
      end do

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
