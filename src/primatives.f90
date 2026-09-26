module primatives
  use, intrinsic::iso_fortran_env, only:uint8
  use library, only:compute_cross_product, normalize
  implicit none(type, external)
  public
  type :: plane
    real:: point(3), normal(3), h_vec(3), v_vec(3)
    integer(uint8) :: color(4)
    integer :: shader
    contains
      procedure :: get_plane_intersection
  end type plane
  ! interface plane
  !   module procedure :: plane_new
  ! end interface plane
type :: sphere
    real :: radius, point(3), h_vec(3), v_vec(3)
    integer(uint8) :: color(4)
    integer :: shader
    contains
      procedure :: get_sphere_intersection
  end type sphere

  contains
    !automatically normalizes whatever normal you pass into the plane
    pure function plane_new(point, normal, color, shade) result(new_plane)
      use library, only:up_vec
      real, intent(in) :: point(3), normal(3)
      integer, intent(in) :: shade
      integer, intent(in) :: color(4)

      type(plane) :: new_plane
      new_plane%h_vec = normalize(compute_cross_product(normal, up_vec))
      new_plane%v_vec = normalize(compute_cross_product(normal, new_plane%h_vec))
      new_plane%point = point
      new_plane%normal = normalize(normal)
      new_plane%color = int([color(1), color(2), color(3), color(4)], kind = uint8)
      new_plane%shader = shade
    end function plane_new

    pure function sphere_new(radius, point, color, shade) result(new_sphere)
      use library, only:up_vec
      real, intent(in) :: point(3), radius
      integer, intent(in) :: shade
      integer, intent(in) :: color(4)
      type(sphere) :: new_sphere

      new_sphere%h_vec = normalize(compute_cross_product(point, up_vec))
      new_sphere%v_vec = normalize(compute_cross_product(point, new_sphere%h_vec))
      new_sphere%point = point
      new_sphere%radius = radius
      new_sphere%color = int([color(1), color(2), color(3), color(4)], kind = uint8)
      new_sphere%shader = shade
    end function sphere_new

    pure function get_sphere_intersection(this, r, cam)result(t)
      class(sphere), intent(in) :: this
      real, intent(in) :: r(3), cam(3)
      real :: t, b, c
      associate(radius => this%radius, point => this%point)
        b = 2 * (dot_product(r, cam) - dot_product(r,point))
        c = dot_product(cam, cam) + (dot_product(point, point))&
            - (2 * dot_product(cam, point)) - (radius ** 2)

        t = ((-1*b) - sqrt((b ** 2) - (4 * c))) / 2
      end associate
    end function get_sphere_intersection

    pure function get_plane_intersection(this, r, c) result(t)
      class(plane), intent(in) :: this
      real, intent(in) :: r(3), c(3)
      real :: t
      t = dot_product(this%point - c, this%normal) / dot_product(r, this%normal)
    end function get_plane_intersection

end module primatives



