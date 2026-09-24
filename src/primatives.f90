module primatives
  use, intrinsic::iso_fortran_env, only:uint8
  use library, only:compute_cross_product, normalize
  implicit none(type, external)
  public
  type :: plane
    real:: point(3), normal(3)
    integer(uint8) :: color(4)
    integer :: shader
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
    !automatically normalizes whatever normal you pass into the plane
    pure function plane_new(point, normal, color, shade) result(new_plane)
      real, intent(in) :: point(3), normal(3)
      integer, intent(in) :: shade
      integer(uint8), intent(in) :: color(4)
      type(plane) :: new_plane
      new_plane%point = point
      new_plane%normal = normalize(normal)
      new_plane%color = color
      new_plane%shader = shade
    end function plane_new

    pure function get_sphere_intersection(this, r, cam)result(t)
      class(sphere), intent(in) :: this
      real, intent(in) :: r(3), cam(3)
      real :: t, a, b, c
      associate(radius => this%radius, point => this%point)
        a = dot_product(r,r)
        b = 2 * (dot_product(r, cam) - dot_product(r,point))
        c = dot_product(cam, cam) + (dot_product(point, point))&
            - (2 * dot_product(cam, point)) - (radius ** 2)

        t = ((-1*b) - sqrt((b ** 2) - (4 * a * c))) / (2 * a)
      end associate
    end function get_sphere_intersection

    pure function get_plane_intersection(this, r, c) result(t)
      class(plane), intent(in) :: this
      real, intent(in) :: r(3), c(3)
      real :: t
      t = dot_product(this%point - c, this%normal) / dot_product(r, this%normal)
    end function get_plane_intersection

end module primatives



