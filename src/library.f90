module library
  implicit none(type, external)
  real, parameter :: up_vec(3) = [0.0, 0.0, 1.0]
  public
  contains
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
