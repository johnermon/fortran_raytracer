module input
  implicit none(type, external)
  public
  logical, save :: is_pressed(0:512) = .false.

  integer, parameter :: w = 87, a = 65, s = 83, d = 68, space =32, shift = 340

  integer, parameter :: up = 265, down = 264, left = 263, right = 262, q = 81, e = 69

  integer, parameter :: p = 80, esc = 256

  contains
    subroutine update_input(window, key, bits, pressed) bind(c)
      use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_bool
      type(c_ptr), value :: window
      logical(c_bool), value :: pressed
      integer(c_int), value :: bits , key

      if(key >=0 .and. key <= 512) then
        is_pressed(key) = pressed
        ! print *, "key number ", key, " is pressed"
      end if

    end subroutine update_input

    pure function keyboard_get_dir() result(dir)
      real :: dir(3)
      dir = [0,0,0]

      if(is_pressed(w)) then
        dir(1) = dir(1) + 1.0
      end if
      if(is_pressed(s)) then
        dir(1) = dir(1) - 1.0
      end if

      if(is_pressed(d)) then
        dir(2) = dir(2) + 1.0
      end if
      if(is_pressed(a)) then
        dir(2) = dir(2) - 1.0
      end if

      if(is_pressed(space)) then
        dir(3) = dir(3) - 1.0
      end if
      if(is_pressed(shift)) then
        dir(3) = dir(3) + 1.0
      end if

    end function keyboard_get_dir

    pure function keyboard_get_rotation() result(dir)
      real :: dir(3)
      dir = [0,0,0]

      if(is_pressed(up)) then
        dir(1) = dir(1) - 0.05
      end if
      if(is_pressed(down)) then
        dir(1) = dir(1) + 0.05
      end if

      if(is_pressed(left)) then
        dir(2) = dir(2) - 0.05
      end if
      if(is_pressed(right)) then
        dir(2) = dir(2) + 0.05
      end if

      if(is_pressed(q)) then
        dir(3) = dir(3) - 0.05
      end if
      if(is_pressed(e)) then
        dir(3) = dir(3) + 0.05
      end if

    end function keyboard_get_rotation
end module input
