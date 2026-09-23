module input
  implicit none(type, external)
  public
  logical, save :: key_event = .false.
  logical, save :: is_pressed(0:512) = .false., is_pressed_toggle(0:512)

  integer, parameter :: w = 87, a = 65, s = 83, d = 68, space =32, r = 82, lshift = 340

  integer, parameter :: up = 265, down = 264, left = 263, right = 262, q = 81, e = 69

  integer, parameter :: p = 80, esc = 256

  contains
    subroutine update_input(window, key, bits, pressed) bind(c)
      use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_bool
      type(c_ptr), value :: window
      integer(c_int), value :: key, bits
      logical(c_bool), value :: pressed
      key_event = .true.

      if(key >=0 .and. key <= 512) then
        is_pressed(key) = pressed
        ! print *, "key number ", key, " is pressed"
      end if

    end subroutine update_input
    !was_just_pressed checks if a key was just pressed, but after
    !reading from the event it returns false until its been toggled off
    !this is opposed to standard is_pressed which tracks current state
    function was_just_pressed(key) result(pressed)
    integer, value :: key
    logical :: pressed
      if(is_pressed_toggle(key).eqv.is_pressed(key)) then
        pressed =.false.
        return
      else if(is_pressed_toggle(key).and..not.is_pressed(key)) then
        is_pressed_toggle(key) = .false.
        pressed = .false.
        return
      end if
      is_pressed_toggle(key) = .true.
      pressed = .true.
    end function was_just_pressed

    function keyboard_get_dir() result(dir)
      real :: dir(3)
      dir = [0,0,0]

      if(is_pressed(w))  dir(1) = dir(1) + 1.0
      if(is_pressed(s))  dir(1) = dir(1) - 1.0
      if(is_pressed(d))  dir(2) = dir(2) + 1.0
      if(is_pressed(a))  dir(2) = dir(2) - 1.0
      if(is_pressed(space))  dir(3) = dir(3) - 1.0
      if(is_pressed(lshift)) dir(3) = dir(3) + 1.0
      block
        logical, save :: run_toggle = .false.
        if(was_just_pressed(r)) then
            run_toggle = .not.run_toggle
        end if

        if(run_toggle) dir = 2*dir
      end block
    end function keyboard_get_dir

    pure function keyboard_get_rotation() result(dir)
      real :: dir(3)
      dir = [0,0,0]
      if(is_pressed(up))  dir(1) = dir(1) - 0.05
      if(is_pressed(down))  dir(1) = dir(1) + 0.05
      if(is_pressed(left))  dir(2) = dir(2) - 0.05
      if(is_pressed(right))  dir(2) = dir(2) + 0.05
      if(is_pressed(q))  dir(3) = dir(3) - 0.05
      if(is_pressed(e))  dir(3) = dir(3) + 0.05
    end function keyboard_get_rotation
end module input
