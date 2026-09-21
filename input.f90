module input
  implicit none(type, external)

  logical, save :: input_state(0:512) = .false.
  contains
    subroutine update_input(window, key, bits, is_pressed) bind(c)
      use , intrinsic :: iso_c_binding, only: c_ptr, c_int, c_bool
      type(c_ptr), value :: window
      logical(c_bool), value :: is_pressed
      integer(c_int), value :: bits , key
      if(key >=0 .and. key <= 512) then
        input_state(key) = is_pressed
      end if
    end subroutine update_input
end module input
