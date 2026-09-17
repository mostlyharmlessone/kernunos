! based on https://fortran-lang.discourse.group/t/best-practices-for-passing-c-strings/104
 SUBROUTINE get_compiler_name(c_f) BIND(C, NAME='get_compiler_name_')
  USE, INTRINSIC :: iso_c_binding, ONLY : c_char, c_null_char
  USE, INTRINSIC :: iso_fortran_env
  CHARACTER(kind=c_char), dimension(*), INTENT(INOUT) :: c_f
  INTEGER :: inc,i
  CHARACTER(80) f
  f = compiler_version()
  inc= len(trim(f))
  ! prevent buffer overflow
  if (inc .gt. 255) then
   inc = 255
  endif
  do i = 1, inc
   c_f(i) = f(i:i)
  end do
  c_f(inc+1) = c_null_char
 END SUBROUTINE get_compiler_name

