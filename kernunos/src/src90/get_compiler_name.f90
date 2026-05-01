! based on https://fortran-lang.discourse.group/t/best-practices-for-passing-c-strings/104
 subroutine get_compiler_name(c_f) BIND(C, NAME='get_compiler_name_')
  use, intrinsic :: iso_c_binding, ONLY : c_char, c_null_char
  use, intrinsic :: iso_fortran_env
  character(kind=c_char), dimension(*), intent(inout) :: c_f
  integer :: inc,i
  character(80) f
  f = compiler_version()
  inc= len(trim(f))
  do i = 1, inc
   c_f(i) = f(i:i)
  end do
  c_f(inc+1) = c_null_char
 end subroutine get_compiler_name

