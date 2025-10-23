 subroutine get_compiler_name(f)
  use, intrinsic :: iso_fortran_env
  character(*), intent(INOUT) :: f
  f = trim(compiler_version())
 end subroutine
