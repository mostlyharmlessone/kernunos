! launching a subprocess a la PROC in perl        
program p
   integer :: i

   call execute_command_line("x a b", exitstat=i)
   print *, "Exit status of x was ", i

   call execute_command_line("x a b", wait=.false.)
   print *, "running x in the background"
end program p 

! or

program test_exec
  integer :: i

  call execute_command_line ("external_prog.exe", exitstat=i)
  print *, "Exit status of external_prog.exe was ", i

  call execute_command_line ("reindex_files.exe", wait=.false.)
  print *, "Now reindexing files in the background"

end program test_exec

! work around for pgfortran
program test
    use iso_fortran_env !intrinsic module for iostat, real kinds, atomic operations, compiler options
    implicit none
    integer(kind=int32)   :: st
    character(len=20)  :: msg
    call system("echo 'hi there!1'")
    call system("echo 'hi there!2'")
end program test


! Note: NOT in a module
subroutine execute_command_line( cmd )
character(len=*) :: cmd

call system( cmd )
end subroutine execute_command_line


