PROGRAM syscall
USE, INTRINSIC :: iso_c_binding, ONLY : c_char, c_int, c_null_char

INTERFACE
  FUNCTION ComProcAvail() &
     BIND(C,NAME='CommandProcessorAvailable')
  IMPORT :: c_int
  INTEGER(c_int) :: ComProcAvail
  END FUNCTION ComProcAvail

  FUNCTION RunSimpleCommand(command, arg) &
     BIND(C, NAME='RunSimpleCommand')
  IMPORT :: c_char, c_int
  CHARACTER(KIND=c_char) :: command(*), arg(*)
  INTEGER(c_int) :: RunSimpleCommand
  END FUNCTION RunSimpleCommand

END INTERFACE

CHARACTER (LEN=*, KIND=c_char), &
   PARAMETER :: dirname=c_char_'CreatedFromFortran'//c_null_char, &
                mkdir_com = c_char_'mkdir'//c_null_char, &
                rmdir_com = c_char_'rmdir'//c_null_char
INTEGER :: status

  status = ComProcAvail()
  IF (status == 0) THEN
    WRITE(*,'(''No command processor available'')')
    STOP
  ELSE
    WRITE(*,'(''Command processor available. Status: '',i3)')status
  END IF

! Create a directory (Unix or Windows based systems)
  status = RunSimpleCommand(mkdir_com, dirname)
  IF (status == 0) THEN
    WRITE(*,'(''First mkdir succeeded -- OK!'')')
  ELSE
    WRITE(*,'(''First mkdir failed. Status: '',i3)')status
  END IF

! Try and create it again.  Normally this should not succeed.
  status = RunSimpleCommand(mkdir_com, dirname)
  IF (status /= 0) THEN
    WRITE(*,'(''Second mkdir failed -- OK!'')')
  ELSE
    WRITE(*,'(''Second mkdir succeeded -- WRONG!'')')
  END IF

! Remove it
  status = RunSimpleCommand(rmdir_com, dirname)
  IF (status == 0) THEN
    WRITE(*,'(''rmdir succeeded -- OK!'')')
  ELSE
    WRITE(*,'(''rmdir failed -- WRONG!'')')
  END IF

END PROGRAM syscall
