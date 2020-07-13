  SUBROUTINE copy_string(cstring, fstring, clen, ftoc) BIND(C)
  USE, INTRINSIC :: iso_c_binding, ONLY: c_char, c_int, c_null_char
  CHARACTER (c_char), INTENT(INOUT) :: cstring(*), fstring(*)
  INTEGER (c_int), INTENT(INOUT) :: clen
  INTEGER (c_int), INTENT(IN) :: ftoc

! If ftoc is set to 1 then convert a Fortran string of length clen
! stored in fstring into a C string stored in cstring. This contains
! the same characters but is terminated by the null character.

! If ftoc is set to 0 then convert a C string stored in cstring
! into a Fortran string stored in fstring. This contains the same
! characters but is NOT terminated by the null character. clen is
! set to the clength of the copied string

  IF (ftoc == 1) THEN
    cstring(1:clen) = fstring(1:clen)
    cstring(clen+1) = c_null_char
  ELSE
    clen = 0
! The DO WHILE construct has been deprecated :-(
    DO 
      IF (cstring(clen+1) == c_null_char) EXIT ! for a WHILE!
      clen = clen+1
      fstring(clen) = cstring(clen)
    END DO
  END IF

  END SUBROUTINE copy_string

  PROGRAM example
  USE, INTRINSIC :: iso_c_binding
  CHARACTER(len=15) :: fstring
  CHARACTER(len=1)  :: cstring(15)
  INTEGER :: ftoc, clen

  INTERFACE
    SUBROUTINE copy_string(cstring, fstring, clen, ftoc) BIND(C)
    USE, INTRINSIC :: iso_c_binding
    CHARACTER (c_char), INTENT(INOUT) :: cstring(*), fstring(*)
    INTEGER (c_int), INTENT(INOUT) :: clen 
    INTEGER (c_int), INTENT(IN) :: ftoc
    END SUBROUTINE copy_string

    SUBROUTINE c_print(cstring) BIND(C)
    USE, INTRINSIC :: iso_c_binding
    CHARACTER (c_char), INTENT(IN) :: cstring(*)
    END SUBROUTINE c_print
  
    SUBROUTINE c_setString(cstring) BIND(C)
    USE, INTRINSIC :: iso_c_binding
    CHARACTER (c_char), INTENT(IN) :: cstring(*)
    END SUBROUTINE c_setString
  END INTERFACE

  fstring = 'Hello World!'
  ftoc = 1
  clen = 12
  CALL copy_string(cstring, fstring, clen, ftoc)
  CALL c_print(cstring)

  CALL c_setString(cstring)
  ftoc = 0
  CALL copy_string(cstring, fstring, clen, ftoc)
  print*, 'Fortranized string: ',fstring(1:clen)
  END
