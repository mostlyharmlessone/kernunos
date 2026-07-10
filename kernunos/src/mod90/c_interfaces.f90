MODULE c_interfaces
 ! gathering all the interfaces for c/fortran interaction
 INTERFACE

! call from c++ to fortran as extern "C" for data exchange
 SUBROUTINE Janus(flag,file_from_C,elements,vertices,legend,cardinal,zern,nV,nE,nL,nC,pupil_elements,pupil_vertices,pupil_nV,pupil_nE,err_janus) bind(C,name='janus_')
 USE, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char,c_int64_t,c_double
 IMPLICIT NONE 
 CHARACTER(c_char), INTENT(IN), DIMENSION(4096) :: file_from_C
 INTEGER(c_int64_t), INTENT(INOUT) :: flag
 INTEGER(c_int), INTENT(INOUT) :: nV
 INTEGER(c_int), INTENT(INOUT) :: nE
 REAL(c_float), INTENT(INOUT) :: vertices(*)
 INTEGER(c_int), INTENT(INOUT) :: elements(*)
 INTEGER(c_int), INTENT(INOUT) :: pupil_nV
 INTEGER(c_int), INTENT(INOUT) :: pupil_nE
 INTEGER(c_int), INTENT(INOUT) :: err_janus
 REAL(c_float), INTENT(INOUT) :: pupil_vertices(*)
 INTEGER(c_int), INTENT(INOUT) :: pupil_elements(*)
 INTEGER(c_int), INTENT(INOUT) :: nL,nC
 REAL(c_float), INTENT(INOUT) :: legend(*)
 REAL(c_double), INTENT(INOUT) :: cardinal(*)
 REAL(c_float), INTENT(INOUT) :: zern(*)
END SUBROUTINE Janus

! call from c++ to fortran as extern "C" 
 SUBROUTINE ConvertOFFtoSTL_C(INAME,ONAME,deftype) bind(C,name='ConvertOFFtoSTL_C_')
! Reads OFF file created by WriteOFF and generates ASCII and binary STL files 
! modified to be called from C/C++
  use, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char
  IMPLICIT NONE
  CHARACTER(c_char), INTENT(INOUT), DIMENSION(4096) :: INAME,ONAME
  INTEGER(c_int),INTENT(IN) :: deftype
END SUBROUTINE ConvertOFFtoSTL_C

! call from c++ to fortran as extern "C"
SUBROUTINE get_compiler_name(c_f) BIND(C, NAME='get_compiler_name_')
 USE, INTRINSIC :: iso_c_binding, ONLY : c_char, c_null_char
 USE, INTRINSIC :: iso_fortran_env
 CHARACTER(kind=c_char), dimension(*), INTENT(INOUT) :: c_f
END SUBROUTINE get_compiler_name

! call from fortran to c

! input has to be c_null_char terminated without extra whitespace, and length consistent with that.
INTEGER(c_int) FUNCTION CleanSemicolons_C(iname, oname) BIND(C,name='CleanSemicolons_C')
! Replaces semicolons with commas in a file, making a new file, returns an integer 0 on success
USE, INTRINSIC :: iso_c_binding, ONLY : c_int,c_char,c_null_char
 CHARACTER(c_char), INTENT(IN), dimension(*) :: iname
 CHARACTER(c_char), INTENT(IN), dimension(*) :: oname
END FUNCTION CleanSemicolons_C

! not actually ever called from Fortran, but same remarks apply
INTEGER(c_int) FUNCTION ConvertPLYtoBIN(iname, oname) BIND(C,name='ConvertPLYtoBIN')
! Reads ASCII PLY and makes binary PLY, returns an integer, 0 on success
USE, INTRINSIC :: iso_c_binding, ONLY : c_int,c_char,c_null_char
 CHARACTER(c_char), INTENT(IN), dimension(*) :: iname
 CHARACTER(c_char), INTENT(IN), dimension(*) :: oname
END FUNCTION ConvertPLYtoBIN

! this is a subroutine because it is a void function in C
SUBROUTINE LogC(message) BIND(C,name='LogC')
! logs a message to a file
USE, INTRINSIC :: iso_c_binding, ONLY : c_char,c_null_char
 CHARACTER(c_char), INTENT(IN), dimension(*) :: message
END SUBROUTINE LogC

! the type (c_int) of the FUNCTION is specified under intrinsic in this one, rather than prototype style as above
FUNCTION CharCount(iname) BIND(C,name='charcount')
! counts the periods "." in a file for determining mire number
USE, INTRINSIC :: iso_c_binding, ONLY : c_char, c_int, c_null_char
 CHARACTER(c_char), INTENT(IN), dimension(*) :: iname
 INTEGER(c_int) :: charcount
END FUNCTION CharCount

SUBROUTINE Ccounter(inc, iname) BIND(C,name='Ccounter')
! used to show progression of calculation and print zernike result when done
USE, INTRINSIC :: iso_c_binding, ONLY : c_int,c_char,c_null_char
 INTEGER(c_int), INTENT(IN) :: inc
 CHARACTER(c_char), INTENT(IN), dimension(*) :: iname
END SUBROUTINE Ccounter


! call from c to fortran

SUBROUTINE fortran_print(info,n,nnzl,nnzu,memuse) BIND(C, &
    NAME='fortran_print')
! This routine is called from the C wrapper.  It summarizes
! the results and memory usage of factoring the sparse
! Harwell-Boeing square matrix.
  USE, INTRINSIC :: iso_c_binding
  USE sparsetypes, ONLY : mem_usage
  IMPLICIT NONE
  TYPE (mem_usage), INTENT (IN) :: memuse
  INTEGER (c_int), INTENT(IN) :: info, n, nnzl, nnzu
END SUBROUTINE

END INTERFACE
    
 contains

END MODULE c_interfaces


    

