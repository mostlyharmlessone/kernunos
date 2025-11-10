module c_interfaces
 ! gathering all the interfaces for c/fortran interaction
 INTERFACE

! call from c++ to fortran as extern "C" for data exchange
 SUBROUTINE Janus(flag,file_from_C,elements,vertices,legend,zern,nV,nE,nL,pupil_elements,pupil_vertices,pupil_nV,pupil_nE,err_janus) bind(C,name='janus_')
 USE, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char
 IMPLICIT NONE 
 CHARACTER(c_char), INTENT(IN), DIMENSION(4096) :: file_from_C
 integer(c_int), INTENT(INOUT) :: flag
 integer(c_int), INTENT(INOUT) :: nV 
 integer(c_int), INTENT(INOUT) :: nE               
 real(c_float), INTENT(INOUT) :: vertices(*)
 integer(c_int), INTENT(INOUT) :: elements(*) 
 integer(c_int), INTENT(INOUT) :: pupil_nV
 integer(c_int), INTENT(INOUT) :: pupil_nE
 integer(c_int), INTENT(INOUT) :: err_janus
 real(c_float), INTENT(INOUT) :: pupil_vertices(*)
 integer(c_int), INTENT(INOUT) :: pupil_elements(*)
 integer(c_int), INTENT(INOUT) :: nL
 real(c_float), INTENT(INOUT) :: legend(*)
 real(c_float), INTENT(INOUT) :: zern(*)
END SUBROUTINE Janus

! call from c++ to fortran as extern "C" 
 subroutine ConvertOFFtoSTL_C(INAME,ONAME,deftype) bind(C,name='ConvertOFFtoSTL_C_')
! Reads OFF file created by WriteOFF and generates ASCII and binary STL files 
! modified to be called from C/C++
  use, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char
  implicit none
  character(c_char), INTENT(INOUT), DIMENSION(4096) :: INAME,ONAME
  integer(c_int),INTENT(IN) :: deftype
end subroutine ConvertOFFtoSTL_C

! call from fortran to c
subroutine ConvertPLYtoBIN(iname, oname) BIND(C,name='ConvertPLYtoBIN')
! Reads ASCII PLY and makes binary PLY
USE, INTRINSIC :: iso_c_binding, ONLY : c_char,c_null_char
 CHARACTER(c_char), INTENT(IN), dimension(*) :: iname
 CHARACTER(c_char), INTENT(OUT), dimension(*) :: oname
end subroutine ConvertPLYtoBIN

subroutine LogC(message) BIND(C,name='LogC')
! logs a message to a file
USE, INTRINSIC :: iso_c_binding, ONLY : c_char,c_null_char
 CHARACTER(c_char), INTENT(IN), dimension(*) :: message
end subroutine LogC

function CharCount(iname) BIND(C,name='charcount')
! counts the periods "." in a file for determinng mire number
USE, INTRINSIC :: iso_c_binding, ONLY : c_char, c_int, c_null_char
 CHARACTER(c_char), INTENT(IN), dimension(*) :: iname
 integer(c_int) :: charcount
end function CharCount

subroutine Ccounter(inc, iname) BIND(C,name='Ccounter')
! used to show progression of calculation and print zernike result when done
USE, INTRINSIC :: iso_c_binding, ONLY : c_int,c_char,c_null_char
 integer(c_int), INTENT(IN) :: inc
 CHARACTER(c_char), INTENT(IN), dimension(*) :: iname
end subroutine Ccounter


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
end subroutine

END INTERFACE
    
 contains

end module c_interfaces


    

