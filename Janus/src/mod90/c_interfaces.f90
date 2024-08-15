module c_interfaces
 ! alphabetical order
 ! gathering all the interfaces for c/fortran interaction
 INTERFACE

! call from c++ to fortran as extern "C" for data exchange
SUBROUTINE Janus(flag,file_from_C,elements,vertices,legend,zern,nV,nE,nL,nZ) bind(C,name='janus_')
 USE, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char
 IMPLICIT NONE 
 CHARACTER(c_char), INTENT(IN), DIMENSION(4096) :: file_from_C
 integer(c_int), INTENT(INOUT) :: flag
 integer(c_int), INTENT(INOUT) :: nV 
 integer(c_int), INTENT(INOUT) :: nE               
 real(c_float), INTENT(INOUT) :: vertices(*)
 integer(c_int), INTENT(INOUT) :: elements(*) 
 integer(c_int), INTENT(INOUT) :: nL
 real(c_float), INTENT(INOUT) :: legend(*)
 integer(c_int), INTENT(INOUT) :: nZ
 real(c_float), INTENT(INOUT) :: zern(*)
END SUBROUTINE Janus

! call from c++ to fortran as extern "C" 
 subroutine ConvertOFFtoSTL_C(INAME,ONAME) bind(C,name='ConvertOFFtoSTL_C_')
! Reads OFF file created by WriteOFF and generates ASCII and binary STL files 
! modified to be called from C/C++
  use io_functions, only : get_new_fileunit
  use special_fct, only : surface_normal,rgb2attr
  use ISO_FORTRAN_ENV, only: INT8,INT16,INT32,REAL32 
  use, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char
  implicit none
  character(c_char), INTENT(INOUT), DIMENSION(4096) :: INAME,ONAME
end subroutine ConvertOFFtoSTL_C

! call from fortran to c
subroutine ConvertPLYtoBIN(iname, oname) BIND(C,name='ConvertPLYtoBIN')
USE, INTRINSIC :: iso_c_binding, ONLY : c_char,c_null_char
 CHARACTER(c_char), INTENT(IN), dimension(*) :: iname
 CHARACTER(c_char), INTENT(OUT), dimension(*) :: oname
end subroutine ConvertPLYtoBIN

subroutine LogC(message) BIND(C,name='LogC')
USE, INTRINSIC :: iso_c_binding, ONLY : c_char,c_null_char
 CHARACTER(c_char), INTENT(IN), dimension(*) :: message
end subroutine LogC

subroutine Ccounter(inc, iname) BIND(C,name='Ccounter')
USE, INTRINSIC :: iso_c_binding, ONLY : c_int,c_char,c_null_char
 integer(c_int), INTENT(IN) :: inc
 CHARACTER(c_char), INTENT(IN), dimension(*) :: iname
end subroutine Ccounter

END INTERFACE
    
 contains

end module c_interfaces


    

