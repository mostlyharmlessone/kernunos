module c_interfaces
 ! alphabetical order
 ! gathering all the interfaces for c/fortran interaction
 INTERFACE

! allows call of dgemm from c using c_dgemm.f90
SUBROUTINE c_dgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc) bind(c,name='C_dgemm')
 USE, INTRINSIC :: iso_c_binding, ONLY : c_char, c_int, c_double
 CHARACTER (c_char), INTENT (IN) :: transa, transb
 INTEGER (c_int), INTENT (IN) :: m, n, k, lda, ldb, ldc
 REAL (c_double), INTENT (IN) :: alpha, beta, a(lda,*), b(ldb,*)
 REAL (c_double), INTENT (INOUT) :: c(ldc,*)
END SUBROUTINE c_dgemm

!allows calling C code for conversion from fortran
SUBROUTINE ConvertPLYtoBIN(infile,outfile) BIND(C,name='convertplytobin_') !note the trailing underscore?
 USE, INTRINSIC :: iso_c_binding, ONLY : c_char
 IMPLICIT NONE
 CHARACTER(kind=c_char), INTENT(IN) :: infile
 CHARACTER(kind=c_char), INTENT(IN) :: outfile
END SUBROUTINE ConvertPLYtoBIN

SUBROUTINE OpenGL_Show(vertices, elements, nV, nE) BIND(C,name='opengl_show')
 USE, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int
 IMPLICIT NONE                
 real(c_float), INTENT(IN) :: vertices(*)
 integer(c_int), INTENT(IN) :: elements(*) 
 integer(c_int), INTENT(IN) :: nV 
 integer(c_int), INTENT(IN) :: nE
END SUBROUTINE OpenGL_Show
 
 END INTERFACE
    
 contains
    
end module
