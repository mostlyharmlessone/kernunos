REAL FUNCTION c_sam(fptr, x) BIND(C)
! Pass a function defined in C to a Fortran
! function where it is used and the result
! passed back to the calling C code
!
! fptr is the C address of function which
!      is interoperable.
! x is the value to be used for evaluation
USE, INTRINSIC :: iso_c_binding

! Provide interface for a function that is
! interoperable with the C procedure pointed
! at by fptr.

INTERFACE
  REAL FUNCTION f(x)
  REAL x
  END FUNCTION
END INTERFACE

TYPE(c_funptr), VALUE :: fptr
REAL(c_float) :: x
PROCEDURE(f), POINTER :: fun

! Associate a Fortran procedure pointer with the target
! of a C function pointer
  CALL c_f_procpointer(fptr, fun)

! Finally we can call the function!
  c_sam = fun(x)
END FUNCTION
