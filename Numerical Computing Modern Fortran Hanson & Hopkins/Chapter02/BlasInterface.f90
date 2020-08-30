      MODULE BlasInterface
      IMPLICIT NONE
! This interface module is an example using two Blas
! routines of how to ensure calls to library routines are
! correct.  Each user-callable routine in the library has
! its calling parameters defined:

        INTERFACE
!
! In the first case we just use the header statements
! as defined in the Blas routine; i.e., this is 
! Fortran 77 with Fortran 90 comments!
          SUBROUTINE DGEMV ( TRANS, M, N, ALPHA, A, LDA, X, INCX, &
                             BETA, Y, INCY )
!     .. Scalar Arguments ..
          DOUBLE PRECISION   ALPHA, BETA
          INTEGER            INCX, INCY, LDA, M, N
          CHARACTER*1        TRANS
!     .. Array Arguments ..
          DOUBLE PRECISION   A( LDA, * ), X( * ), Y( * )
          END SUBROUTINE DGEMV

!
! In the second example we rewrite the interface to use
! the new Fortran 90 declaration style. We also preserve
! the use of assumed size arrays where it would be more
! appropriate to use assumed shape array arguments.
          FUNCTION DNRM2(N,DX,INCX) RESULT(RES)
          DOUBLE PRECISION :: RES
!     .. Scalar Arguments ..
          INTEGER ::       INCX,N
!     ..
!     .. Array Arguments ..
          DOUBLE PRECISION :: DX(*)
          END FUNCTION DNRM2
        END INTERFACE
      END MODULE BlasInterface
