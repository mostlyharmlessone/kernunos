      MODULE LapackInterface
      IMPLICIT NONE
! This interface module is an example using two Lapack
! routines of how to ensure calls to library routines are
! correct.  Each user-callable routine in the library has
! its calling parameters defined:

!
! We just use the header statements as defined in the
! original Lapack routines; i.e., this is Fortran 77 with
! Fortran 90 comments!

        INTERFACE
          SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
!
!     .. Scalar Arguments ..
          INTEGER            INFO, LDA, M, N
!     ..
!     .. Array Arguments ..
          INTEGER            IPIV( * )
          DOUBLE PRECISION   A( LDA, * )
          END SUBROUTINE DGETRF

          SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
!
!     .. Scalar Arguments ..
          CHARACTER          TRANS
          INTEGER            INFO, LDA, LDB, N, NRHS
!     ..
!     .. Array Arguments ..
          INTEGER            IPIV( * )
          DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
          END SUBROUTINE DGETRS
        END INTERFACE
      END MODULE LapackInterface
