    PROGRAM example4
      USE set_precision
      USE omp_lib
      IMPLICIT NONE
! Illustrate use of the ATOMIC construct.
! The example is a sum of weighted inner products.

      INTEGER, PARAMETER :: n = 100
      REAL (wp), ALLOCATABLE :: w(:), x(:), y(:)
      REAL (wp) :: s
      INTEGER :: i

      ALLOCATE (w(n),x(n),y(n))
      CALL random_number(w)

      s = 0.0E0_wp
!$OMP PARALLEL DO
      DO i = 1, n
!$OMP CRITICAL
        CALL random_number(x)
        CALL random_number(y)
!$OMP END CRITICAL
!$OMP ATOMIC ! Note array product use
! and intrinsic routine.
        s = s + dot_product(x*w,y)
      END DO
!$OMP END PARALLEL DO
      WRITE (*, '(''Weighted dot product = '', e14.7)') s
    END PROGRAM
