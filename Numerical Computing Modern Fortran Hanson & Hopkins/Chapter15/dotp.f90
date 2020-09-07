    PROGRAM dotp
      USE set_precision, ONLY : wp
      USE omp_lib, ONLY : omp_set_num_threads, &
         omp_get_thread_num

! Illustrate use of the ATOMIC construct.
! The example is a simple dot product
      IMPLICIT NONE
      INTEGER, PARAMETER :: n = 10000, nth = 10
      REAL, PARAMETER :: zero = 0.0E0_wp
      REAL(wp), ALLOCATABLE :: x(:), y(:)
      REAL(wp) :: s, r
      INTEGER :: i, j

! Get a bunch of nth threads
      CALL omp_set_num_threads(nth)

      ALLOCATE (x(n),y(n))
      x(:) = 3.0E0_wp
      y(:) = 2.0E0_wp

      DO j = 1, 100
        s = zero
!$OMP PARALLEL DO
        DO i = 1, n
! Possibility of race condition between reading and
! writing s from/to memory
          s = s + x(i)*y(i)
        END DO
!$OMP END PARALLEL DO
        r = s - n*6
        WRITE (*, '(''Dot product - n*6 = '', E12.4,&
          &'' Thread number = '', i3)') &
              r, omp_get_thread_num()
        if(abs(r) > sqrt(epsilon(r))*n) then
            WRITE(*, '('' Race condition, stop!'')')
            STOP
        end if
      END DO
    END PROGRAM dotp
