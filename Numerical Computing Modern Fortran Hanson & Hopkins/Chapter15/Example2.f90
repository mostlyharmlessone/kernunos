    PROGRAM example2
      USE omp_lib, ONLY : omp_set_num_threads, omp_get_thread_num
      USE set_precision, ONLY : wp
      IMPLICIT NONE
      INTEGER :: i
      INTEGER, PARAMETER :: n = 101
      REAL (wp) :: a, x(n), y(n)

! Use four threads
      CALL omp_set_num_threads(4)
      x(1:n) = 1.0E0_wp
      y(1:n) = 1.0E0_wp
      a = 2.0E0_wp

!$OMP PARALLEL DO
      DO i = 1, n
        y(i) = a*x(i) + y(i)
        IF (i == n) THEN
          WRITE (*,'(''Thread '', i3, '' processes index '',i5)')&
             omp_get_thread_num(), n  
        END IF
      END DO
!$OMP END PARALLEL DO

    END PROGRAM
