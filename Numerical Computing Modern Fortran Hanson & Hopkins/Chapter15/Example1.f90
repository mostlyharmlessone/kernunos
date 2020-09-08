    PROGRAM example1
      USE omp_lib
      INTEGER x

      x = 2
!$OMP PARALLEL num_threads(2) SHARED(x)
      IF (omp_get_thread_num()==0) THEN
        x = 5
      ELSE
! The following read of x has a race condition.
! It may not show in the output!
        WRITE(*, '(''Race condition: Thread: '', i5, '', x ='', i5)') &
             omp_get_thread_num(), x
      END IF
!$OMP BARRIER
      IF (omp_get_thread_num()==0) THEN
        WRITE(*, '(''After barrier:  Thread:     0, x ='', i5)')  x
      ELSE
        WRITE(*, '(''After barrier:  Thread: '', i5, '', x ='', i5)') &
             omp_get_thread_num(), x
      END IF
!$OMP END PARALLEL

    END PROGRAM example1
