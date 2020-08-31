    PROGRAM example3
      USE set_precision, ONLY : skind
      USE omp_lib, ONLY : omp_get_max_threads, omp_get_thread_num

      IMPLICIT NONE
      INTEGER :: i, istat, k, mythread, nthreads
      REAL (skind), ALLOCATABLE :: time(:)
      LOGICAL :: flag
INTERFACE
    SUBROUTINE subt(time,flag)
          USE set_precision, ONLY : skind
          IMPLICIT NONE
          REAL (skind), INTENT (IN) :: time
          LOGICAL, VOLATILE :: flag
    END SUBROUTINE      
END INTERFACE
      nthreads = omp_get_max_threads()
      ALLOCATE (time(nthreads))
      CALL random_number(time)
      flag = .FALSE.

!$OMP PARALLEL DO PRIVATE(mythread) SHARED(flag)
      DO i = 1, nthreads
        mythread = omp_get_thread_num()
        IF (mythread==0) THEN
          WRITE (*,'(A)',ADVANCE='No') 'Enter an integer -- '
          READ (*,*,IOSTAT=istat) k
! This has the VOLATILE attribute within the routine subt().
          flag = .TRUE.
        END IF
! All threads call subt()
        CALL subt(time(i),flag)
      END DO
!$OMP END PARALLEL DO
    END PROGRAM

    SUBROUTINE subt(time,flag)
      USE set_precision, ONLY : skind
      USE omp_lib, ONLY : omp_get_thread_num
      IMPLICIT NONE

      REAL (skind), INTENT (IN) :: time
      LOGICAL, VOLATILE :: flag
      INTEGER, SAVE :: istart, iend, irate, mythread

!$OMP THREADPRIVATE(istart, iend, irate, mythread)
      mythread = omp_get_thread_num()

      IF (mythread > 0) THEN
        CALL system_clock(COUNT = istart,COUNT_RATE = irate)
        DO 
! Spin Elapsed TIME S before testing FLAG.
          CALL system_clock(COUNT = iend)
          IF ((iend - istart) >= time*irate) EXIT
        END DO

        DO ! Spin until FLAG==.TRUE.
! Since FLAG is VOLATILE, this loop will not be removed
! or changed by the compiler in the executing code.
          IF (flag) EXIT
        END DO
        WRITE(*, '(''Thread '',I3, '' waited '',' // &
          'F6.3, '' secs before (Volatile) FLAG was tested'')') mythread, time
      ELSE
        WRITE (*, '(''Thread   0 did no waiting after input.'')')
      END IF

    END SUBROUTINE
