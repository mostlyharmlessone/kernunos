    PROGRAM example5
      USE set_precision
      USE omp_lib
      IMPLICIT NONE
! Illustrate creating your own private variables.
      REAL (wp), ALLOCATABLE :: vt(:)
      INTEGER, SAVE :: natural
      INTEGER :: i, maxt
!$OMP THREADPRIVATE(NATURAL)
      maxt = omp_get_max_threads()

!$OMP PARALLEL DO 
      DO i = 1, maxt
! Only one thread allocates vt(:).
!$OMP CRITICAL
        IF ( .NOT. allocated(vt)) ALLOCATE (vt(maxt))
!$OMP END CRITICAL 
        natural = omp_get_thread_num() + 1
! This vt() value is private to each thread:      
        vt(natural) = natural
      END DO
!$OMP END PARALLEL DO      
      WRITE (*,'(A,/,(I5,F6.2))') &
        'Natural Numbers for Each Thread = ', (i,vt(i),i=1,maxt)
    END PROGRAM example5
