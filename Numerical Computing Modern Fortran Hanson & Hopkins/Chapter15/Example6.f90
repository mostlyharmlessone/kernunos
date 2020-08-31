    PROGRAM example6
      USE set_precision
      USE omp_lib
      IMPLICIT NONE

      INTERFACE
        SUBROUTINE subz(n,allwork)
        IMPORT wp
          INTEGER, INTENT (IN) :: n
          REAL (wp), TARGET, INTENT (INOUT) :: allwork(:)
        END SUBROUTINE
      END INTERFACE

! Illustrate passing work space to multiple threads.
      REAL (wp), ALLOCATABLE, TARGET :: allwork(:)
! Here nmax is the largest value for n:
      INTEGER :: n, nmax = 10001
! Allocate one block of working storage.
      ALLOCATE (allwork(nmax*omp_get_max_threads()))
!$OMP PARALLEL
      DO n = 1, nmax
! The array allwork(:) is re-used as work space
! with each call to the routine subz().
        CALL subz(n,allwork)
      END DO
!$OMP END PARALLEL
      If(omp_get_thread_num()==0) THEN
          WRITE(*,'(A)') 'If no "Error" messages, work assignment succeeded.'
      end if
    END PROGRAM example6

    SUBROUTINE subz(n,allwork)
      USE omp_lib
      USE set_precision
      IMPLICIT NONE
      INTEGER, INTENT (IN) :: n
      REAL (wp), TARGET, INTENT (INOUT) :: allwork(:)
      INTEGER :: i
      REAL (wp), POINTER, SAVE :: work(:)

! Designate an n - segment of the working array
! allwork(:) passed to subz as the local
! array, work(:).
!$OMP THREADPRIVATE(work)
      work => allwork(omp_get_thread_num()*n+1:)

! Assign work(:) some values.
!$OMP CRITICAL
      DO i = 1, n
        work(i) = REAL(i, wp)
      END DO
! Make sure this assignment is thread-safe:
      DO i = 1, n
        IF (work(i) /= i) THEN
          WRITE(*,'(''Error: assignment appears not to be thread-safe!'')')
          EXIT
        END IF
      END DO
!$OMP END CRITICAL
    END SUBROUTINE
