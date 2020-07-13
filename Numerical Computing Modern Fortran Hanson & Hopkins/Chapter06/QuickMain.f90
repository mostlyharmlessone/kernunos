PROGRAM sorter
USE quicksort, ONLY: qsort
USE set_precision, ONLY: wp
INTEGER, PARAMETER :: nbase=100, ntimes=5, ninc=5
INTEGER :: left, right, i, errno, nvals
REAL :: t1, t2, timesr(ntimes), timess(ntimes)
REAL(wp), ALLOCATABLE :: a(:)

! Control whether sorting the sorted data is done
! This may take a very long time if the number of
! items gets large.
LOGICAL, PARAMETER :: dosorted=.TRUE.

nvals = nbase

! Output header identifying which version of quicksort
! is being run

WRITE(*,*)
WRITE(*,'(7x,''Sorts array of double precision reals into ascending order'')')
WRITE(*,'(7x,''using explicit double precision arrays.'')')
WRITE(*,'(7x,''Basic sorting operations implemented by function calls.'')')
WRITE(*,*)

IF (.NOT. dosorted) THEN
  WRITE(*,'(7X,''This run is NOT sorting already sorted data'')')
  WRITE(*,'(7X,''Switch on by setting the variable dosorted to TRUE'')')
  WRITE(*,'(7X,''Beware: this can cause long execution times'')')

  WRITE(*,*)
  WRITE(*,'(7X,''Ignore last three columns of the output table'')')
  WRITE(*,*)
END IF


WRITE(*,'(''      Size      Random     Ratio   Expected   Sorted     Ratio   Expected'')')

DO i = 1, ntimes

  ALLOCATE(a(nvals), STAT=errno)
  IF (errno /= 0) THEN
    WRITE(*, '(''Allocate failed trying to generate data vectors'')')
    WRITE(*, '(''i = '', i3, ''nvals = '', i12)')i, nvals
    STOP
  END IF

  left = 1
  right = nvals

! Start with a random vector of real
  CALL random_number(a)

! and sort them
  CALL cpu_time(t1)
  CALL qsort(a,left, right)
  CALL cpu_time(t2)
  CALL check
  timesr(i) = t2 - t1

  IF (dosorted) THEN
! now sort the sorted list
    CALL cpu_time(t1)
    CALL qsort(a,left, right)
    CALL cpu_time(t2)
    CALL check
    timess(i) = t2 - t1
  ELSE
    timess(i) = 0.0
  END IF

! Get ready to go around again
  nvals = nvals*ninc
  DEALLOCATE(a)
  IF (i == 1) THEN
    WRITE(*,'(i12, 2(f10.2, 20x))')nvals, timesr(1), timess(1)
  ELSE
    WRITE(*, '(i12, 6f10.2)') nvals, timesr(i), trat(i), trexp(), &
                             &timess(i), tsat(i), tsexp() 
  END IF 
  
END DO


CONTAINS
  SUBROUTINE check

  ! Check that the sorted list is indeed sorted
  INTEGER :: i
  REAL(wp) :: val

  val = a(left)
  DO i = left+1, right
    IF( val <= a(i)) THEN
      val = a(i)
    ELSE
      WRITE(*, '(''list not sorted: '',i8 ,'' > '',e12.4 &
	       &, '' for i = '',i8)')val, a(i), i
      STOP
    END IF
  END DO

  END SUBROUTINE check

  REAL FUNCTION trat(i)
  ! Compute time ratios for random data
  INTEGER :: i
  REAL, PARAMETER :: zero = 0.0e0
  IF(timesr(i-1) /= zero) THEN
    trat = timesr(i)/timesr(i-1)
  ELSE
    trat = zero
  END IF
  END FUNCTION trat

  REAL FUNCTION tsat(i)
  ! Compute time ratios for sorted data
  REAL, PARAMETER :: zero = 0.0e0
  INTEGER :: i
  IF(timess(i-1) /= zero) THEN
    tsat = timess(i)/timess(i-1)
  ELSE
    tsat = zero
  END IF
  END FUNCTION tsat

  REAL FUNCTION trexp()
  ! compute expected ration for random data
  INTEGER :: nold
  nold = nvals/ninc
  trexp = ninc*LOG(REAL(nvals))/LOG(REAL(nold))
  END FUNCTION trexp

  REAL FUNCTION tsexp()
  ! compute expected ration for sorted data
  tsexp = ninc*ninc
  END FUNCTION tsexp

  END PROGRAM
