PROGRAM sorter
USE sortElements, ONLY: qsort
USE concrete, ONLY : dpdata
USE set_precision, ONLY : wp
INTEGER, PARAMETER :: nbase=100, ntimes=7, ninc=5
LOGICAL, PARAMETER :: dosorted=.FALSE.
REAL :: t1, t2, timesr(ntimes), timess(ntimes)
INTEGER :: left, right, i, errno, nvals

TYPE (dpdata) :: sortdata

nvals = nbase

WRITE(*,*)
WRITE(*,'(7x,''Sorts array of double precision reals into ascending order'')')
WRITE(*,'(7x,''using an abstract data type and interfaces.'')')
WRITE(*,'(7x,''Basic sorting operations implemented by function calls'')')
WRITE(*,'(7x,''to type bound procedures.'')')
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

  ALLOCATE(sortdata%items(nvals), STAT=errno)
  IF (errno /= 0) THEN
    WRITE(*, '(''Allocate failed trying to generate data vectors'')')
    WRITE(*, '(''i = '', i3, ''nvals = '', i12)')i, nvals
    STOP
  END IF

  left = 1
  right = nvals

  sortdata%left = left
  sortdata%right = right

! Start with a random vector of real
  CALL random_number(sortdata%items)

! and sort them
  CALL cpu_time(t1)
  CALL qsort(sortdata)
  CALL cpu_time(t2)
  CALL check
  timesr(i) = t2 - t1

  IF (dosorted) THEN
! now sort the sorted list
    CALL cpu_time(t1)
    CALL qsort(sortdata)
    CALL cpu_time(t2)
    CALL check
    timess(i) = t2 - t1
  ELSE
    timess(i) = 0.0
  END IF

! Get ready to go around again
  nvals = nvals*ninc
  DEALLOCATE(sortdata%items)
  IF (i == 1) THEN
    WRITE(*,'(i12, 2(f10.2, 20x))')nvals, timesr(1), timess(1)
  ELSE
    WRITE(*, '(i12, 6f10.2)') nvals, timesr(i), trat(i), trexp(), timess(i), tsat(i), tsexp() 
  END IF 
  
END DO


CONTAINS
  SUBROUTINE check
  INTEGER :: i
  REAL(wp) :: val

  val = sortdata%items(left)
  DO i = sortdata%left+1, sortdata%right
    IF( val <= sortdata%items(i)) THEN
      val = sortdata%items(i)
    ELSE
      WRITE(*, '(''list not sorted: '',e12.4 ,'' > '',e12.4 &
	       &, '' for i = '',i8)')val, sortdata%items(i), i
      STOP
    END IF
  END DO

  END SUBROUTINE check

  REAL FUNCTION trat(i)
  INTEGER :: i
  REAL, PARAMETER :: zero = 0.0e0
  IF(timesr(i-1) /= zero) THEN
    trat = timesr(i)/timesr(i-1)
  ELSE
    trat = zero
  END IF
  END FUNCTION trat

  REAL FUNCTION tsat(i)
  REAL, PARAMETER :: zero = 0.0e0
  INTEGER :: i
  IF(timess(i-1) /= zero) THEN
    tsat = timess(i)/timess(i-1)
  ELSE
    tsat = zero
  END IF
  END FUNCTION tsat

  REAL FUNCTION trexp()
  INTEGER :: nold
  nold = nvals/ninc
  trexp = ninc*LOG(REAL(nvals))/LOG(REAL(nold))
  END FUNCTION trexp

  REAL FUNCTION tsexp()
  tsexp = ninc*ninc
  END FUNCTION tsexp


END PROGRAM
