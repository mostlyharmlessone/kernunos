       PROGRAM testClassFinalize 
! This program unit is written by the user of ZEROIN.
       USE brentClass, ONLY : ZEROIN
       USE brentExtraArgsFinalize, ONLY: brentArgsExtra, extraF
       USE set_precision, ONLY : wp
       TYPE(brentArgsExtra), allocatable :: extraBrent
       REAL(wp) :: z

       allocate(extraBrent)
! Open file for evaluation points and define the end points of
! the interval and use the default tolerance.
       open(unit=extraBrent % unitNumber,&
      & file='EvaluationList',STATUS='REPLACE')
       extraBrent%ax    = 2.0E0_wp
       extraBrent%bx    = 3.0E0_wp
       extraBrent%param = 5.0E0_wp

       z = zeroin(extraBrent, extraF)
       WRITE(*, &
      & '('' Root of f(x) = x*(x*x-2.0d0)-param = 0, is:'', f15.10)') z
       WRITE(*, &
      & '('' The number of function evaluations required: '', I0)') &
        extraBrent % evaluations
! Trigger the FINALIZATION routine. This writes a final line
! and closes the file EvaluationList.
       deallocate(extraBrent)
       END PROGRAM testClassFinalize


