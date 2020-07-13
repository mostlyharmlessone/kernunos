    PROGRAM test_alt_ret2
      INTEGER :: callerstep
      REAL :: x, f(2)
      INTEGER, PARAMETER :: evalf=1, evalfderiv=2,  &
               converged=3, initialize=0

      callerstep = initialize

LOOP: DO
        SELECT CASE (callerstep)
        CASE (initialize)
! Initialization step
! Set initial estimate of root
          x = 0.81E0
        CASE (evalf)
! Compute f(x)
          f(1) = fun(x)
        CASE (evalfderiv)
! Compute f'(x)     
          f(2) = fderiv(x)
        CASE (converged)
! Convergence
          WRITE (*,'(A/ 3E12.4)') 'Solution and absolute error: ', x,  &
            x - ftrue()
          EXIT LOOP
        END SELECT
! Call the newton iteration
        CALL newt2(x,f,callerstep)
      END DO LOOP

    CONTAINS

      REAL FUNCTION ftrue()
      ftrue = 0.9E0
      END FUNCTION ftrue

      REAL FUNCTION fun(x)
      REAL, INTENT(IN) :: x
      fun = x*x - 0.81E0
      END FUNCTION fun

      REAL FUNCTION fderiv(x)
      REAL, INTENT(IN) :: x
      fderiv  = x+x
      END FUNCTION fderiv

    END

    SUBROUTINE newt2(x,f,callerstep)
      INTEGER, INTENT(OUT) :: callerstep
      REAL, INTENT(INOUT) :: x
      REAL, INTENT(IN) :: f(2)
      INTEGER, PARAMETER :: evalf=1, evalfderiv=2,  &
               converged=3, initialize=0, getderiv=1, &
               updatex=2
      REAL :: dx
      INTEGER, SAVE :: nextstep=initialize

      SELECT CASE (nextstep)
      CASE (initialize)
! Initialization step
! In this case just get f(x) for the initial x
        callerstep = evalf
        nextstep = getderiv
      CASE (getderiv)
!  f(x) is saved in f(1) -- get f'(x)
        callerstep = evalfderiv
        nextstep = updatex
      CASE (updatex)
! Compute Newton step, f(x)/f'
        dx = f(1)/f(2)
! Update solution
        x = x - dx
! Continue if change to x is still large
        IF (ABS(dx)>EPSILON(x)*ABS(x)) THEN
! Start the cycle again by getting f(x)
          callerstep = evalf
          nextstep = getderiv
        ELSE
! Signal that solution has been found 
          callerstep = converged
          nextstep = initialize
        END IF
      END SELECT

    END
