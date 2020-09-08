      PROGRAM test_alt_ret
      INTEGER igo
      REAL x,f(2), fun, fderiv, ftrue
      fun(x) = x**2 - 0.81
      fderiv(x)  = 2.0*x
      ftrue() = 0.9

      igo = 0
      x = 0.81
      GO TO 30

C Compute f(x)
   10 CONTINUE
      f(1) = fun(x)
      GO TO 30

C Compute the derivative of f(x)
   20 CONTINUE
      f(2) = fderiv(x)

C Perform the next step
   30 CONTINUE
      CALL newt(x,f,igo,*10,*20)

C newt executes a simple return on convergence
C which executes the next statement after the call
      WRITE (*,'(A/ 3E12.4)') 'Solution and absolute error: ',
     +  x,x - ftrue()

      END

      SUBROUTINE newt(x,f,igo,*,*)
      INTEGER igo
      REAL x,f(2),dx

      GO TO (10,20),igo
C igo == 0 does not branch; acts as an initialization
C Request f(x) in f(1)
      igo = 1
      RETURN 1

C igo == 1 request derivative of f(x) in f(2)
   10 CONTINUE
      igo = 2
      RETURN 2

C igo = 2 we have both function values
C perform a Newton iteration
   20 CONTINUE
      dx = f(1)/f(2)
C Update solution
      x = x - dx
C Continue if change to x is still large
      igo = 1
      IF (ABS(dx).GT.EPSILON(x)*ABS(x)) RETURN 1
C Signal that solution has been found, plain return.
      RETURN

      END
