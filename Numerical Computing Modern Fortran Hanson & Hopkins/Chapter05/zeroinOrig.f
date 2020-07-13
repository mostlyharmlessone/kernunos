      DOUBLE PRECISION FUNCTION zeroin(ax,bx,funct,tol)

C     .. Scalar Arguments ..
      DOUBLE PRECISION ax,bx,tol
C     ..
C     .. Function Arguments ..
      DOUBLE PRECISION funct
      EXTERNAL funct
C
C  A ZERO of the function  f(x)  is computed in the interval [ax,bx].
C
C  Input:
C
C  ax     left endpoint of initial interval
C  bx     right endpoint of initial interval
C  funct  function subprogram which evaluates f(x) for any x in
C         the interval  [ax,bx]
C  tol    desired length of the interval of uncertainty of the
C         final result (>= 0.)
C  Note: ax,bx,tol are components of TYPE(brentArgs)

C  Output:
C
C  ZEROIN - An abscissa approximating a ZERO of  f  in the interval [ax,bx]
C
C      It is assumed  that   f(ax)   and   f(bx)   have  opposite  signs
C  without a check. ZEROIN  returns a ZERO  x  in the given interval
C  [ax,bx]  to within a tolerance  4*macheps*ABS(x)+tol, where macheps  is
C  the  relative machine precision.
C      This function subprogram is a slightly  modified  translation  of
C  the Algol 60 procedure  ZERO  given in  Richard Brent, Algorithms for
C  Minimization Without Derivatives, Prentice-Hall, Inc. (1973).
C  The Fortran version was originally from the book by
C  G. Forsythe, M. Malcolm and C. Moler, Computer Methods for 
C  Mathematical Computations. Prentice-Hall, Inc. (1977).
C
C     ..
C     .. Local Scalars ..
      DOUBLE PRECISION a,b,c,d,e,eps,fa,fb,fc,p,q,r,s,tol1,xm
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC ABS,SIGN
C     ..
      eps = epsilon(0.0d0)
C
C  initialization
C
      a = ax
      b = bx
      fa = funct(a)
      fb = funct(b)
C
C begin step
C
   20 c = a
      fc = fa
      d = b - a
      e = d
   30 IF (ABS(fc).GE.ABS(fb)) GO TO 40
      a = b
      b = c
      c = a
      fa = fb
      fb = fc
      fc = fa
C
C convergence test
C
   40 tol1 = 2.0d0*eps*ABS(b) + 0.5d0*tol
      xm = 0.5d0* (c-b)
      IF ((ABS(xm).LE.tol1) .OR. (fb.EQ.0.0d0)) GO TO 90
C
C is bisection necessary
C
      IF ((ABS(e).LT.tol1) .OR. (ABS(fa).LE.ABS(fb))) GO TO 70
C
C is quadratic interpolation possible
C
      IF (a.NE.c) GO TO 50
C
C linear interpolation
C
      s = fb/fa
      p = 2.0d0*xm*s
      q = 1.0d0 - s
      GO TO 60
C
C inverse quadratic interpolation
C
   50 q = fa/fc
      r = fb/fc
      s = fb/fa
      p = s* (2.0d0*xm*q* (q-r)- (b-a)* (r-1.0d0))
      q = (q-1.0d0)* (r-1.0d0)* (s-1.0d0)
C
C adjust signs
C
   60 IF (p.GT.0.0d0) q = -q
      p = ABS(p)
      e = d
      IF (((2.0d0*p).GE. (3.0d0*xm*q-ABS(tol1*q))) .OR.
     +    (p.GE.ABS(0.5d0*s*q))) GO TO 70
      e = d
      d = p/q
      GO TO 80
C
C bisection
C
   70 d = xm
      e = d
C
C complete step
C
   80 a = b
      fa = fb
      IF (ABS(d).GT.tol1) b = b + d
      IF (ABS(d).LE.tol1) b = b + SIGN(tol1,xm)
      fb = funct(b)
      IF ((fb* (fc/ABS(fc))).GT.0.0d0) GO TO 20
      GO TO 30
C
C done
C
   90 zeroin = b
      RETURN
      END
