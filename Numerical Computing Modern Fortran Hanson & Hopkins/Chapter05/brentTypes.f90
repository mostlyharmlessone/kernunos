      MODULE brentTypes
      USE set_precision, ONLY : wp

      TYPE, PUBLIC :: brentargs
        REAL(wp) :: ax
        REAL(wp) :: bx
        REAL(wp) :: tol = SQRT(EPSILON(1.0E0_wp))
      END TYPE brentargs

      REAL(wp), PARAMETER :: zero = 0.0E0_wp, half = 0.5E0_wp, & 
         one = 1.0E0_wp, two = 2.0E0_wp, three = 3.0E0_wp

      CONTAINS
        REAL(wp) FUNCTION zeroin(brentType,funct)

!     .. Scalar Arguments ..
        TYPE(brentargs) :: brentType
!     ..
!     .. Function Interface
        INTERFACE 
          FUNCTION funct(x) RESULT(res)
! Use IMPORT to bring wp into local scope
          IMPORT wp
          REAL(wp), INTENT(IN) :: x
          REAL(wp) :: res
          END FUNCTION funct
        END INTERFACE
!
!  A ZERO of the function  f(x)  is computed in the interval [ax,bx].
!
!  Input:
!
!  ax     left endpoint of initial interval
!  bx     right endpoint of initial interval
!  funct  function subprogram which evaluates f(x) for any x in
!         the interval  [ax,bx]
!  tol    desired length of the interval of uncertainty of the
!         final result (>= 0.)
!  Note: ax,bx,tol are components of TYPE(brentArgs)

!  Output:
!
!  ZEROIN - An abscissa approximating a ZERO of  f  in the interval [ax,bx]
!
!      It is assumed  that   f(ax)   and   f(bx)   have  opposite  signs
!  without a check. ZEROIN  returns a ZERO  x  in the given interval
!  [ax,bx]  to within a tolerance  4*macheps*ABS(x)+tol, where macheps  is
!  the  relative machine precision.
!      This function subprogram is a slightly  modified  translation  of
!  the Algol 60 procedure  ZERO  given in  Richard Brent, Algorithms for
!  Minimization Without Derivatives, Prentice-Hall, Inc. (1973).
!  The Fortran version was originally from the book by
!  G. Forsythe, M. Malcolm and C. Moler, Computer Methods for 
!  Mathematical Computations. Prentice-Hall, Inc. (1977).
!
!     ..
!     .. Local Scalars ..
      REAL(wp) :: a,ax,b,bx,c,d,e,eps,fa,fb,fc,p,q,r,s,tol,tol1,xm
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC :: ABS,SIGN
!     ..
      ax = brentType%ax
      bx = brentType%bx
      tol = brentType%tol
      eps = EPSILON(zero)
!
!  initialization
!
      a = ax
      b = bx
      fa = funct(a)
      fb = funct(b)
!
! begin step
!
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
!
! convergence test
!
   40 tol1 = two*eps*ABS(b) + half*tol
      xm = half* (c-b)
      IF ((ABS(xm).LE.tol1) .OR. (fb.EQ.zero)) GO TO 90
!
! is bisection necessary
!
      IF ((ABS(e).LT.tol1) .OR. (ABS(fa).LE.ABS(fb))) GO TO 70
!
! is quadratic interpolation possible
!
      IF (a.NE.c) GO TO 50
!
! linear interpolation
!
      s = fb/fa
      p = two*xm*s
      q = one - s
      GO TO 60
!
! inverse quadratic interpolation
!
   50 q = fa/fc
      r = fb/fc
      s = fb/fa
      p = s* (two*xm*q* (q-r)- (b-a)* (r-one))
      q = (q-one)* (r-one)* (s-one)
!
! adjust signs
!
   60 IF (p.GT.zero) q = -q
      p = ABS(p)
      e = d
      IF (((two*p).GE. (three*xm*q-ABS(tol1*q))) .OR. &
          (p.GE.ABS(half*s*q))) GO TO 70
      e = d
      d = p/q
      GO TO 80
!
! bisection
!
   70 d = xm
      e = d
!
! complete step
!
   80 a = b
      fa = fb
      IF (ABS(d).GT.tol1) b = b + d
      IF (ABS(d).LE.tol1) b = b + SIGN(tol1,xm)
      fb = funct(b)
      IF ((fb* (fc/ABS(fc))).GT.zero) GO TO 20
      GO TO 30
!
! done
!
   90 zeroin = b
      END FUNCTION zeroin

      END MODULE brentTypes
