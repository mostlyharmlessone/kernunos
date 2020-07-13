      INTEGER FUNCTION IDAMAX(N,DX,INCX)
*
*  Purpose
*  =======
*
*  IDAMAX determines, for the double precision vector, x the smallest
*         index i such that
*
*     |x_i| = max{ |x_k| }
*
*  where k = 1 + (j-1)*incx, j = 1 to n.
*
*  Parameters
*  ==========
*
*  N    - INTEGER
*         On entry, N specifies the number of elements to be tested.
*         Unchanged on exit.
*  DX   - DOUBLE PRECISION
*         On entry, DX specifies the vector x above.
*         Unchanged on exit.
*  INCX - INTEGER
*         On entry, INCX specifies the increment parameter used to step
*         through the array DX.
*         Unchanged on exit.
*
*
*  Level 1 Blas routine
*
*  Toms algorithm 539 -- Lawson et al, 1979
*  Fortran 77 version -- Tim Hopkins, 1994
*
*     .. Scalar Arguments ..
      INTEGER          INCX,N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION DX(*)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION DMAX
      INTEGER          I,IX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        DABS
*     ..
      IDAMAX = 0
      IF (N.LT.1 .OR. INCX.LE.0) RETURN
      IDAMAX = 1
      IF (N.EQ.1) RETURN
      IF (INCX.EQ.1) GO TO 30
*
*        code for increment not equal to 1
*
      IX = 1
      DMAX = DABS(DX(1))
      IX = IX + INCX
      DO 20 I = 2,N
          IF (DABS(DX(IX)).LE.DMAX) GO TO 10
          IDAMAX = I
          DMAX = DABS(DX(IX))
   10     IX = IX + INCX
   20 CONTINUE
      RETURN
*
*        code for increment equal to 1
*
   30 DMAX = DABS(DX(1))
      DO 40 I = 2,N
          IF (DABS(DX(I)).LE.DMAX) GO TO 40
          IDAMAX = I
          DMAX = DABS(DX(I))
   40 CONTINUE
      RETURN

      END
