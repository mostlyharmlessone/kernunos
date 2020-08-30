      SUBROUTINE DSWAP(N,DX,INCX,DY,INCY)
*
*  Purpose
*  =======
*
*  DSWAP exchanges elements of the double precision vectors x and y.
*
*  The elements of the x-vector used are
*
*     1 + (i-1)*incx,   if incx >= 0,
*     1 + (n-i)*|incx|, if incx < 0.
*
*  and similarly for y and incy.
*
*  Uses unrolled loops.
*
*  Parameters
*  ==========
*
*  N    - INTEGER
*         On entry, N specifies the number of elements to be swapped.
*         Unchanged on exit.
*  DX   - DOUBLE PRECISION
*         On entry, DX specifies the vector x above.
*         On exit, DX contains the data originally in DY.
*  INCX - INTEGER
*         On entry, INCX specifies the increment parameter used to step
*         through the array DX.
*         Unchanged on exit.
*  DY   - DOUBLE PRECISION
*         On entry, DY specifies the vector y above.
*         On exit, DY contains the data originally in DX.
*  INCY - INTEGER
*         On entry, INCY specifies the increment parameter used to step
*         through the array DY.
*         Unchanged on exit.
*
*
*  Level 1 Blas routine
*
*  Toms algorithm 539 -- Lawson et al, 1979
*  Fortran 77 version -- Tim Hopkins, 1994
*
*     .. Scalar Arguments ..
      INTEGER          INCX,INCY,N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION DX(*),DY(*)
*     ..
*     .. Local Scalars ..
      DOUBLE PRECISION DTEMP
      INTEGER          I,IX,IY,M,MP1
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MOD
*     ..
      IF (N.LE.0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
*
*       code for unequal increments or equal increments not equal
*         to 1
*
      IX = 1
      IY = 1
      IF (INCX.LT.0) IX = (-N+1)*INCX + 1
      IF (INCY.LT.0) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
          DTEMP = DX(IX)
          DX(IX) = DY(IY)
          DY(IY) = DTEMP
          IX = IX + INCX
          IY = IY + INCY
   10 CONTINUE
      RETURN
*
*       code for both increments equal to 1
*
*
*       clean-up loop
*
   20 M = MOD(N,3)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
          DTEMP = DX(I)
          DX(I) = DY(I)
          DY(I) = DTEMP
   30 CONTINUE
      IF (N.LT.3) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
          DTEMP = DX(I)
          DX(I) = DY(I)
          DY(I) = DTEMP
          DTEMP = DX(I+1)
          DX(I+1) = DY(I+1)
          DY(I+1) = DTEMP
          DTEMP = DX(I+2)
          DX(I+2) = DY(I+2)
          DY(I+2) = DTEMP
   50 CONTINUE
      RETURN

      END
