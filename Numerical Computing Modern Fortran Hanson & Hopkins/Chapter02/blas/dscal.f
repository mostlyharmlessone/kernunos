      SUBROUTINE DSCAL(N,DA,DX,INCX)
*
*  Purpose
*  =======
*
*  DSCAL computes x = a*x for a double precision scalar a and
*        elements of a double precision vector x.
*
*  The elements of the x-vector used are
*
*     1 + (i-1)*incx,   if incx >= 0.
*
*  Uses unrolled loops.
*
*  Parameters
*  ==========
*
*  N    - INTEGER
*         On entry, N specifies the number of elements to be scaled.
*         Unchanged on exit.
*  DA   - DOUBLE PRECISION
*         On entry, DA specifies the value of the scalar a above.
*         Unchanged on exit.
*  DX   - DOUBLE PRECISION
*         On entry, DX specifies the vector x above.
*         On exit, DX contains the scaled data.
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
      DOUBLE PRECISION DA
      INTEGER          INCX,N
*     ..
*     .. Array Arguments ..
      DOUBLE PRECISION DX(*)
*     ..
*     .. Local Scalars ..
      INTEGER          I,M,MP1,NINCX
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC        MOD
*     ..
      IF (N.LE.0 .OR. INCX.LE.0) RETURN
      IF (INCX.EQ.1) GO TO 20
*
*        code for increment not equal to 1
*
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
          DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
*
*        code for increment equal to 1
*
*
*        clean-up loop
*
   20 M = MOD(N,5)
      IF (M.EQ.0) GO TO 40
      DO 30 I = 1,M
          DX(I) = DA*DX(I)
   30 CONTINUE
      IF (N.LT.5) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
          DX(I) = DA*DX(I)
          DX(I+1) = DA*DX(I+1)
          DX(I+2) = DA*DX(I+2)
          DX(I+3) = DA*DX(I+3)
          DX(I+4) = DA*DX(I+4)
   50 CONTINUE
      RETURN

      END
