      SUBROUTINE SUB2(Y)
      IMPLICIT REAL*4 (A-H,P-Z)
      PARAMETER (N=22)
      COMMON /VECS/ X(N)
      DIMENSION Y(N)
      DO 100 I=1,N
      Y(I)=X(I)/2
100   CONTINUE
      RETURN 
      END

