      SUBROUTINE SUB4(Y)
      IMPLICIT REAL*4 (A-H,P-Z)
      include 'common2.h'
      DIMENSION Y(N)
      DO 100 I=1,N
      Y(I)=X(I)/2
100   CONTINUE
      RETURN 
      END
