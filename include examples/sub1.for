      SUBROUTINE SUB1(N,X,Y)
      IMPLICIT REAL*4 (A-H,P-Z)

      DIMENSION X(N),Y(N)
      DO 100 I=1,N
      Y(I)=X(I)/2
100   CONTINUE
      RETURN 
      END


