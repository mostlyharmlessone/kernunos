      PROGRAM MAIN4 
      IMPLICIT REAL*4 (A-H,P-Z)
      include 'common2.h'
      DIMENSION Y(N)

      DO 100 I=1,N
      X(I)=I**2
100   CONTINUE

      CALL SUB4(Y)  
    
      DO 200 I=1,N
      WRITE(*,*) Y(I)
200   CONTINUE     
    
      END
