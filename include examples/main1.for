      PROGRAM MAIN1  
      IMPLICIT REAL*4 (A-H,P-Z)
      PARAMETER (N=22)
      DIMENSION X(N),Y(N)

      DO 100 I=1,N
      X(I)=I**2
100   CONTINUE

      CALL SUB1(N,X,Y)  
    
      DO 200 I=1,N
      WRITE(*,*) Y(I)
200   CONTINUE     
    
      END
