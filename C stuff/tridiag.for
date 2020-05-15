      SUBROUTINE TRIDIAG(A,B,C,R,U,N)
      PARAMETER (NMAX=360)
      IMPLICIT REAL*4 (A-H,P-Z)
      DIMENSION GAM(NMAX),A(N),B(N),C(N),R(N),U(N)
      IF(B(1).EQ.0.) THEN
	  WRITE(*,*) 'ERROR IN TRIDIAG(1)'
      STOP
	  ENDIF
      BET=B(1)
      U(1)=R(1)/BET
      DO 11 J=2,N 
        GAM(J)=C(J-1)/BET
        BET=B(J)-A(J)*GAM(J)
        IF(BET.EQ.0.) THEN
	    WRITE(*,*) 'ERROR IN TRIDIAG(2)'
        STOP
	    ENDIF
        U(J)=(R(J)-A(J)*U(J-1))/BET
11    CONTINUE
      DO 12 J=N-1,1,-1
        U(J)=U(J)-GAM(J+1)*U(J+1)
12    CONTINUE
      RETURN
      END
