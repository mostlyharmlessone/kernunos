       SUBROUTINE CYCLIC(X,Y,N,Y2)
C      THIS VERSION USES TRIDIAG/SHERMAN-MORRISON (PRESS ET AL)
C      PERIODIC BOUNDARY CONDITION SPLINE PLUG-IN FOR NUMREC SPLINE
       IMPLICIT REAL*4 (A-H,P-Z)
       PARAMETER (PI=3.141592653589793D0)
       PARAMETER (NMAX=360)
       DIMENSION X(N),Y(N),Y2(N),R(NMAX),A(NMAX),B(NMAX),C(NMAX)
       DIMENSION BB(NMAX),U(NMAX),Z(NMAX)
C      INITIALIZE
       PERD=2*PI
       R(1)=(Y(2)-Y(1))/(X(2)-X(1))-(Y(1)-Y(N))/(X(1)-X(N)+PERD)
       A(1)=(X(1)-X(N)+PERD)/6.0
       B(1)=(X(2)-X(N)+PERD)/3.0
       C(1)=(X(2)-X(1))/6.0
       Y2(1)=X(1)
       DO 100 J=2,N-1
       R(J)=(Y(J+1)-Y(J))/(X(J+1)-X(J))-(Y(J)-Y(J-1))/(X(J)-X(J-1))
       A(J)=(X(J)-X(J-1))/6.0
       B(J)=(X(J+1)-X(J-1))/3.0
       C(J)=(X(J+1)-X(J))/6.0
       Y2(J)=X(J)
100    CONTINUE
       J=N
       R(J)=(Y(1)-Y(J))/(X(1)-X(J)+PERD)-(Y(J)-Y(J-1))/(X(J)-X(J-1))
       A(J)=(X(J)-X(J-1))/6.0
       B(J)=(X(1)-X(J-1)+PERD)/3.0
       C(J)=(X(1)-X(J)+PERD)/6.0
       Y2(J)=X(J)
C      BEGIN CYCLIC ALGORITHM
       IF(N.LE.2) THEN
       WRITE(*,*)'N TOO SMALL IN CYCLIC'
       ENDIF
       IF(N.GT.NMAX) THEN
       WRITE(*,*)'NMAX TOO SMALL IN CYCLIC'
       ENDIF
       ALPHA=C(N)
       BETA=A(1)
       GAMMA=-B(1)
       BB(1)=B(1)-GAMMA
       BB(N)=B(N)-ALPHA*BETA/GAMMA
       DO 110 I=2,N-1
       BB(I)=B(I)
110    CONTINUE
       CALL TRIDIAG(A,BB,C,R,Y2,N)
       U(1)=GAMMA
       U(N)=ALPHA
       DO 200 I=2,N-1
       U(I)=0
200    CONTINUE
       CALL TRIDIAG(A,BB,C,U,Z,N)
       FACT=(Y2(1)+BETA*Y2(N)/GAMMA)/(1.+Z(1)+BETA*Z(N)/GAMMA)
       DO 300 I=1,N
       Y2(I)=Y2(I)-FACT*Z(I)
300    CONTINUE
       RETURN
       END
