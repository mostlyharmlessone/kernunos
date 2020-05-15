	  subroutine speedtst0
	  implicit real*4 (a-h,p-z)
          parameter (NMAX=60)
          PARAMETER (PI=3.141592653589793D0)
	  dimension x(NMAX),y(NMAX),y2a(NMAX),y2b(NMAX),y2c(NMAX)
          N=59
C         JUST SOME TEST DATA
	  do 100 i=1,N
	  x(i)=2*PI*(i-1.0)/N
	  y(i)=6+0.02*Cos(x(i))
100	  continue
          CALL cfun(X,Y,N)
	  end subroutine

	  subroutine speedtst1
	  implicit real*4 (a-h,p-z)
          parameter (NMAX=60)
          PARAMETER (PI=3.141592653589793D0)
	  dimension x(NMAX),y(NMAX),y2a(NMAX),y2b(NMAX),y2c(NMAX)
          N=59
C         JUST SOME TEST DATA
	  do 100 i=1,N
	  x(i)=2*PI*(i-1.0)/N
	  y(i)=6+0.02*Cos(x(i))
100	  continue
          CALL PSPLINE(X,Y,N,Y2A)
	  end subroutine

	  subroutine speedtst2
	  implicit real*4 (a-h,p-z)
          parameter (NMAX=60)
          PARAMETER (PI=3.141592653589793D0)
	  dimension x(NMAX),y(NMAX),y2a(NMAX),y2b(NMAX),y2c(NMAX)
          N=59
C         JUST SOME TEST DATA
	  do 100 i=1,N
	  x(i)=2*PI*(i-1.0)/N
	  y(i)=6+0.02*Cos(x(i))
100	  continue
	  DO 110 II=1,3600
          CALL PSPLI(X,Y,N,Y2C)
110	  CONTINUE          
	  end subroutine

	  subroutine speedtst3
	  implicit real*4 (a-h,p-z)
          parameter (NMAX=60)
          PARAMETER (PI=3.141592653589793D0)
	  dimension x(NMAX),y(NMAX),y2a(NMAX),y2b(NMAX),y2c(NMAX)
          N=59
C         JUST SOME TEST DATA
	  do 100 i=1,N
	  x(i)=2*PI*(i-1.0)/N
	  y(i)=6+0.02*Cos(x(i))
100	  continue
	  DO 120 II=1,3600
          CALL CYCLIC(X,Y,N,Y2B)
120	  CONTINUE 
	  end subroutine

      SUBROUTINE PSPLINE(X,Y,N,Y2)
C     THIS VERSION USES LUDCMP/LUBKSB
C     PERIODIC BOUNDARY CONDITION SPLINE PLUG-IN FOR NUMREC SPLINE
      PARAMETER (NMAX=60)
      PARAMETER (PI=3.141592653589793D0)
      DIMENSION X(N),Y(N),Y2(N),R(NMAX),A(NMAX),B(NMAX),C(NMAX)
      DIMENSION AM(NMAX,NMAX),INDX(NMAX)
C     INITIALIZE
      DO 50 II=1,N
	  DO 50 JJ=1,N
      AM(II,JJ)=0
50    CONTINUE
C	  USING LU DECOMP
C     MAKE THE MATRIX
      PERD=2*PI
      R(1)=(Y(2)-Y(1))/(X(2)-X(1))-(Y(1)-Y(N))/(X(1)-X(N)+PERD)
      A(1)=(X(1)-X(N)+PERD)/6.0
      B(1)=(X(2)-X(N)+PERD)/3.0
      C(1)=(X(2)-X(1))/6.0
	  Y2(1)=R(1)
	  AM(1,N)=A(1)
	  AM(1,1)=B(1)
	  AM(1,2)=C(1)
	  DO 100 J=2,N-1
      R(J)=(Y(J+1)-Y(J))/(X(J+1)-X(J))-(Y(J)-Y(J-1))/(X(J)-X(J-1))
      A(J)=(X(J)-X(J-1))/6.0
      B(J)=(X(J+1)-X(J-1))/3.0
      C(J)=(X(J+1)-X(J))/6.0
	  AM(J,J-1)=A(J)
	  AM(J,J)=B(J)
	  AM(J,J+1)=C(J)
	  Y2(J)=R(J)
100   CONTINUE
	  J=N
      R(J)=(Y(1)-Y(J))/(X(1)-X(J)+PERD)-(Y(J)-Y(J-1))/(X(J)-X(J-1))
      A(J)=(X(J)-X(J-1))/6.0
      B(J)=(X(1)-X(J-1)+PERD)/3.0
      C(J)=(X(1)-X(J)+PERD)/6.0
	  AM(J,J-1)=A(J)
	  AM(J,J)=B(J)
	  AM(J,1)=C(J)
	  Y2(J)=R(J)
      CALL LUDCMP(AM,N,NMAX,INDX,D)
      CALL LUBKSB(AM,N,NMAX,INDX,Y2)
	  RETURN
	  END

      SUBROUTINE LUDCMP(A,N,NP,INDX,D)
      PARAMETER (NMAX=60,TINY=1.0E-20)
      DIMENSION A(NP,NP),INDX(N),VV(NMAX)
      D=1.
      DO 12 I=1,N
        AAMAX=0.
        DO 11 J=1,N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
11      CONTINUE
        IF (AAMAX.EQ.0.) PAUSE 'Singular matrix.'
        VV(I)=1./AAMAX
12    CONTINUE
      DO 19 J=1,N
        IF (J.GT.1) THEN
          DO 14 I=1,J-1
            SUM=A(I,J)
            IF (I.GT.1)THEN
              DO 13 K=1,I-1
                SUM=SUM-A(I,K)*A(K,J)
13            CONTINUE
              A(I,J)=SUM
            ENDIF
14        CONTINUE
        ENDIF
        AAMAX=0.
        DO 16 I=J,N
          SUM=A(I,J)
          IF (J.GT.1)THEN
            DO 15 K=1,J-1
              SUM=SUM-A(I,K)*A(K,J)
15          CONTINUE
            A(I,J)=SUM
          ENDIF
          DUM=VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
16      CONTINUE
        IF (J.NE.IMAX)THEN
          DO 17 K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
17        CONTINUE
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J)=IMAX
        IF(J.NE.N)THEN
          IF(A(J,J).EQ.0.)A(J,J)=TINY
          DUM=1./A(J,J)
          DO 18 I=J+1,N
            A(I,J)=A(I,J)*DUM
18        CONTINUE
        ENDIF
19    CONTINUE
      IF(A(N,N).EQ.0.)A(N,N)=TINY
      RETURN
      END

      SUBROUTINE LUBKSB(A,N,NP,INDX,B)
      DIMENSION A(NP,NP),INDX(N),B(N)
      II=0
      DO 12 I=1,N
        LL=INDX(I)
        SUM=B(LL)
        B(LL)=B(I)
        IF (II.NE.0)THEN
          DO 11 J=II,I-1
            SUM=SUM-A(I,J)*B(J)
11        CONTINUE
        ELSE IF (SUM.NE.0.) THEN
          II=I
        ENDIF
        B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
        SUM=B(I)
        IF(I.LT.N)THEN
          DO 13 J=I+1,N
            SUM=SUM-A(I,J)*B(J)
13        CONTINUE
        ENDIF
        B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END

       SUBROUTINE PSPLI(X,Y,N,Y2)
C      THIS VERSION USES MY ALGORITHM
C      PERIODIC BOUNDARY CONDITION SPLINE PLUG-IN FOR NUMREC SPLINE
       IMPLICIT REAL*4 (A-H,P-Z)
       PARAMETER (PI=3.141592653589793D0)
       PARAMETER (NMAX=60)
       DIMENSION X(N),Y(N),Y2(N),R(NMAX),A(NMAX),B(NMAX),C(NMAX)
       DIMENSION UD(2,2,NMAX),UE(2,NMAX)
C      INITIALIZE
            PERD=2*PI
      R(1)=(Y(2)-Y(1))/(X(2)-X(1))-(Y(1)-Y(N))/(X(1)-X(N)+PERD)
        A(1)=(X(1)-X(N)+PERD)/6.0
        B(1)=(X(2)-X(N)+PERD)/3.0
        C(1)=(X(2)-X(1))/6.0
        DO 100 J=2,N-1
       R(J)=(Y(J+1)-Y(J))/(X(J+1)-X(J))-(Y(J)-Y(J-1))/(X(J)-X(J-1))
        A(J)=(X(J)-X(J-1))/6.0
        B(J)=(X(J+1)-X(J-1))/3.0
        C(J)=(X(J+1)-X(J))/6.0
100    CONTINUE
        J=N
       R(J)=(Y(1)-Y(J))/(X(1)-X(J)+PERD)-(Y(J)-Y(J-1))/(X(J)-X(J-1))
        A(J)=(X(J)-X(J-1))/6.0
        B(J)=(X(1)-X(J-1)+PERD)/3.0
        C(J)=(X(1)-X(J)+PERD)/6.0
C      FIRST EQUATION
       J=1
        DET=B(J)*B(1-J+N)-A(J)*C(1-J+N)
        UD(1,1,J)=-C(J)*B(1-J+N)/DET
        UD(1,2,J)=A(1-J+N)*C(1-J+N)/DET
        UD(2,1,J)=A(J)*C(J)/DET
        UD(2,2,J)=-A(1-J+N)*B(J)/DET
        UE(1,J)=R(J)*B(1-J+N)
        U1X=-C(1-J+N)*R(1-J+N)
        UE(1,J)=(UE(1,J)+U1X)/DET
        UE(2,J)=-A(J)*R(J)
        U2X=R(1-J+N)*B(J)
        UE(2,J)=(UE(2,J)+U2X)/DET
C      ALL BUT THE LAST EQUATION
        DO 200 J=2,INT(1+(N-1)/2)
        DET=B(J)*B(1-J+N)+A(J)*B(1-J+N)*UD(1,1,-1+J)
        DET=DET-A(J)*C(1-J+N)*UD(1,2,-1+J)*UD(2,1,-1+J)
        DET=DET+B(J)*C(1-J+N)*UD(2,2,-1+J)
        DET=DET+A(J)*C(1-J+N)*UD(1,1,-1+J)*UD(2,2,-1+J)
        UD(1,1,J)=-((C(J)*(B(1-J+N)+C(1-J+N)*UD(2,2,-1+J)))/DET)
        UD(1,2,J)=(A(J)*A(1-J+N)*UD(1,2,-1+J))/DET
        UD(2,1,J)=(C(J)*C(1-J+N)*UD(2,1,-1+J))/DET
        UD(2,2,J)=-((A(1-J+N)*(B(J)+A(J)*UD(1,1,-1+J)))/DET)
        UE(1,J)=-A(J)*(R(1-J+N)-C(1-J+N)*UE(2,-1+J))*UD(1,2,-1+J)
        U1X=(R(J)-A(J)*UE(1,-1+J))*(B(1-J+N)+C(1-J+N)*UD(2,2,-1+J))
        UE(1,J)=(UE(1,J)+U1X)/DET
        UE(2,J)=(R(1-J+N)-C(1-J+N)*UE(2,-1+J))*(B(J)+A(J)*UD(1,1,-1+J))
        U2X=-C(1-J+N)*(R(J)-A(J)*UE(1,-1+J))*UD(2,1,-1+J)
        UE(2,J)=(UE(2,J)+U2X)/DET
200    CONTINUE
       J=INT(N/2)
        IF(J.EQ.(N/2.0)) THEN
C      EVEN CASE (2x2)
C      NO UD(,,J); UE(,J) IS THE SOLUTION AT J,J+1
        DET=B(J)*B(1+J)-A(1+J)*C(J)
        DET=DET+A(J)*B(1+J)*UD(1,1,-1+J)
        DET=DET-A(J)*A(1+J)*UD(1,2,-1+J)
        DET=DET-C(J)*C(1+J)*UD(2,1,-1+J)
        DET=DET-A(J)*C(1+J)*UD(1,2,-1+J)*UD(2,1,-1+J)
        DET=DET+B(J)*C(1+J)*UD(2,2,-1+J)
        DET=DET+A(J)*C(1+J)*UD(1,1,-1+J)*UD(2,2,-1+J)
        UE(1,J)=(R(1+J)-C(1+J)*UE(2,-1+J))*(-C(J)-A(J)*UD(1,2,-1+J))
        U1X=(R(J)-A(J)*UE(1,-1+J))*(B(1+J)+C(1+J)*UD(2,2,-1+J))
        UE(1,J)=(UE(1,J)+U1X)/DET 
        UE(2,J)=(R(1+J)-C(1+J)*UE(2,-1+J))*(B(J)+A(J)*UD(1,1,-1+J))
        U2X=(R(J)-A(J)*UE(1,-1+J))*(-A(1+J)-C(1+J)*UD(2,1,-1+J))
        UE(2,J)=(UE(2,J)+U2X)/DET 
C      READY FOR BACKSUBSTITUTION
        Y2(J)=UE(1,J)
        Y2(J+1)=UE(2,J)
        ELSE
C        (N-2*J+1.EQ.2)
C      ODD CASE (3x3)
C      NO UD(,,J); Y2(J ETC.) IS THE SOLUTION AT J,J+1,J+2
        DET=B(J)*B(1+J)*B(2+J)-A(1+J)*B(2+J)*C(J)
        DET=DET-A(2+J)*B(J)*C(1+J)
        DET=DET+A(J)*B(1+J)*B(2+J)*UD(1,1,-1+J)
        DET=DET-A(J)*A(2+J)*C(1+J)*UD(1,1,-1+J)
        DET=DET+A(J)*A(1+J)*A(2+J)*UD(1,2,-1+J)
        DET=DET+C(J)*C(1+J)*C(2+J)*UD(2,1,-1+J)
        DET=DET-A(J)*B(1+J)*C(2+J)*UD(1,2,-1+J)*UD(2,1,-1+J)
        DET=DET+B(J)*B(1+J)*C(2+J)*UD(2,2,-1+J)
        DET=DET-A(1+J)*C(J)*C(2+J)*UD(2,2,-1+J)
        DET=DET+A(J)*B(1+J)*C(2+J)*UD(1,1,-1+J)*UD(2,2,-1+J)
        Y2(J)=(R(2+J)-C(2+J)*UE(2,-1+J))
        Y2(J)=Y2(J)*(C(J)*C(1+J)-A(J)*B(1+J)*UD(1,2,-1+J))
        Y2X=R(J)-A(J)*UE(1,-1+J)
        Y2X=Y2X*(B(1+J)*B(2+J)-A(2+J)*C(1+J)+B(1+J)*C(2+J)*UD(2,2,-1+J))
        Y2Y=R(1+J)*(-B(2+J)*C(J)+A(J)*A(2+J)*UD(1,2,-1+J))
        Y2Y=Y2Y+R(1+J)*(-C(J)*C(2+J)*UD(2,2,-1+J))
        Y2(J)=(Y2(J)+Y2X+Y2Y)/DET
        Y2(J+1)=R(2+J)-C(2+J)*UE(2,-1+J)
        Y2X=-B(J)*C(1+J)-A(J)*C(1+J)*UD(1,1,-1+J)
        Y2X=Y2X+A(J)*A(1+J)*UD(1,2,-1+J)
        Y2(J+1)=Y2(J+1)*Y2X
        Y2X=R(J)-A(J)*UE(1,-1+J)
        Y2Y=-A(1+J)*B(2+J)+C(1+J)*C(2+J)*UD(2,1,-1+J)
        Y2Y=Y2Y-A(1+J)*C(2+J)*UD(2,2,-1+J)
        Y2(J+1)=Y2(J+1)+Y2X*Y2Y
        Y2X=B(J)*B(2+J)+A(J)*B(2+J)*UD(1,1,-1+J)
        Y2X=Y2X-A(J)*C(2+J)*UD(1,2,-1+J)*UD(2,1,-1+J)
        Y2X=Y2X+B(J)*C(2+J)*UD(2,2,-1+J)
        Y2X=Y2X+A(J)*C(2+J)*UD(1,1,-1+J)*UD(2,2,-1+J)
        Y2(J+1)=(Y2(J+1)+R(1+J)*Y2X)/DET
        Y2(J+2)=R(2+J)-C(2+J)*UE(2,-1+J)
        Y2X=B(J)*B(1+J)-A(1+J)*C(J)
        Y2X=Y2X+A(J)*B(1+J)*UD(1,1,-1+J)
        Y2(J+2)=Y2(J+2)*Y2X
        Y2X=R(J)-A(J)*UE(1,-1+J)
        Y2Y=A(1+J)*A(2+J)-B(1+J)*C(2+J)*UD(2,1,-1+J)
        Y2(J+2)=Y2(J+2)+Y2X*Y2Y
        Y2X=-A(2+J)*B(J)-A(J)*A(2+J)*UD(1,1,-1+J)
        Y2X=Y2X+C(J)*C(2+J)*UD(2,1,-1+J)
        Y2(J+2)=(Y2(J+2)+R(1+J)*Y2X)/DET
C      READY FOR BACKSUBSTITUTION
        ENDIF
C        BACKSUBSTITUTION
        DO 300 I=J-1,1,-1
        Y2(I)=UE(1,I)+UD(1,1,I)*Y2(I+1)+UD(1,2,I)*Y2(N-I)
        Y2(N-I+1)=UE(2,I)+UD(2,1,I)*Y2(I+1)+UD(2,2,I)*Y2(N-I)
300    CONTINUE
       RETURN
       END

       SUBROUTINE CYCLIC(X,Y,N,Y2)
C      THIS VERSION USES TRIDIAG/SHERMAN-MORRISON (PRESS ET AL)
C      PERIODIC BOUNDARY CONDITION SPLINE PLUG-IN FOR NUMREC SPLINE
       IMPLICIT REAL*4 (A-H,P-Z)
       PARAMETER (PI=3.141592653589793D0)
       PARAMETER (NMAX=60)
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

      SUBROUTINE TRIDIAG(A,B,C,R,U,N)
      PARAMETER (NMAX=60)
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
