       SUBROUTINE PSPLI(X,Y,N,Y2)
C      THIS VERSION USES MY ALGORITHM
C      PERIODIC BOUNDARY CONDITION SPLINE PLUG-IN FOR NUMREC SPLINE
       IMPLICIT REAL*4 (A-H,P-Z)
       PARAMETER (PI=3.141592653589793D0)
       PARAMETER (NMAX=360)
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
