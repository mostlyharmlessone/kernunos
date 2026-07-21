      MODULE LapackInterface
      IMPLICIT NONE
!     interfaces for FORTRAN77 LAPACK to fortran 90+
!     adapted from http://www.siam.org/books/ot134 Numerical Computing with Modern Fortran Richard J.Hanson and Tim Hopkins SIAM

        INTERFACE

!        Not part of LAPACK, but included here anyway
!        Copyright (c) 2021   Anthony M de Beus
!        LAPACK "API": Arguments copied and modified from -- LAPACK routine (version 3.1) --
!          Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
!          November 2006

         SUBROUTINE GaussJordan( N, NRHS, A, LDA, B, LDB, INFO )
!        PURPOSE matrix solver, O(n^3)
          INTEGER, PARAMETER :: wp = KIND(0.0D0) ! working precision
!         .. Scalar Arguments ..
          INTEGER,INTENT(IN)   :: LDA, LDB, N, NRHS
          INTEGER, INTENT(OUT) :: INFO
!         .. Array Arguments ..
          REAL(wp),INTENT(INOUT) ::  A( LDA, LDA ), B( LDB, NRHS )
         END SUBROUTINE GaussJordan

         SUBROUTINE DCBSV( N, KU, NRHS, AB, LDAB, B, LDB, INFO )
!         USE OMP_LIB
!        PURPOSE solves the cyclic/periodic general banded system, see LAPACK routine DGBSV by contrast
!        using an O(N/KU+KU)xKUxKU algorithm
          INTEGER, PARAMETER :: wp = KIND(0.0D0) ! working precision
!        .. Scalar Arguments ..
          INTEGER, Intent(IN) ::  KU, LDAB, LDB, N, NRHS
          INTEGER, INTENT(OUT) :: INFO
!        .. Array Arguments ..
          Real(wp), Intent(IN) :: AB( ldab, * )
          Real(wp), Intent(INOUT) ::  B( ldb, * )
         END SUBROUTINE DCBSV

         SUBROUTINE DCTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
          USE OMP_LIB
!         PURPOSE solves the cyclic/periodic tridiagonal system, see LAPACK routine DGTSV for comparison
!         Copyright (c) 2021   Anthony M de Beus
          INTEGER, PARAMETER :: wp = KIND(0.0D0) ! working precision
!         .. Scalar Arguments ..
          INTEGER, INTENT(IN) :: LDB, N, NRHS
          INTEGER, INTENT(OUT) :: INFO
!         .. Array Arguments ..
          REAL(wp), INTENT(IN) :: D( N ), DL( N ), DU( N )  ! no output no LU factors
          REAL(wp), INTENT(INOUT) :: B( LDB, NRHS )         ! on entry RHS, on exit, solution
        END SUBROUTINE DCTSV


!        Not part of LAPACK, part of /http://netlib.org/math
!        Copyright (c) 1996 California Institute of Technology, Pasadena, CA. ALL RIGHTS RESERVED.

!         SUBROUTINE DC2FIT(XI,YI,SDI,NXY,B,NB,W,NW,YKNOT,YPKNOT,SIGFAC, IERR1)
!!        Based on Government Sponsored Research NAS7-03001.
!!        >> 2000-12-01 DC2FIT Krogh  Dim. SDI(*) instead NXY.
!!        >> 1995-11-21 DC2FIT Krogh  Converted from SFTRAN to Fortran 77.
!!        >> 1994-10-19 DC2FIT Krogh  Changes to use M77CON
!!        >> 1994-01-31 DC2FIT CLL Added test for SDI(i) .le. 0 when SDI(1) > 0.
!!        >> 1990-01-23 CLL Deleted ref to unused variable NX in call to IERM1
!!        >> 1989-10-20 CLL
!!        >> 1987-10-22 DC2FIT Lawson  Initial code.
!!           Least squares fit to discrete data by a C-2 cubic spline.
!!        Algorithm and program designed by C.L.Lawson and R.J.Hanson.
!!        The general approach but not the complete code is given in
!!        'SOLVING LEAST SQUARES PROBLEMS', by Lawson and Hanson, Prentice-Hall, 1974.
!!        Programming and later changes and corrections by Lawson,Hanson,
!!        T.Lang, and D.Campbell, Sept 1968, Nov 1969, and Aug 1970.
!          INTEGER IERR1, NW, NXY, NB
!          DOUBLE PRECISION XI(NXY), YI(NXY), SDI(*), B(NB), W(NW, 5)
!          DOUBLE PRECISION YKNOT(NB), YPKNOT(NB)
!          DOUBLE PRECISION SIGFAC
!         END SUBROUTINE DC2FIT

!        These are part of LAPACK
!        LAPACK driver routine (version 3.7.0) --
!        LAPACK is a software package provided by Univ. of Tennessee,    --
!        Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!        December 2016

         SUBROUTINE DSYEV( JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
!     .. Scalar Arguments ..
         CHARACTER          JOBZ, UPLO
         INTEGER            INFO, LDA, LWORK, N
!     .. Array Arguments ..
         DOUBLE PRECISION   A( LDA, * ), W( * ), WORK( * )
         END SUBROUTINE DSYEV

         SUBROUTINE DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )
!     .. Scalar Arguments ..
         INTEGER            INFO, LDA, LDB, N, NRHS
!     .. Array Arguments ..
         INTEGER            IPIV( * )
         DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
         END SUBROUTINE DGESV
         
         SUBROUTINE DGELS( TRANS, M, N, NRHS, A, LDA, B, LDB, WORK, LWORK, INFO )
!     .. Scalar Arguments ..
         CHARACTER          TRANS
         INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS
!     .. Array Arguments ..
         DOUBLE PRECISION   A( LDA, * ), B( LDB, * ), WORK( * )
         END SUBROUTINE DGELS

          SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
!     .. Scalar Arguments ..
          INTEGER            INFO, LDA, M, N
!     .. Array Arguments ..
          INTEGER            IPIV( * )
          DOUBLE PRECISION   A( LDA, * )
          END SUBROUTINE DGETRF

          SUBROUTINE DGETRS( TRANS, N, NRHS, A, LDA, IPIV, B, LDB, INFO )
!     .. Scalar Arguments ..
          CHARACTER          TRANS
          INTEGER            INFO, LDA, LDB, N, NRHS
!     .. Array Arguments ..
          INTEGER            IPIV( * )
          DOUBLE PRECISION   A( LDA, * ), B( LDB, * )
          END SUBROUTINE DGETRS

          SUBROUTINE DGBSV( N, KL, KU, NRHS, AB, LDAB, IPIV, B, LDB, INFO )
!      .. Scalar Arguments ..
          INTEGER            INFO, KL, KU, LDAB, LDB, N, NRHS
!      .. Array Arguments ..
          INTEGER            IPIV( * )
          DOUBLE PRECISION   AB( ldab, * ), B( ldb, * )
          
          END SUBROUTINE DGBSV
          
          SUBROUTINE DGTSV( N, NRHS, DL, D, DU, B, LDB, INFO )
!      .. Scalar Arguments ..
          INTEGER            INFO, LDB, N, NRHS
!      .. Array Arguments ..
          DOUBLE PRECISION   B( LDB, * ), D( * ), DL( * ), DU( * )
          END SUBROUTINE DGTSV
                  
          SUBROUTINE DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
!      .. Scalar Arguments ..
          INTEGER            INFO, LDA, LWORK, N
!       .. Array Arguments ..
          INTEGER            IPIV( * )
          DOUBLE PRECISION   A( LDA, * ), WORK( * )
          END SUBROUTINE DGETRI

          SUBROUTINE XERBLA( SRNAME, INFO )
!         .. Scalar Arguments ..
          CHARACTER(6)        SRNAME
          INTEGER            INFO
          END SUBROUTINE XERBLA  

          SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
!         .. Scalar Arguments ..
          DOUBLE PRECISION ALPHA,BETA
          INTEGER K,LDA,LDB,LDC,M,N
          CHARACTER TRANSA,TRANSB
!         .. Array Arguments ..
          DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*) 
          END SUBROUTINE DGEMM  

          SUBROUTINE DGEMV(TRANS,M,N,ALPHA,A,LDA,X,INCX,BETA,Y,INCY)
!         .. Scalar Arguments ..
          DOUBLE PRECISION ALPHA,BETA
          INTEGER INCX,INCY,LDA,M,N
          CHARACTER TRANS
!         .. Array Arguments ..
          DOUBLE PRECISION A(LDA,*),X(*),Y(*) 
          END SUBROUTINE DGEMV   

          FUNCTION DNRM2(N,DX,INCX) RESULT(RES)
          DOUBLE PRECISION RES
!          .. Scalar Arguments ..
          INTEGER  INCX,N
!         .. Array Arguments ..
          DOUBLE PRECISION DX(*)
          END FUNCTION DNRM2

        END INTERFACE    

      END MODULE LapackInterface
