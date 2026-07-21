MODULE spline_interfaces
 ! alphabetical order
 ! gathering the interfaces of spline subprograms not in a module
 INTERFACE

 SUBROUTINE AdjustRadSplineCenter
 END SUBROUTINE AdjustRadSplineCenter
      
 SUBROUTINE CubicSplineQuad(ii,iflag,rv,zv,z2v,n,r,z) !Forsythe p.90 cubic spline integration
  USE set_precision, ONLY  : wp
  INTEGER, INTENT(IN) :: n,iflag,ii
  REAL(wp), INTENT(INOUT) ::  rv(n)
  REAL(wp), INTENT(IN) ::  zv(n),z2v(n),r
  REAL(wp), INTENT(OUT) :: z
 END SUBROUTINE 

 SUBROUTINE LSQEval(M2,c,v,f,ft,ftt,fttt)
  USE set_precision, ONLY : wp
  INTEGER, INTENT(IN) :: M2
  REAL(wp), INTENT(IN) :: v,c(M2)
  REAL(wp),INTENT(OUT),OPTIONAL :: f,ft,ftt,fttt
 END SUBROUTINE

 SUBROUTINE lsqfit(t,z,M1,M2,c)
  USE set_precision, ONLY  :  wp
  use LapackInterface, ONLY : dgetrf, dgetrs !, dgels, GaussJordan
  REAL(wp), INTENT(IN) :: t(M1),z(M1)
  INTEGER, INTENT(IN) :: M1,M2
  REAL(wp), INTENT(OUT) ::c(M2)
 END SUBROUTINE

 SUBROUTINE LSQspline(t, y, m, a, z, z2, n, err_report, periodic, csr , sparse)
   USE set_precision, ONLY: wp
! Number of data points
     INTEGER, INTENT(IN) :: m
! Data points
     REAL(wp), INTENT(IN) ::  t(m),y(m)
! number of knots:
     INTEGER, INTENT(IN) :: n
! knots function, second derivatives of spline at knots
     REAL(wp), INTENT(OUT) :: a(n),z(n),z2(n)
! periodic = true means periodic bc, false, natural spline conditions
! csr = true means using the Gram product ATA and the 3n x 3n system, false means using the H&H M+3*n system
! sparse = true means using superlu to solve the system, false means converting to a dense matrix and using LAPACK
! obviously sparse = true is suitable for large n
     LOGICAL, INTENT(IN) :: periodic, csr , sparse
! error reporting
     INTEGER, INTENT(OUT) :: err_report
 END SUBROUTINE

 SUBROUTINE MakeRadSplineCenter(dat,error_report)
  use, INTRINSIC :: iso_c_binding, ONLY : c_int,c_int64_t
  USE set_precision, ONLY : wp
  INTEGER(c_int64_t), INTENT(IN) :: dat
  INTEGER(c_int), INTENT(OUT) :: error_report
 END SUBROUTINE

 SUBROUTINE nspline(r,z,n,z2,err_report)
  USE set_precision, ONLY : wp
  INTEGER, INTENT(IN) :: n
  REAL(wp), INTENT(IN) ::  r(n)
  REAL(wp), INTENT(IN) ::  z(n)
  REAL(wp), INTENT(OUT) :: z2(n)
  INTEGER, INTENT(OUT) :: err_report
 END SUBROUTINE

 SUBROUTINE nsplineCenter(ii,r,z,n,z2,err_report)
  USE set_precision, ONLY  : wp
  INTEGER, INTENT(IN) :: ii,n
  REAL(wp), INTENT(IN) ::  r(n)
  REAL(wp), INTENT(IN) ::  z(n)
  REAL(wp), INTENT(OUT) :: z2(n)
  INTEGER, INTENT(OUT) :: err_report
 END SUBROUTINE

SUBROUTINE pspli(t,z,n,zt2,err_report)
 USE set_precision, ONLY  :  wp
 REAL(wp), INTENT(IN) :: t(n)
REAL(wp), INTENT(IN) :: z(n)
 INTEGER, INTENT(IN) :: n
 REAL(wp), INTENT(OUT) ::zt2(n)
 INTEGER, INTENT(OUT) :: err_report
 REAL(wp) :: PERD,error
 REAL(wp) :: d(n),a(n),b(n),c(n)
END SUBROUTINE
 
 SUBROUTINE SplineCenter(dat,jj,r,z,zr2,n,u,err_report)
 USE set_precision, ONLY  : wp
 use, INTRINSIC :: iso_c_binding, ONLY : c_int,c_int64_t
  INTEGER(c_int64_t), INTENT(IN) :: dat
  INTEGER, INTENT(IN) :: n,jj
  REAL(wp), INTENT(IN) ::  r(n)
  REAL(wp), INTENT(IN) ::  z(n),zr2(n)
  REAL(wp), INTENT(OUT) :: u
  INTEGER(c_int), INTENT(OUT) :: err_report
 END SUBROUTINE
      
 SUBROUTINE SplineEval(KP,x,y,y2,n,u,f,fp,fpp,fppp)
  USE set_precision, ONLY : wp
  INTEGER, INTENT(IN) :: KP ! periodic vs natural spline flag
  INTEGER, INTENT(IN) :: n ! vector input length
  REAL(wp),INTENT(IN) :: u ! abscissa at which the spline is to be evaluated
  REAL(wp),INTENT(IN) :: x(n) ! abscissas of knots
  REAL(wp),INTENT(IN) :: y(n) ! ordinates of knots
  REAL(wp),INTENT(IN) :: y2(n) ! second deriv at knots
  REAL(wp),INTENT(OUT),OPTIONAL :: f,fp,fpp,fppp ! function, 1st,2nd,3rd deriv
 END SUBROUTINE  

SUBROUTINE SplineEvalCenter(ii,x,y,y2,n,u,f,fp,fpp,fppp)
 USE set_precision, ONLY : wp
 INTEGER, INTENT(IN) :: ii ! meridian
 INTEGER, INTENT(IN) :: n ! vector input length
 REAL(wp),INTENT(IN) :: u ! abscissa at which the spline is to be evaluated
 REAL(wp),INTENT(IN) :: x(n) ! abscissas of knots
 REAL(wp),INTENT(IN) :: y(n) ! ordinates of knots
 REAL(wp),INTENT(IN) :: y2(n) ! second deriv at knots
 REAL(wp),INTENT(OUT),OPTIONAL :: f,fp,fpp,fppp ! function, 1st,2nd,3rd deriv
END SUBROUTINE

 SUBROUTINE SplineEval1Dx1D(iflag,u,v,f,fr,frr,ft,frt,ftt)
  USE set_precision, ONLY : wp
  INTEGER, INTENT(IN) :: iflag     ! iflag=0 no integration
  REAL(wp), INTENT(IN) :: u, v
  REAL(wp), INTENT(OUT),OPTIONAL ::  f,fr,ft,frt,frr,ftt
 END SUBROUTINE
               
 SUBROUTINE thomas(a,b,c,d,z,n,k) ! "Llewellyn Thomas" algorithm for tridiagonal banded matrices"
  USE set_precision, ONLY  : wp
  INTEGER, INTENT(IN) :: n,k
  REAL(wp), INTENT(INOUT) :: a(n),b(n),c(n),d(n,k)
  REAL (wp), INTENT(OUT) :: z(n,k)
 END SUBROUTINE

 SUBROUTINE trapez(ii,iflag,rv,zv,z2v,n,r,z) !trapezoidal rule for spline integration
  USE set_precision, ONLY  : wp
  REAL(wp), INTENT(IN) ::  rv(*)
  REAL(wp), INTENT(IN) ::  zv(*),z2v(*),r
  INTEGER, INTENT(IN) :: n,iflag,ii
  REAL(wp), INTENT(OUT) :: z
 END SUBROUTINE
     
 END INTERFACE
    
 contains
    
END MODULE


