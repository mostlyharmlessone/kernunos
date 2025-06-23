module spline_interfaces
 ! alphabetical order
 ! gathering all the interfaces of spline subprograms not in a module
 INTERFACE

 subroutine AdjustRadSplineCenter
 end subroutine AdjustRadSplineCenter
      
 subroutine CubicSplineQuad(ii,iflag,rv,zv,z2v,n,r,z) !Forsythe p.90 cubic spline integration
  use set_precision, only : wp
  integer, INTENT(IN) :: n,iflag,ii
  real(wp), INTENT(IN) ::  rv(n),zv(n),z2v(n),r
  real(wp), INTENT(OUT) :: z
 end subroutine 

 subroutine LSQEval(M2,c,v,f,ft,ftt,fttt)
  USE set_precision, ONLY : wp
  INTEGER, intent(in) :: M2
  REAL(wp), intent(in) :: v,c(M2)
  REAL(wp),INTENT(OUT),OPTIONAL :: f,ft,ftt,fttt
 end subroutine

 subroutine lsqfit(t,z,M1,M2,c)
  use set_precision, only :  wp
  use LapackInterface, ONLY : dgetrf, dgetrs !, dgels, GaussJordan
  REAL(wp), intent(in) :: t(M1),z(M1)
  INTEGER, intent(in) :: M1,M2
  REAL(wp), intent(out) ::c(M2)
 end subroutine

 subroutine MakeRadSplineCenter(dat)
  use, INTRINSIC :: iso_c_binding, ONLY : c_int
  use set_precision, ONLY : wp
  integer(c_int), INTENT(IN) :: dat
 end subroutine

 subroutine nspline(r,z,n,z2,err_report)
  USE set_precision, ONLY : wp
  integer, INTENT(IN) :: n
  real(wp), INTENT(IN) ::  r(n),z(n)
  real(wp), INTENT(OUT) :: z2(n)
  integer, INTENT(OUT) :: err_report
 end subroutine

 subroutine nsplineCenter(ii,r,z,n,z2,err_report)
  use set_precision, only : wp
  integer, INTENT(IN) :: ii,n
  real(wp), INTENT(IN) ::  r(n)
  real(wp), INTENT(IN) ::  z(n)
  real(wp), INTENT(OUT) :: z2(n)
  integer, INTENT(OUT) :: err_report
 end subroutine

subroutine pspli(t,z,n,zt2,err_report)
 use set_precision, only :  wp
 REAL(wp), intent(in) :: t(n),z(n)
 INTEGER, intent(in) :: n
 REAL(wp), intent(out) ::zt2(n)
 integer, INTENT(OUT) :: err_report
 REAL(wp) :: PERD,error
 REAL(wp) :: d(n),a(n),b(n),c(n)
end subroutine
 
 subroutine SplineCenter(dat,jj,r,z,zr2,n,u)
 use set_precision, only : wp
 use, INTRINSIC :: iso_c_binding, ONLY : c_int
  integer(c_int), INTENT(IN) :: dat
  integer, INTENT(IN) :: n,jj
  real(wp), INTENT(IN) ::  r(n),z(n),zr2(n)
  real(wp), INTENT(OUT) :: u
 end subroutine
      
 subroutine SplineEval(KP,x,y,y2,n,u,f,fp,fpp,fppp)
  USE set_precision, ONLY : wp
  INTEGER, INTENT(IN) :: KP ! periodic vs natural spline flag
  INTEGER, INTENT(IN) :: n ! vector input length
  REAL(wp),INTENT(IN) :: u ! abscissa at which the spline is to be evaluated
  REAL(wp),INTENT(IN) :: x(n) ! abscissas of knots
  REAL(wp),INTENT(IN) :: y(n) ! ordinates of knots
  REAL(wp),INTENT(IN) :: y2(n) ! second deriv at knots
  REAL(wp),INTENT(OUT),OPTIONAL :: f,fp,fpp,fppp ! function, 1st,2nd,3rd deriv
 end subroutine  

subroutine SplineEvalCenter(ii,x,y,y2,n,u,f,fp,fpp,fppp)
 USE set_precision, ONLY : wp
 INTEGER, INTENT(IN) :: ii ! meridian
 INTEGER, INTENT(IN) :: n ! vector input length
 REAL(wp),INTENT(IN) :: u ! abscissa at which the spline is to be evaluated
 REAL(wp),INTENT(IN) :: x(n) ! abscissas of knots
 REAL(wp),INTENT(IN) :: y(n) ! ordinates of knots
 REAL(wp),INTENT(IN) :: y2(n) ! second deriv at knots
 REAL(wp),INTENT(OUT),OPTIONAL :: f,fp,fpp,fppp ! function, 1st,2nd,3rd deriv
end subroutine

 subroutine SplineEval1Dx1D(iflag,u,v,f,fr,frr,ft,frt,ftt)
  use set_precision, ONLY : wp
  integer, INTENT(IN) :: iflag     ! iflag=0 no integration
  real(wp), INTENT(IN) :: u, v
  real(wp), INTENT(OUT),OPTIONAL ::  f,fr,ft,frt,frr,ftt
 end subroutine
               
 subroutine thomas(a,b,c,d,z,n,k) ! "Llewellyn Thomas" algorithm for tridiagonal banded matrices"
  use set_precision, only : wp
  integer, INTENT(IN) :: n,k
  real(wp), INTENT(INOUT) :: a(n),b(n),c(n),d(n,k)
  real (wp), INTENT(OUT) :: z(n,k)
 end subroutine

 subroutine trapez(ii,iflag,rv,zv,z2v,n,r,z) !trapezoidal rule for spline integration
  use set_precision, only : wp
  real(wp), INTENT(IN) ::  rv(*),zv(*),z2v(*),r
  integer, INTENT(IN) :: n,iflag,ii
  real(wp), INTENT(OUT) :: z
 end subroutine
     
 END INTERFACE
    
 contains
    
end module


