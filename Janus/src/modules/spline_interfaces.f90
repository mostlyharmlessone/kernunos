module spline_interfaces
 ! alphabetical order
 ! gathering all the interfaces of spline subprograms not in a module
 INTERFACE
      
 subroutine bsearch(r,rv,n,high,low)
  use set_precision, only : wp
  integer, intent(in) :: n
  real(wp), intent(in) :: r, rv(n)
  integer, intent(out) :: high, low
 end subroutine
      
 subroutine CubicSplineQuad(rv,zv,z2v,n,r,z) !Forsythe p.90 cubic spline integration
  use set_precision, only : wp
  integer, INTENT(IN) :: n
  real(wp), INTENT(IN) ::  rv(n),zv(n),z2v(n),r
  real(wp), INTENT(OUT) :: z
 end subroutine 

 subroutine nspline(r,z,n,z2)
  USE set_precision, ONLY : wp
  integer, INTENT(IN) :: n
  real(wp), INTENT(IN) ::  r(n),z(n)
  real(wp), INTENT(OUT) :: z2(n)
 end subroutine

 subroutine nsplineCenter(r,z,n,z2)
 use set_precision, only : wp
  integer, INTENT(IN) :: n
  real(wp), INTENT(IN) ::  r(n),z(n)
  real(wp), INTENT(OUT) :: z2(n)
 end subroutine
 
 subroutine SplineCenter(r,z,zr2,n,u)
 use set_precision, only : wp
  integer, INTENT(IN) :: n
  real(wp), INTENT(IN) ::  r(n),z(n),zr2(n)
  real(wp), INTENT(OUT) :: u
 end subroutine

 subroutine pspli(t,z,n,zt2)
  use set_precision, only :  wp
   REAL(wp), intent(in) :: t(*),z(*)
   INTEGER, intent(in) :: n
   REAL(wp), intent(out) ::zt2(n)
 end subroutine
      
 subroutine SplineEval(KP,x,y,y2,n,u,f,fp,fpp,fppp)
  USE set_precision, ONLY : wp
  USE special_fct, ONLY : OPERATOR(.p.) !tensor summation convention
  use,intrinsic :: ieee_arithmetic
  INTEGER, INTENT(IN) :: KP ! periodic vs natural spline flag
  INTEGER, INTENT(IN) :: n ! vector input length
  REAL(wp),INTENT(IN) :: u ! abscissa at which the spline is to be evaluated
  REAL(wp),INTENT(IN) :: x(n) ! abscissas of knots
  REAL(wp),INTENT(IN) :: y(n) ! ordinates of knots
  REAL(wp),INTENT(IN) :: y2(n) ! second deriv at knots
  REAL(wp),INTENT(OUT),OPTIONAL :: f,fp,fpp,fppp ! function, 1st,2nd,3rd deriv
 end subroutine  

 subroutine SplineEval1Dx1D(iflag,u,v,f,fr,ft,frt,frr,ftt) 
  use set_precision, ONLY : wp
  use,intrinsic :: ieee_arithmetic
  integer, INTENT(IN) :: iflag     ! iflag=0 no integration
  real(wp), INTENT(IN) :: u, v
  real(wp), INTENT(OUT),OPTIONAL ::  f,fr,ft,frt,frr,ftt
 end subroutine
               
 subroutine thomas(a,b,c,d,z,n) ! "Llewellyn Thomas" algorithm for tridiagonal banded matrices"
  use set_precision, only : wp
  integer, INTENT(IN) :: n
  real(wp), INTENT(INOUT) :: a(n),b(n),c(n),d(n)
  real (wp), INTENT(OUT) :: z(n)
 end subroutine

 subroutine trapez(rv,zv,z2v,n,r,z) !trapezoidal rule for spline integration
  use set_precision, only : wp
  real(wp), INTENT(IN) ::  rv(*),zv(*),z2v(*),r
  integer, INTENT(IN) :: n
  real(wp), INTENT(OUT) :: z
 end subroutine
     
 END INTERFACE
    
 contains
    
end module


