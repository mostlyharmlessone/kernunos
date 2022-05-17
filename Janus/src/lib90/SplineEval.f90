subroutine SplineEval(KP,x,y,y2,n,u,f,fp,fpp,fppp)
 USE set_precision, ONLY : wp
 USE cornea_arrays, ONLY : PI
 USE special_fct, ONLY : OPERATOR(.p.) !tensor summation convention
 USE spline_interfaces, ONLY : bsearch
 use,intrinsic :: ieee_arithmetic
 IMPLICIT NONE

! !1-D version
! shamelessly adapted from Computer Methods for Mathematical Computations Forsythe et al. 1977
! http://www.pdas.com/fmmdownload.html : SUBROUTINE Seval3Single(u,x,y,b,c,d,f,fp,fpp,fppp)
! ---------------------------------------------------------------------------
!  PURPOSE - Evaluate the cubic spline function and its derivatives
!     S(u)=w*y(i+1)+wbar*y(i) + dr*dr*( (w**3-w)*y2(i+1)+(wbar**3-wbar)*y2(i) )/6 p.71
!     Press et al. notation, A=w,B=wbar,C=dr*dr*(w**3-w)/6,D=dr*dr*(wbar**3-wbar)/6 p.95
!           where  x(i) <= u < x(i+1)

!  for extrapolation:  if u<x(1), i=1 is used;if u>x(n), i=n is used 

  INTEGER, INTENT(IN) :: KP ! periodic vs natural spline flag
  INTEGER, INTENT(IN) :: n ! vector input length
  REAL(wp),INTENT(IN) :: u ! abscissa at which the spline is to be evaluated
  REAL(wp),INTENT(IN) :: x(n) ! abscissas of knots
  REAL(wp),INTENT(IN) :: y(n) ! ordinates of knots
  REAL(wp),INTENT(IN) :: y2(n) ! second deriv at knots
  REAL(wp),INTENT(OUT),OPTIONAL :: f,fp,fpp,fppp ! function, 1st,2nd,3rd deriv

  INTEGER :: i,i1 ! i1=i+1 in general?
  REAL(wp) :: dr,PERD,A,B,C,D,dA,dB,dC,dD
  REAL(wp), DIMENSION(2) :: AB,CD,dAB,dCD,z,z2
  logical :: IsInf
    
  PERD=2*PI ! period of natural spline if applicable

   call bsearch(u,x,n,i1,i) ! binary search

  dr=x(i1)-x(i)
 
  A=(x(i1)-u)/dr ; dA=-1/dr
  B=(u-x(i))/dr ; dB=1/dr
  
! FOR PERIODIC SPLINES PERIOD 2*PI  
   if ((A*B) < 0) then  !redefine A,B,i,i1
    if (KP == 1) then    
!    Interpolation across gap with KP=1 , periodic spline 
     i=N
     i1=1  
     dr=x(i1)-x(i)+PERD	 
     if (A < 0) then
       B=(u-x(i))/dr   
       A=(x(i1)-u+PERD)/dr
     else
!      B < 0
       B=(u-x(i)+PERD)/dr
       A=(x(i1)-u)/dr
     endif
    end if 
   endif  
   
  C=dr*dr*(A**3-A)/6; dC=dr*dr*dA*(3*A**2-1)/6
  D=dr*dr*(B**3-B)/6; dD=dr*dr*dB*(3*B**2-1)/6
  AB(1)=A ; AB(2)=B ; dAB(1)=dA ; dAB(2)=dB
  CD(1)=C ; CD(2)=D ; dCD(1)=dC ; dCD(2)=dD
      
  z(1)=y(i)
  z(2)=y(i1)
  z2(1)=y2(i)
  z2(2)=y2(i1)
   
   if ((A*B) < 0) then 
    if (KP == 0) then  !  natural spline extrapolation z2=0 outside spline 
     z2=0._wp
    end if
   end if
       
   if (Present(f))  f = (AB.p.z) + (CD.p.z2) !f=A*y(i)+B*y(i1)+((A**3-A)*y2(i)+(B**3-B)*y2(i1))*(dr**2)/6.0_wp
!  1st deriv       
   if (Present(fp)) fp = (dAB.p.z) + (dCD.p.z2) !fp=(y(i1)-y(i))/dr-(3*A*A-1)*dr*y2(i)/6.0+(3*B*B-1)*dr*y2(i1)/6.0_wp 
!  2nd deriv N.B ddAB=0 ddCD=AB
   if (Present(fpp)) fpp = (AB.p.z2)  ! fpp=A*y2(i)+B*y2(i1) 
!  3rd deriv 
   if (Present(fppp)) fppp = (dAB.p.z2) ! fppp=(y2(i1)-y2(i))/dr   

   IsInf=ieee_is_finite(f)
   If(.not.IsInf) then
    write(*,*) 'Warning from SplineEval',KP,u,n,i1,i,z,z2
    stop
   endif
                           
  return
end subroutine SplineEval 
