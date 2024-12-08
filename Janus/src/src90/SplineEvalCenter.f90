! centerpoint version, radial spline + extrapolation, needs radial meridian ii
subroutine SplineEvalCenter(ii,x,y,y2,n,u,f,fp,fpp,fppp)
 USE set_precision, ONLY : wp
 USE cornea_arrays, ONLY : RadSplineCenter
 USE special_fct, ONLY : bsearch, OPERATOR(.p.) !tensor summation convention
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

  INTEGER, INTENT(IN) :: ii ! which MM/2 meridian 
  INTEGER, INTENT(IN) :: n ! vector input length
  REAL(wp),INTENT(IN) :: u ! abscissa at which the spline is to be evaluated
  REAL(wp),INTENT(IN) :: x(n) ! abscissas of knots
  REAL(wp),INTENT(IN) :: y(n) ! ordinates of knots
  REAL(wp),INTENT(IN) :: y2(n) ! second deriv at knots
  REAL(wp),INTENT(OUT),OPTIONAL :: f,fp,fpp,fppp ! function, 1st,2nd,3rd deriv
  INTEGER :: i,i1,high,low ! i1=i+1 unless periodic across gap
  REAL(wp) :: dr,A,B,C,D,dA,dB,dC,dD
  REAL(wp), DIMENSION(2) :: AB,CD,dAB,dCD,z,z2
  REAL(wp), allocatable :: xx(:),yy(:),yy2(:)

   allocate(xx(n+1),yy(n+1),yy2(n+1))
!  find center
   call bsearch(0.0_wp,x,n,high,low)
   if (low .eq. high ) then ! 0.0 == x(n)
    write(*,*) 'Unexpected error in SplineEvalCenter'
    stop
   endif
   do i=1,low
    xx(i)=x(i)
    yy(i)=y(i)
    yy2(i)=y2(i)
   end do
!  add a centerpoint at origin with zero slope
    xx(low+1)=0_wp
    yy(low+1)=RadSplineCenter(2,ii)
    yy2(low+1)=RadSplineCenter(3,ii)
   do i=high,n  ! high=low+1
    xx(i+1)=x(i)
    yy(i+1)=y(i)
    yy2(i+1)=y2(i)
   end do
    call bsearch(u,xx,n+1,i1,i) ! binary search
    if (i1 .eq. i) then ! if on the knot
     if (i .ne. (n+1)) then  ! last knot for non-cyclic spline
      i1=i+1
     else
      i1=n+1; i=n
     endif
    endif
    dr=xx(i1)-xx(i)
    A=(xx(i1)-u)/dr ; dA=-1/dr
    B=(u-xx(i))/dr ; dB=1/dr
    C=dr*dr*(A**3-A)/6; dC=dr*dr*dA*(3*A**2-1)/6
    D=dr*dr*(B**3-B)/6; dD=dr*dr*dB*(3*B**2-1)/6
    AB(1)=A ; AB(2)=B ; dAB(1)=dA ; dAB(2)=dB
    CD(1)=C ; CD(2)=D ; dCD(1)=dC ; dCD(2)=dD
    z(1)=yy(i)
    z(2)=yy(i1)
    z2(1)=yy2(i)
    z2(2)=yy2(i1)
    if ((A*B) < 0) then !  natural spline extrapolation z2=0 outside spline
      z2=0._wp
    endif
    if (A < 0 .and. B < 0) then
     write (*,*) 'Unexpected input in SplineEvalCenter',xx(i1),u,xx(i)
     return
    endif

       
   if (Present(f)) f = (AB.p.z) + (CD.p.z2) !f=A*y(i)+B*y(i1)+((A**3-A)*y2(i)+(B**3-B)*y2(i1))*(dr**2)/6.0_wp                    
!  1st deriv       
   if (Present(fp)) fp = (dAB.p.z) + (dCD.p.z2) !fp=(y(i1)-y(i))/dr-(3*A*A-1)*dr*y2(i)/6.0+(3*B*B-1)*dr*y2(i1)/6.0_wp 
!  2nd deriv N.B ddAB=0 ddCD=AB
   if (Present(fpp)) fpp = (AB.p.z2)  ! fpp=A*y2(i)+B*y2(i1) 
!  3rd deriv 
   if (Present(fppp)) fppp = (dAB.p.z2) ! fppp=(y2(i1)-y2(i))/dr   
                           
  return
end subroutine SplineEvalCenter



