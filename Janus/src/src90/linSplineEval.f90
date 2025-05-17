! here y2 is the output of linspline
subroutine linSplineEval(KP,r,y2,n,u,f,fp)
 USE set_precision, ONLY : wp
 USE cornea_arrays, ONLY : PI
 USE special_fct, ONLY : bsearch, OPERATOR(.p.) !tensor summation convention
 use,intrinsic :: ieee_arithmetic
 IMPLICIT NONE

  INTEGER, INTENT(IN) :: KP ! periodic KP=1 vs natural spline flag KP=0
  INTEGER, INTENT(IN) :: n ! vector input length
  REAL(wp),INTENT(IN) :: u ! abscissa at which the spline is to be evaluated
  REAL(wp),INTENT(IN) :: r(n) ! ordinates 
  REAL(wp),INTENT(IN) :: y2(n) ! output of linSpline
  REAL(wp),INTENT(OUT),OPTIONAL :: f,fp ! function, 1st deriv
  INTEGER :: i,i1 ! i1=i+1 unless periodic across gap
  REAL(wp) :: dr,PERD,A,B,dA,dB,x(n)
  REAL(wp), DIMENSION(2) :: AB,dAB,z
  logical :: IsInf
  
  PERD =2*PI
  if (n .eq. 1) then  ! degenerate case
   write(*,*) 'Warning: degenerate SplineEval'
   if (Present(f)) f=y2(n)
   if (Present(fp)) fp=0
   return
  endif
  dr=(r(n)-r(1))/(n-1) ! evenly spaced knots over r()
  do i=1,n
   x(i)=r(1)+(i-1)*dr  ! evenly spaced knots
  end do
  call bsearch(u,x,n,i1,i) ! binary search
  if (i1 .eq. i) then
   if (i .ne. n) then  ! on a knot
    i1=i+1
    B=0 ; A=1
   else   
    if (KP .ne. 1) then
     i1=n ; i=n-1 ! should be usual default with floor
     B=1 ; A=0    ! terminal knot natural spline
    else
     i1=1 ; i=n  !catches the terminal knot in the forward interval, KP = 1
     B=0 ; A=1
    endif    
   endif   
  else                   ! normal sequence
   A=(x(i1)-u)/dr
   B=(u-x(i))/dr
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
  endif
  dA=-1/dr ; ; dB=1/dr
  AB(1)=A ; AB(2)=B ; dAB(1)=dA ; dAB(2)=dB
  z(1)=y2(i)
  z(2)=y2(i1)

  if ((A*B) < 0) then
   if (KP /= 1) then  !  natural spline extrapolation z2=0 outside spline
    if (KP ==2) then
     if (Present(f)) f=0
     if (Present(fp)) fp=0
     return
    endif
   end if
  end if
  if (A < 0 .and. B < 0) then
   if (KP /= 1) then
    write (*,*) 'Unexpected input in SplineEval',x(i1),u,x(i)
    return
   end if
  end if

   if (Present(f)) f = (AB.p.z)  !f=A*y(i)+B*y(i1)                    
!  1st deriv       
   if (Present(fp)) fp = (dAB.p.z)  !fp=(y(i1)-y(i))/dr

   if (Present(f)) IsInf=ieee_is_finite(f)
   if (Present(fp)) IsInf=ieee_is_finite(f) .and. ieee_is_finite(fp)
   if(.not.IsInf) then
    write(*,*) 'Error in SplineEval',KP,u,n,i1,i,z,dr
    write(*,*) AB,dAB
    return
   endif
                           
  return
end subroutine linSplineEval