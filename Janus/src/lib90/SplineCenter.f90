 subroutine SplineCenter(r,z,zr2,n,u)
 use set_precision, only : wp
 use cornea_arrays, only : eps
 use spline_interfaces, ONLY : bsearch, SplineEval
 use,intrinsic :: ieee_arithmetic 
  real(wp), INTENT(IN) ::  r(n),z(n),zr2(n)
  integer, INTENT(IN) :: n
  real(wp), INTENT(OUT) :: u
  real(wp) :: g,gr,grr,slopeh,slopel
  integer :: high, low, j
  logical :: IsInf
  
! find center
  call bsearch(0.0_wp,r,n,high,low)
 
! Newton's method is quick and clean 
  If (z(high)*z(low) <= 0) then   ! find a single real root of the slope
  j=0
  g=(z(high)-z(low))/2.0_wp
  u=(r(high)+r(low))/2.0_wp
  gr=2*g/(r(high)-r(low))
  do while ((j < 10) .AND. (ABS(g/gr) > eps)) 
   j=j+1   
   call SplineEval(0,r,z,zr2,n,u,g,gr) 
   if (gr ==0) then
    u=(r(high)+r(low))/2.0_wp     ! just make it in the center; no guarantee of a local root
    exit
   endif
   u=u-g/gr
  end do
! should take under 10 iterations  
  if (j > 9) then
   u=(r(high)+r(low))/2.0_wp      ! just make it in the center; no guarantee of a local root
   write(*,*) 'Probable error on iterations in SplineCenter finding root',u,g,gr  
  endif 
  else 
  
   call SplineEval(0,r,z,zr2,n,z(high),g,slopeh)
   call SplineEval(0,r,z,zr2,n,z(low),g,slopel)
   If (slopeh*slopel <= 0) then   ! find a minmax
   
    j=0
    u=(r(high)+r(low))/2.0_wp
    gr=2*g/(r(high)-r(low))
    grr=2*gr/(r(high)-r(low))
    do while ((j < 10) .AND. (ABS(gr/grr) > eps)) 
     j=j+1   
     call SplineEval(0,r,z,zr2,n,u,g,gr,grr) 
     if (grr ==0) then
      u=(r(high)+r(low))/2.0_wp     ! just make it in the center; no guarantee of a local minmax
      exit
     endif     
     u=u-gr/grr
    end do  
! should take under 10 iterations  
    if (j > 9) then
     u=(r(high)+r(low))/2.0_wp      ! just make it in the center; no guarantee of a local minmax
     write(*,*) 'Probable error on iterations in SplineCenter finding minmax',u,gr,grr,ABS(gr/grr)      
    endif 
    
    else
      u=(r(high)+r(low))/2.0_wp     ! just make it in the center; no guarantee of a local minmax
    endif 
                 
  endif
   
  end subroutine SplineCenter
