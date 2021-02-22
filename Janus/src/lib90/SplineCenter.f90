 subroutine SplineCenter(r,z,zr2,n,u)
 use set_precision, only : wp
 use cornea_arrays, only : eps
 USE spline_interfaces, ONLY : bsearch, SplineEval
  real(wp), INTENT(IN) ::  r(n),z(n),zr2(n)
  integer, INTENT(IN) :: n
  real(wp), INTENT(OUT) :: u
  real(wp) :: g,gr,grr
  integer :: high, low, j
  
! find center
  call bsearch(0.0_wp,r,n,high,low)
 
! Newton's method is quick and clean 
  If (z(high)*z(low) <= 0) then   ! find a single real root of the slope
  j=0
  g=(z(high)-z(low))/2.0_wp
  u=(r(high)+r(low))/2.0_wp
  gr=2*g/(r(high)-r(low))
  do while ((j < 100) .AND. (ABS(g/gr) > eps)) 
   j=j+1
   call SplineEval(0,r,z,zr2,n,u,g,gr)  
   u=u-g/gr
  end do
! should take under 10 iterations  
  if (j > 99) then
   write(*,*) 'Probable error on iterations in SplineCenter finding root',u,g,gr
   stop
  endif 
  else                            ! no root, just make it in the center; no guarantee of a local minmax
   u=(r(high)+r(low))/2.0_wp
  endif
   
  end subroutine SplineCenter
