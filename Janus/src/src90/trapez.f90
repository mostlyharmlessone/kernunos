subroutine trapez(ii,iflag,rv,zv,z2v,n,r,z)
!  Cubic Spline Quadrature using trapezoidal rule
!  ONLY used for radial splines
   use set_precision, only : wp
   use spline_interfaces, only : SplineEval, SplineEvalCenter
   use special_fct, ONLY : bsearch
   real(wp), INTENT(IN) ::  rv(*),zv(*),z2v(*),r
   integer, INTENT(IN) :: n,iflag,ii
   real(wp), INTENT(OUT) :: z
   real(wp) :: TRAP
   integer :: i, high, low
   call bsearch(r,rv,n,high,low)
!  TRAPEZOIDAL RULE, UNEVEN STEPS     
   TRAP=0      
   do i=2,low
    TRAP=TRAP+(rv(i)-rv(i-1))*(zv(i-1)+zv(i))/2.
   end do
   if ((iflag-mod(iflag,10))/10 == 0) then
    call SplineEval(0,rv,zv,z2v,n,r,z)
   endif
   if ((iflag-mod(iflag,10))/10 == 1) then ! using center-node spline
    call SplineEvalCenter(ii,rv,zv,z2v,n,r,z)
   endif
   TRAP=TRAP+(r-rv(low))*(z+zv(low))/2.
   z=TRAP  

end subroutine trapez       
