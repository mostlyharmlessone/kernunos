subroutine trapez(rv,zv,z2v,n,r,z0,z,TRAP)
!  Cubic Spline Quadrature using trapezoidal rule
!  ONLY used for radial splines
   use set_precision, only : wp
   USE spline_interfaces, only : bsearch, SplineEval, SplineEvalCenter
   real(wp), INTENT(IN) ::  rv(*),zv(*),z2v(*),r,z0,z
   integer, INTENT(IN) :: n
   real(wp), INTENT(OUT) :: TRAP
   integer :: i, high, low
   call bsearch(r,rv,n,high,low)
!  TRAPEZOIDAL RULE, UNEVEN STEPS     
   TRAP=0      
   do i=2,low
    TRAP=TRAP+(rv(i)-rv(i-1))*(zv(i-1)+zv(i))/2.
   end do
   TRAP=TRAP+(r-rv(low))*(z+zv(low))/2.

end subroutine trapez       
