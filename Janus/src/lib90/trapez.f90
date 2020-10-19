subroutine trapez(rv,zv,z2v,n,r,z)
!  Cubic Spline Quadrature using trapezoidal rule
!  ONLY used for radial splines
   use set_precision, only : wp
   USE spline_interfaces, only : bsearch, SplineEval
   real(wp), INTENT(IN) ::  rv(*),zv(*),z2v(*),r
   integer, INTENT(IN) :: n
   real(wp), INTENT(OUT) :: z
   real(wp) :: TRAP, z1, z2 
   integer :: i, high, low
    
   call bsearch(r,rv,n,high,low)
!  TRAPEZOIDAL RULE, UNEVEN STEPS     
   TRAP=0      
   do i=2,low
    TRAP=TRAP+(rv(i)-rv(i-1))*(zv(i-1)+zv(i))/2.
   end do   
   CALL SplineEval(0,rv,zv,z2v,n,r,z,z1,z2)
   TRAP=TRAP+(r-rv(low))*(z+zv(low))/2.
   z=TRAP  
       
end subroutine trapez       
