subroutine CubicSplineQuad(ii,iflag,rv,zv,z2v,n,r,z)
!  Cubic Spline Quadrature using Forsythe p.90
!  ONLY used for radial splines KP=0
 use set_precision, only : wp
 USE spline_interfaces, ONLY : bsearch, SplineEval, SplineEvalCenter
  integer, INTENT(IN) :: n,iflag,ii
  real(wp), INTENT(IN) ::  rv(n),zv(n),z2v(n),r
  real(wp), INTENT(OUT) :: z
  real(wp) :: QUAD,z1,z2
  integer :: i, high, low
  call bsearch(r,rv,n,high,low)
! from Forsythe p.90
  QUAD=0

  if (r .le. 0) then

   do i=1,low-1
    QUAD=QUAD+(rv(i+1)-rv(i))*(zv(i+1)+zv(i))/2.&
             - (rv(i+1)-rv(i))**3*(z2v(i+1)+z2v(i))/24.
   end do
   if ((iflag-mod(iflag,10))/10 == 0) then
    call SplineEval(0,rv,zv,z2v,n,r,z,z1,z2)
   endif
   if ((iflag-mod(iflag,10))/10 == 1) then ! using center-node spline
    call SplineEvalCenter(ii,rv,zv,z2v,n,r,z,z1,z2)
   endif
   QUAD=QUAD+(r-rv(low))*(z+zv(low))/2. - (r-rv(low))**3*(z2+z2v(low))/24.
   z=QUAD

  else

   do i=high+1,n
    QUAD=QUAD+(rv(i)-rv(i-1))*(zv(i-1)+zv(i))/2.&
           - (rv(i)-rv(i-1))**3*(z2v(i-1)+z2v(i))/24.
   end do
   if ((iflag-mod(iflag,10))/10 == 0) then
    call SplineEval(0,rv,zv,z2v,n,r,z,z1,z2)
   endif
   if ((iflag-mod(iflag,10))/10 == 1) then ! using center-node spline
    call SplineEvalCenter(ii,rv,zv,z2v,n,r,z,z1,z2)
   endif
   QUAD=QUAD+(rv(high)-r)*(z+zv(high))/2. - (rv(high)-r)**3*(z2+z2v(high))/24.
   z=-QUAD

  endif

end subroutine CubicSplineQuad
