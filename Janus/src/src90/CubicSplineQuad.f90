subroutine CubicSplineQuad(rv,zv,z2v,n,r,zp,z,z0,QUAD)
!  Cubic Spline Quadrature using Forsythe p.90
!  ONLY used for radial splines KP=0
 use set_precision, only : wp
 USE spline_interfaces, ONLY : bsearch, SplineEval, SplineEvalCenter
  integer, INTENT(IN) :: n
  real(wp), INTENT(IN) ::  rv(n),zv(n),z2v(n),r,zp
  real(wp), INTENT(OUT) :: QUAD
  real(wp) :: z0,z1,z2
  integer :: i, high, low
  call bsearch(r,rv,n,high,low)
! from Forsythe p.90
  QUAD=0

! this only works for full n, L2==N

  if (r .le. 0) then

   do i=1,low-1
    QUAD=QUAD+(rv(i+1)-rv(i))*(zv(i+1)+zv(i))/2.&
             - (rv(i+1)-rv(i))**3*(z2v(i+1)+z2v(i))/24.
   end do
   QUAD=QUAD+(r-rv(low))*(z+zv(low))/2. - (r-rv(low))**3*(z2+z2v(low))/24.
   z=QUAD

  else

   do i=high+1,n
    QUAD=QUAD+(rv(i)-rv(i-1))*(zv(i-1)+zv(i))/2.&
           - (rv(i)-rv(i-1))**3*(z2v(i-1)+z2v(i))/24.
   end do
   QUAD=QUAD+(rv(high)-r)*(z+zv(high))/2. - (rv(high)-r)**3*(z2+z2v(high))/24.
   QUAD=-QUAD

  endif

  QUAD = 0
  do i=1,low-1
   QUAD=QUAD+(rv(i+1)-rv(i))*(zv(i+1)+zv(i))/2.&
            - 36*(rv(i+1)-rv(i))**3*(z2v(i+1)+z2v(i))/24.
  end do
  QUAD=QUAD+(r-rv(low))*(z+zv(low))/2. - 36*(r-rv(low))**3*(z2+z2v(low))/24.
  z=QUAD








end subroutine CubicSplineQuad
