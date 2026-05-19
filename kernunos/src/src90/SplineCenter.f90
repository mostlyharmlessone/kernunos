! finds the center of the spline defined by where there is a max/min 
 subroutine SplineCenter(dat,jj,r,z,zr2,n,u,err_report)
 use, INTRINSIC :: iso_c_binding, ONLY : c_int, c_int64_t
 use set_precision, only : wp
 use parameters
 use spline_interfaces, ONLY : SplineEval, SplineEvalCenter
  use special_fct, ONLY : bsearch
 use,intrinsic :: ieee_arithmetic 
 implicit none
  integer(c_int64_t), INTENT(IN) :: dat
  integer, INTENT(IN) :: n,jj
  real(wp), INTENT(IN) ::  r(n)
  real(wp), INTENT(IN) ::  z(n),zr2(n)
  real(wp), INTENT(OUT) :: u
  integer(c_int), INTENT(OUT) :: err_report
  real(wp) :: g,gr,grr,slopeh,slopel,quadA,quadB,quadC,descriminant,slope0
  integer :: high, low, j
  err_report = 0
! bracket the origin between r values and get their indices
  call bsearch(0.0_wp,r,n,high,low)
  if (low .eq. high) then ! 0.0 should never be a value or knot of r
   write(*,*) 'Unusual error in SplineCenter, setting u=r(low)',r(low),high,low
   u=r(low)
   err_report = 1
   return
  endif
! Newton's method is quick and clean for roots
  if (z(high)*z(low) <= 0) then   ! the origin is between a positive and negative z-value; if z is the slope then the root is a minmax of the curve
  j=0                             ! otherwise we are picking a point where z is zero instead of a minmax
  g=(z(high)-z(low))/2.0_wp
  u=(r(high)+r(low))/2.0_wp
  if (abs(r(high)-r(low)) .lt. eps*eps) then
   write(*,*) 'Unusual error in SplineCenter, setting u=(r(high)+r(low))/2.0_wp'
   u=(r(high)+r(low))/2.0_wp
   err_report = 2
   return
  endif
  gr=2*g/(r(high)-r(low))
  do while ((j < 10) .AND. (ABS(g/gr) > eps)) ! no more than 10 iterations
   j=j+1
! First time through RadSlopeCenter == 0 and dat == 0 which means btest(dat, 0) .eq. false
  if (btest(dat, 0) ) then ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
   call SplineEvalCenter(jj,r,z,zr2,n,u,g,gr)
  else
   call SplineEval(0,r,z,zr2,n,u,g,gr) 
  endif
   if (ABS(gr) < EPS*EPS) then        ! gr is the first derivative, if it gets small as g/gr gets small there might be an overflow
    u=(r(high)+r(low))/2.0_wp     ! just make it in the center; no guarantee of a local root
    write(*,*) 'no guarantee of a local root in SplineCenter, setting u=(r(high)+r(low))/2.0_wp',g,gr,btest(dat, 0)
    err_report = 3
    return
   endif
   u=u-g/gr
  end do
! should take under 10 iterations; if not:
  if (j > 10) then
   u=(r(high)+r(low))/2.0_wp      ! just make it in the center; no guarantee of a local root
   write(*,*) 'Probable error on iterations in SplineCenter finding root',u,g,gr,btest(dat, 0)
   err_report = 4
   return
  endif
  else
!  the origin isn't between a positive and negative value, so if the slope changes sign, there's a minmax

!  First time through RadSplineCenter == 0
   if (btest(dat, 0) ) then ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
    call SplineEvalCenter(jj,r,z,zr2,n,r(high),g,slopeh)
    call SplineEvalCenter(jj,r,z,zr2,n,r(low),g,slopel)
   else
    call SplineEval(0,r,z,zr2,n,r(high),g,slopeh)
    call SplineEval(0,r,z,zr2,n,r(low),g,slopel)
   endif
  
   if (slopeh*slopel <= 0) then   ! find a minmax, the slope changes sign   
!   Quadratic solution for the minimum, not using Newton's method
    quadB=6.0*(r(high)*zr2(low)-r(low)*zr2(high))
    quadC=3.0*(zr2(low)-zr2(high))
    quadA=6.0*(z(low)-z(high))
    quadA=quadA-r(low)*r(low)*zr2(low)
    quadA=quadA+2*r(high)*r(low)*(zr2(low)-zr2(high))
    quadA=quadA+2*r(high)*r(high)*zr2(low)
    quadA=quadA-2*r(low)*r(low)*zr2(high)
    quadA=quadA+r(high)*r(high)*zr2(high)
    descriminant=quadB*quadB-4*quadC*quadA
    u=(quadB-sqrt(descriminant))/(2*quadC)
!   check that u is at slope0 = 0
    call SplineEval(0,r,z,zr2,n,u,g,slope0)
    if (ABS(slope0) > EPS) then
!   Using Newton's method, usually works with 2-3 iterations, very unlikely to occur
     write(*,*) "Using Newton's method to find minimum, this is unusual"
     j=0
     gr=(slopeh+slopel)/2.0_wp
     u=(r(high)+r(low))/2.0_wp
     grr=2*gr/(r(high)-r(low))
     do while ((j < 10) .AND. (ABS(gr) > eps))
      j=j+1
     ! First time through RadSlopeCenter == 0
      if (btest(dat, 0)) then ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
       call SplineEvalCenter(jj,r,z,zr2,n,u,g,gr,grr)
      else
       call SplineEval(0,r,z,zr2,n,u,g,gr,grr)
      endif
      if (ABS(grr) < EPS*EPS) then
       write(*,*) 'no guarantee of a local minmax in SplineCenter',u,g,gr,grr,btest(dat, 0),n,j
       u=(r(high)+r(low))/2.0_wp     ! just make it in the center; no guarantee of a local minmax
       call SplineEvalCenter(jj,r,z,zr2,n,u,g,gr,grr)
       write(*,*) 'Using arithmetic center in SplineCenter',u,g,gr,grr
       err_report = 5
       return
      endif
      u=u-gr/grr
     end do
! should take under 10 iterations
     if (j > 10) then
      u=(r(high)+r(low))/2.0_wp      ! just make it in the center; no guarantee of a local minmax
      write(*,*) 'Probable error on iterations in SplineCenter finding minmax',u,gr,grr,btest(dat, 0)
      err_report = 6
      return
     endif
    endif
    else   ! there's no change in sign of the slope (or the function)
      u=(r(high)+r(low))/2.0_wp     ! just make it in the center; no guarantee of a local minmax
      write(*,*) 'no guarantee of a local minmax in SplineCenter, no change in slope'
      err_report = 7
      return
    endif        
  endif
   
  end subroutine SplineCenter
