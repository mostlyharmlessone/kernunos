! finds the center of the spline defined by where there is a max/min 
 subroutine SplineCenter(dat,jj,r,z,zr2,n,u)
 use, INTRINSIC :: iso_c_binding, ONLY : c_int
 use set_precision, only : wp
 use cornea_arrays, only : eps
 use spline_interfaces, ONLY : SplineEval, SplineEvalCenter
  use special_fct, ONLY : bsearch
 use,intrinsic :: ieee_arithmetic 
  integer(c_int), INTENT(IN) :: dat
  integer, INTENT(IN) :: n,jj
  real(wp), INTENT(IN) ::  r(n),z(n),zr2(n)
  real(wp), INTENT(OUT) :: u
  real(wp) :: g,gr,grr,slopeh,slopel
  integer :: high, low, j

! bracket the origin between r values and get their indices
  call bsearch(0.0_wp,r,n,high,low)
  if (low .gt. high) then ! 0.0 should never be r(n)
   write(*,*) 'Unexplained error in SplineCenter'
   stop
  endif
! Newton's method is quick and clean 
  if (z(high)*z(low) <= 0) then   ! the origin is between a positive and negative z-value; if z is the slope then the root is a minmax of the curve
  j=0                             ! otherwise we are picking a point where z is zero instead of a minmax
  g=(z(high)-z(low))/2.0_wp
  u=(r(high)+r(low))/2.0_wp
  gr=2*g/(r(high)-r(low))
  do while ((j < 10) .AND. (ABS(g/gr) > eps)) ! no more than 10 iterations
   j=j+1
! First time through RadSlopeCenter == 0 and dat == 0
  if (btest(dat, 0) ) then ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
   call SplineEvalCenter(jj,r,z,zr2,n,u,g,gr)
  else
   call SplineEval(0,r,z,zr2,n,u,g,gr) 
  endif
   if (ABS(gr) < EPS) then        ! gr is the second derivative, if it gets small as g/gr gets small there is an issue
    u=(r(high)+r(low))/2.0_wp     ! just make it in the center; no guarantee of a local root
    write(*,*) 'no guarantee of a local root in SplineCenter'
    return
   endif
   u=u-g/gr
  end do
! should take under 10 iterations; if not:  
  if (j > 9) then
   u=(r(high)+r(low))/2.0_wp      ! just make it in the center; no guarantee of a local root
   write(*,*) 'Probable error on iterations in SplineCenter finding root',u,g,gr  
  endif
 
  else     ! the origin isn't between a positive and negative value, so if the slope changes sign, there's a minmax

!  First time through RadSlopeCenter == 0
   if (btest(dat, 0) ) then ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
    call SplineEvalCenter(jj,r,z,zr2,n,z(high),g,slopeh)
    call SplineEvalCenter(jj,r,z,zr2,n,z(low),g,slopel)
   else
    call SplineEval(0,r,z,zr2,n,z(high),g,slopeh)
    call SplineEval(0,r,z,zr2,n,z(low),g,slopel)
   endif
  
   if (slopeh*slopel <= 0) then   ! find a minmax, the slope changes sign   
    j=0
    u=(r(high)+r(low))/2.0_wp
    gr=2*g/(r(high)-r(low))
    grr=2*gr/(r(high)-r(low))
    do while ((j < 10) .AND. (ABS(gr/grr) > eps)) 
     j=j+1    

     ! First time through RadSlopeCenter == 0
     if (btest(dat, 0) ) then ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
      call SplineEvalCenter(jj,r,z,zr2,n,u,g,gr,grr)
     else
      call SplineEval(0,r,z,zr2,n,u,g,gr,grr)
     endif

     if (ABS(grr) < EPS) then
      u=(r(high)+r(low))/2.0_wp     ! just make it in the center; no guarantee of a local minmax
      write(*,*) 'no guarantee of a local minmax in SplineCenter'
      return
     endif     
     u=u-gr/grr
    end do  
! should take under 10 iterations  
    if (j > 9) then
     u=(r(high)+r(low))/2.0_wp      ! just make it in the center; no guarantee of a local minmax
     write(*,*) 'Probable error on iterations in SplineCenter finding minmax',u,gr,grr,ABS(gr/grr) 
!     write(*,*) low,high,r(low),z(low),r(high),z(high)
!      write(*,*) slopel,slopeh
!      can plot these with gnuplot, plot 
!      do i=1,n
!      write(*,*) r(i),z(i)
!      end do
!      stop    
    endif 
    
    else   ! there's no change in sign of the slope (or the function)
      u=(r(high)+r(low))/2.0_wp     ! just make it in the center; no guarantee of a local minmax
      write(*,*) 'no guarantee of a local minmax in SplineCenter, no change in slope'
    endif 
                 
  endif
   
  end subroutine SplineCenter
