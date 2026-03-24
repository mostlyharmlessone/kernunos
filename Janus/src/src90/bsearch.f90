! (usually) binary search for r in rv
! this has gotten quite ugly and there is no doubt some better/quicker solution
subroutine bsearch(r,rv,n,high,low)
 use set_precision, only : wp
 use cornea_arrays, only : eps, PI
 implicit none
 integer, intent(in) :: n
 real(wp), intent(in) :: r, rv(n)
 integer, intent(out) :: high, low
 integer ::  i, m, direction, i0
 real(wp) :: rmin
 low = 1 ; i0 = 1 ; rmin = rv(1)
 high = n
 if ( n < 1 ) then
  write(*,*) 'FATAL Error in bsearch, n < 1',n
  stop
 endif
 ! degenerate case
 if (n .eq. 1) then
  high = 1 ; low = 1
  write(*,*) 'Warning: degenerate bsearch',r,rv
  return
 endif

! reverse sequences are an issue
! modified for cyclic rv where rv(1) may not be the beginning.
! binary search for rolodex with two possible directions
  direction = 0
  do i=1,n-1
   if (rmin > rv(i)) then
    rmin = rv(i)
    i0 = i
   endif
   if (rv(i) < rv(i+1)) then
    direction = direction + 1
   else
    direction = direction -1
   endif
  end do
  if (rmin > rv(n)) then
   rmin = rv(n)
   i0 = n
  endif
! index of lowest
  low = i0
! one of the neighbors is largest, assign high to largest
  if (i0 .eq. 1) then
   if (rv(i0+1) > rv(n)) then
    high = i0+1
   else
    high = n
   endif
  endif
  if (i0 .eq. n) then
   if (rv(1) > rv(n-1)) then
    high = 1
   else
    high = n-1
   endif
  endif
! not regular ordered data, where the minimum is at the beginning or the end, probably periodic
  if (i0 .ne. n .and. i0 .ne. 1) then
   if (rv(i0-1) > rv(i0+1)) then
    high = i0-1
   else
    high = i0+1
   endif
  endif
! low should always be less than high for a binary search
  if (low .gt. high) then
   i0 = low
   low = high
   high = i0
  endif
  m=-9

 if ( direction .gt. 0 ) then  ! forward ordered vector, lowest to highest, clockwise
    do while ((high-low) > 1)
     m=floor((low + high)/ 2.)
     if (rv(m) > r) then
      high=m
     else
      low=m
     endif
    end do
 else    ! reverse ordered vector, highest to lowest
    do while ((high-low) > 1)
     m=floor((low + high)/ 2.)
     if (rv(m) > r) then
      low=m
     else
      high=m
     endif
    end do
 endif

 ! if the binary search wasn't done because of starting points, search whole string
 if (m < 0) then
  if ( direction .gt. 0 ) then
   do m=1,n-1
    if ((r - rv(m) .ge. 0) .and. (r - rv(m+1) .le. 0)) exit
   end do
   if ((r - rv(n) .ge. 0) .and. (r - rv(1) .le. 0)) then
    low = n
    high = 1
   else
    low = m
    high = m+1
   endif
  else               ! reverse direction
   do m=1,n-1
    if ((r - rv(m) .ge. 0) .and. (r - rv(m+1) .le. 0)) exit
   end do
    if ((r - rv(n) .ge. 0) .and. (r - rv(1) .le. 0)) then
    low = 1
    high = n
   else
    low = m
    high = m + 1
   endif
  endif

! periodic case, binary search not done because of starting point, regular search ends up at n+1 -> rv(n) = 2*PI = 0
  if (high .eq. (n+1)) then

   if (abs(2*PI-r) > abs(rv(low))) then
    high = 1
    low = 180
   else
    high = 1
    low = 2
   endif

 !  write(*,*) 'No binary search',low,high,rv(low),r,rv(high),direction .gt. 0
 !  write(*,*) rv
  endif

 endif

! sanity checks
! high and low  are numbers from 1 to n
 if (high .le. 0 .or. low .le. 0 .or. high .gt. n .or. low .gt. n) then
  write(*,*) 'FATAL index error in bsearch',low,high,n,m
  write(*,*) r
  write(*,*) rv
  stop
 endif

! high and low are one apart unless they are at a periodic jump
 if (abs(high-low) .gt. 1 .and. (low+high) .ne. (n+1)) then
  write(*,*) 'FATAL directional error in bsearch',direction,low,high,n,m
  write(*,*) r
  write(*,*) rv
  stop
 endif

! check if r .eq. a knot, then not in an interval, set low = high
 if (abs(r-rv(high)) .le. eps) then
  low=high
  endif
 if (abs(r-rv(low)) .le. eps) then
  high=low
 endif

! if not in the interval and not a knot and not a reverse sequence but still not starting with minimum value
! then almost certainly actually is in interval but one boundary is the periodic one  .or. (r < rv(low)), so don't check for that
 if ( (r > rv(high) .and. abs(2*PI-r) > rv(high) ) .and. (i0 > 1) .and. (direction > 0) .and. (high .ne. low) ) then
  write(*,*) 'FATAL possible index error in bsearch',low,high,rv(low),r,abs(2*PI-r),rv(high),direction>0,m,i0
  write(*,*) rv
  stop
 endif

return
end subroutine bsearch

