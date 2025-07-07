! binary search for r in rv
subroutine bsearch(r,rv,n,high,low)
 use set_precision, only : wp
 implicit none
integer, intent(in) :: n
real(wp), intent(in) :: r, rv(n)
integer, intent(out) :: high, low
integer m
real eps
 eps = 0.00001
 low=1
 high=n
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
 if ( rv(n) > rv(1) ) then  ! forward ordered vector, lowest to highest
  if (r > rv(n)) then
   low=n-1
  else
   if (r < rv(1)) then
    high=2
   else
    do while ((high-low) > 1) 
     m=floor((low + high)/ 2.)
     if (rv(m) > r) then
      high=m
     else
      low=m
     endif
    end do
   endif 
  endif  
 else    ! reverse ordered vector, highest to lowest
  if (r < rv(n)) then
   high=n-1
  else
   if (r > rv(1)) then
    low=2
   else
    do while ((high-low) > 1) 
     m=floor((low + high)/ 2.)
     if (rv(m) > r) then
      low=m
     else
      high=m
     endif
    end do
   endif 
  endif
 endif
! sanity check
 if (high .le. 0 .or. low .le. 0) then
  write(*,*) 'low error in bsearch',low,high,n,m
  write(*,*) r
  write(*,*) rv
  stop
 endif
 if (high .gt. n .or. low .gt. n) then
  write(*,*) 'high error in bsearch',low,high,n,m
  write(*,*) r
  write(*,*) rv
  stop
 endif
! check if r .eq. a knot, then not in an interval
 if (abs(r-rv(high)) .le. eps) then
  low=high
  endif
 if (abs(r-rv(low)) .le. eps) then
  high=low
 endif
return
end subroutine bsearch        

