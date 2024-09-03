! binary search for r in rv
subroutine bsearch(r,rv,n,high,low)
 use set_precision, only : wp
 use cornea_arrays, only : eps
 implicit none
integer, intent(in) :: n
real(wp), intent(in) :: r, rv(n)
integer, intent(out) :: high, low
integer m
 low=1
 high=n
 if ( n < 1 ) then
  write(*,*) 'FATAL Error in bsearch, n < 1',n
  stop
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
! sanity check if r .eq. r(n) in cyclic
 if (abs(r-rv(n)) .le. eps) then
!  low=n
!  high=1
 endif
return
end subroutine bsearch        

