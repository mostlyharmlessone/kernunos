! PURPOSE adds a central node to the radial spline at the origin between central points
! only use after nspline and MakeRadSplineCenter have run
 subroutine nsplineCenter(ii,r,z,n,z2)
 use set_precision, only : wp
 use cornea_arrays, only : RadSplineCenter
 use spline_interfaces, ONLY : thomas, SplineEval
  use special_fct, ONLY : bsearch
 use LapackInterface, ONLY : dgtsv
 use,intrinsic :: ieee_arithmetic
  integer, INTENT(IN) :: ii,n
  real(wp), INTENT(IN) ::  r(n)
  real(wp), INTENT(IN) ::  z(n)
  real(wp), INTENT(OUT) :: z2(n)
  real(wp),allocatable ::  a(:),b(:),c(:),d(:),zz2(:),a_short(:)
  real(wp),allocatable ::  rr(:),zz(:)
  integer :: high, low, i
  integer :: info    ! for lapack use below 

  INFO=0             
  if ( n < 2 ) then  ! invalid parameter
   INFO=-1
   return
  endif 
 
  if ( n == 2 ) then 
   return  ! degenerate case  
  else 
    allocate (a(n-1),b(n-1),c(n-1),d(n-1),zz2(n-1))
    allocate (rr(n+1),zz(n+1))
  endif

! boundary conditions for natural spline, not used in calculations
  z2(1)=0.
  z2(n)=0.
  a=0  ;  b=0  ;   c=0  ;  d=0

! find center
  call bsearch(0.0_wp,r,n,high,low)
  do i=1,low
   rr(i)=r(i)
   zz(i)=z(i)
  end do
! add a centerpoint at origin with zero slope
   rr(low+1)=0_wp
   call SplineEval(0,r,z,z2,n,RadSplineCenter(1,ii),zz(low+1))
   RadSplineCenter(2,ii)=zz(low+1)

  do i=high,n  ! high=low+1
   rr(i+1)=r(i)
   zz(i+1)=z(i)
  end do

  INFO=0 ; a=0  ;  b=0  ;   c=0  ;  d=0 


! spline equation at internal knots
    do i=2,n
     a(i-1)=(rr(i)-rr(i-1))/6.0
     b(i-1)=(rr(i+1)-rr(i-1))/3.0
     c(i-1)=(rr(i+1)-rr(i))/6.0
     d(i-1)=(zz(i+1)-zz(i))/(rr(i+1)-rr(i))-(zz(i)-zz(i-1))/(rr(i)-rr(i-1))
    end do

!  lapack
    if (n > 3) then                                   ! n > 3 only if using LAPACK dgtsv
    allocate (a_short(n-2))
     do i=1,n-2
      a_short(i)=a(i+1)                               ! truncated "a" for dgtsv, don't have to truncate "c"
     end do
    endif
!  call thomas(a,b,c,d,zz2,n-1,1) ! can use to check against lapack
  if (n > 3) then
   zz2(:)=d(:) ! for lapack
   call dgtsv( n-1, 1, a_short, b, c, zz2, n-1, INFO )     ! overwrites b and d into solution
   deallocate(a_short)
  else  ! have to use thomas for n=3
   call thomas(a,b,c,d,zz2,n-1,1) ! can use to check against lapack, doesn't use a(1) or c(n); overwrites b and d
  endif

! remove centerpoint zz2(low+1), adjust for zz2 being 1...n-1 corresponding with z2(2...n)
   do i=2,high  
     z2(i)=zz2(i-1)
   end do
! skip zz2(high) high=low+1
   do i=high+1,n-1
     z2(i)=zz2(i)  
   end do
 ! store z2 central/origin
   RadSplineCenter(3,ii)=zz2(high)

 end subroutine nsplineCenter

