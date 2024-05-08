 subroutine nsplineCenter(r,z,n,N1,z2)
 use set_precision, only : wp
 use cornea_arrays, only : eps
 use spline_interfaces, ONLY : bsearch, thomas
 use,intrinsic :: ieee_arithmetic
 use LapackInterface, ONLY : dgtsv
  integer, INTENT(IN) :: n,N1
  real(wp), INTENT(IN) ::  r(n)
  real(wp), INTENT(INOUT) ::  z(N1)
  real(wp), INTENT(OUT) :: z2(N1)
  real(wp) ::  zz2(n+1),rr(n+1),zz(n+1),zzhigh
  real(wp) ::  a(n-1),b(n-1),c(n-1),d(n-1),a_short(n-2)
  integer :: info, high, low, i
  real(wp) :: f      ! error handling
  logical :: IsNaN

! PURPOSE adds a central node to the radial spline at the origin between central points

! find center
  call bsearch(0.0_wp,r,n,high,low)

  do i=1,low
   rr(i)=r(i)
   zz(i)=z(i)
  end do
! add a centerpoint for slopes calc zero slope at origin (could use another unique value)
   rr(low+1)=0_wp

  do i=high,n  ! high=low+1
   rr(i+1)=r(i)
   zz(i+1)=z(i)
  end do

  INFO=0 ; a=0  ;  b=0  ;   c=0  ;  d=0 
! boundary conditions for natural spline   
  z2(1)=0.      
  z2(n)=0.
  zz2(1)=0.
  zz2(n+1)=0.
! spline equation at internal knots
!  first half
   do i=2,low
     a(i-1)=(rr(i)-rr(i-1))/6.0
     b(i-1)=(rr(i+1)-rr(i-1))/3.0
     c(i-1)=(rr(i+1)-rr(i))/6.0
     d(i-1)=(zz(i+1)-zz(i))/(rr(i+1)-rr(i))-(zz(i)-zz(i-1))/(rr(i)-rr(i-1))
   end do

! different for zero slope equation in middle at low+1=high
! continuity is still true but while rr(low)=r(low), rr(low+1)=rr(high)=0, rr(high+1)=r(high)
! zz(i)=zz(low+1) is unknown zz(low+2)=z(high), known as is zz(low)
!   this doesn't work when rr is evenly spaced.
    i=low+1

!    a(i-1)=-((rr(i)-rr(i-1))**2)/6.0
 !   b(i-1)= ((rr(i+1)-rr(i))**2-(rr(i)-rr(i-1))**2)/3.0
!    c(i-1)= ((rr(i+1)-rr(i))**2)/6.0
 !   d(i-1)= zz(i+1)-zz(i-1)

! here zz(low+!) is unknown; add as a fourth unknown?
a(i-1)=0
b(i-1)=(rr(i+1)-rr(i))/3.0
c(i-1)=(rr(i+1)-rr(i))/6.0
d(i-1)=(zz(i+1)-zz(i))/(rr(i+1)-rr(i))

!  second half
   do i=high+1,n
   a(i-1)=(rr(i)-rr(i-1))/6.0
   b(i-1)=(rr(i+1)-rr(i-1))/3.0
   c(i-1)=(rr(i+1)-rr(i))/6.0
   d(i-1)=(zz(i+1)-zz(i))/(rr(i+1)-rr(i))-(zz(i)-zz(i-1))/(rr(i)-rr(i-1))
   end do

write(*,*) 'a',a(low)
write(*,*) 'b',b(low)
write(*,*) 'c',c(low)
write(*,*) 'd',d(low)
!stop

!!  lapack
!   do i=1,n-2
!    a_short(i)=a(i+1) ! truncated "a" for dgtsv, don't have to truncate "c"
!   end do
!  call dgtsv( n-1, 1, a_short, b, c, d, n-1, INFO )     ! overwrites b and d into solution
!  do i=2,n-1
!    zz2(i)=d(i-1)
!  end do
  call thomas(a,b,c,d,zz2,n-1,1) ! can use to check against lapack
  
! remove centerpoint zz2(low+1)
   do i=2,low   !zz2(1)=0 natural spline
     z2(i)=zz2(i)
   end do
!  calculate the central point
   i=low+1
!  These should be equal
   zz(i)=zz(i-1)+zz2(i-1)*((rr(i)-rr(i-1))**2)/6.0 + zz2(i)*((rr(i)-rr(i-1))*2)/3.0
   zzhigh=zz(i+1)+zz2(i+1)*((rr(i+1)-rr(i))**2)/6.0 + zz2(i)*((rr(i+1)-rr(i))*2)/3.0
   if (ABS(zz(i)-zzhigh) .gt. EPS) then
    write(*,*) 'Error in nsplineCenter',zz(i),zzhigh
   endif
! skip zz2(low) high=low+1
   do i=high,n-1
     z2(i)=zz2(i+1)  !zz2(n+1)=0 natural spline
   end do
 ! store z and z2 central/origin at 2*N*M+1
   z2(N1)=zz2(low+1)
   z(N1)=zz(low+1)

f = dot_product(z2,z2)
IsNaN=ieee_is_NaN(f)
if (IsNaN) then
 write(*,*) 'Warning from nsplineCenter ',r(1:n)
 stop
endif

 end subroutine nsplineCenter

