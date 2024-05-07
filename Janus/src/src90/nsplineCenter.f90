 subroutine nsplineCenter(r,z,n,z2)
 use set_precision, only : wp
 use cornea_arrays, only : eps
 use spline_interfaces, ONLY : bsearch, thomas
 use LapackInterface, ONLY : dgtsv
  integer, INTENT(IN) :: n
  real(wp), INTENT(IN) ::  r(n),z(n)
  real(wp), INTENT(OUT) :: z2(n)
  real(wp) ::  zz2(n+1),rr(n+1),zz(n+1),zzhigh
  real(wp) ::  a(n),b(n),c(n),d(n),a_short(n-1)
  integer :: info, high, low, i

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
    i=low+1
    a(i-1)=((rr(i)-rr(i-1))**2)/6.0
    b(i-1)=(  (rr(i)-rr(i-1))**2 - (rr(i+1)-rr(i))**2  )/3.0
    c(i-1)=((rr(i+1)-rr(i))**2)/6.0
    d(i-1)=(zz(i+1)-zz(i-1))
!  second half
   do i=high+1,n+1
     a(i-1)=(rr(i)-rr(i-1))/6.0
     b(i-1)=(rr(i+1)-rr(i-1))/3.0
     c(i-1)=(rr(i+1)-rr(i))/6.0
     d(i-1)=(zz(i+1)-zz(i))/(rr(i+1)-rr(i))-(zz(i)-zz(i-1))/(rr(i)-rr(i-1))
   end do

!   zz2(:)=d(:) ! for lapack because it overwrites into solution
!   do i=1,n-1
!    a_short(i)=a(i+1) ! truncated "a" for dgtsv, don't have to truncate "c"
!   end do
!  call dgtsv( n, 1, a_short, b, c, zz2, n, INFO )     ! overwrites b and d into solution
  call thomas(a,b,c,d,zz2,n,1) ! can use to check against lapack
  
! remove centerpoint zz2(low+1)
   do i=2,low   !zz2(1)=0 natural spline
     z2(i)=zz2(i)
   end do
!  save these later, I'll need them
   i=low+1
!   zz2(i)
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
   
 end subroutine nsplineCenter

