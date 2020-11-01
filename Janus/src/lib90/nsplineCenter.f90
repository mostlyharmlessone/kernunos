 subroutine nsplineCenter(r,z,n,z2)
 use set_precision, only : wp
 use spline_interfaces, ONLY : bsearch
 use LapackInterface, ONLY : dgbsv
  real(wp), INTENT(IN) ::  r(n),z(n)
  integer, INTENT(IN) :: n
  real(wp), INTENT(OUT) :: z2(n)
  real(wp) ::  a(n-1),b(n-1),c(n-1),d(n-1),zz2(n-1),AB(5,n-1),rr(n+1),zz(n+1)
  integer :: ipiv(n-1), info, high, low

! PURPOSE adds a central node to the radial spline either at the origin or at the mean distance between central points

! find center
  call bsearch(0.0_wp,r,n,high,low)
  do i=1,low
   rr(i)=r(i)
   zz(i)=z(i)
  end do
! add a centerpoint for slopes calc zero slope at origin
   rr(low+1)=0_wp
   zz(low+1)=0_wp
!  Or zero slope at average/linear interpolation for each diagonal instead of single point
   rr(low+1)=(r(low)+r(high))/2.0
  do i=high,n  ! high=low+1
   rr(i+1)=r(i)
   zz(i+1)=z(i)
  end do

  AB=0  
! boundary conditions for natural spline   
  z2(1)=0.      
  z2(n)=0.      
! spline equation at internal knots
   do i=2,n
     a(i-1)=(rr(i)-rr(i-1))/6.0
     b(i-1)=(rr(i+1)-rr(i-1))/3.0
     c(i-1)=(rr(i+1)-rr(i))/6.0
     d(i-1)=(zz(i+1)-zz(i))/(rr(i+1)-rr(i))-(zz(i)-zz(i-1))/(rr(i)-rr(i-1))
     zz2(i-1)=d(i-1) ! for lapack
!    lapack bandform
!    AB(KL+KU+1+i-j,j) = A(i,j) KU includes main diagonal
     AB(4,i-1)=b(i-1)
     AB(3,i-1)=a(i-1)
     AB(5,i-1)=c(i-1)   
   end do       
   call dgbsv(n-1,1,2,1,AB,5,ipiv,zz2,n-1,info)
!   call thomas(a,b,c,d,z2,n-1) ! can use to check against lapack
! The lapack insertion below from Hanson & Hopkins chapter 2: exampleLapack90.f90
! Check that the Lapack routine has been successful
   if (info<0) then
    WRITE (*,'(''Argument '',i3,'' has an illegal value'')') - info
   else if (info>0) then
    WRITE (*,'(''Zero diagonal value detected in upper ''// &
     &                       ''triangular factor at position &
     &'',i7)') info
     stop
   endif
! remove centerpoint   
   do i=2,low
     z2(i)=zz2(i-1)
   end do
! skip zz2(low) high=low+1
   do i=high,n-1
     z2(i)=zz2(i)
   end do
   
 end subroutine nsplineCenter

