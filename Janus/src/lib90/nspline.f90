 subroutine nspline(r,z,n,z2)
 use set_precision, only : wp
 use LapackInterface, ONLY : dgbsv
  real(wp), INTENT(IN) ::  r(n),z(n)
  integer, INTENT(IN) :: n
  real(wp), INTENT(OUT) :: z2(n)
  real(wp) ::  a(n-2),b(n-2),c(n-2),d(n-2),zz2(n-2),AB(5,n-2)
  integer :: ipiv(n-2), info     
  AB=0 
  INFO=0
  a=0
  b=0
  c=0
  d=0 
! boundary conditions for natural spline   
  z2(1)=0.      
  z2(n)=0.      
! spline equation at internal knots
   do i=2,n-1
     a(i-1)=(r(i)-r(i-1))/6.0
     b(i-1)=(r(i+1)-r(i-1))/3.0
     c(i-1)=(r(i+1)-r(i))/6.0
     d(i-1)=(z(i+1)-z(i))/(r(i+1)-r(i))-(z(i)-z(i-1))/(r(i)-r(i-1))
     zz2(i-1)=d(i-1) ! for lapack
     end do 
!    lapack bandform
!    AB(KL+KU+1+i-j,j) = A(i,j) KU includes main diagonal
     AB(3,:)=c(:)
     AB(4,:)=b(:)
     AB(5,:)=a(:)                 
   call dgbsv(n-2,1,2,1,AB,5,ipiv,zz2,n-2,info)    ! general band;  AB(3,1) and AB(5,n-2) are ignored
!   call thomas(a,b,c,d,z2,n-2) ! can use to check against lapack, doesn't use a(1) or c(n)
!   call dgtsv (n-2, 1, a, b, c, zz2, n-2, INFO)  ! tridiagonal lapack a,c are (n-3) b, zz2 are (n-2)
     a(1)=0
     c(n)=0
!   call DCTSV( n-2 ,1, a, b, c, zz2, n-2, INFO )  ! or call periodic version
     AB=0
     AB(1,:)=a(:)
     AB(2,:)=b(:)
     AB(3,:)=c(:)
!   call DCBSV( n-2, 1, 1, AB, 3, zz2, n-2, INFO ) ! or call general periodic version
!
   do i=2,n-1
     z2(i)=zz2(i-1)
   end do  
   
!  call SPLINE(R,Z,N,1.E30,1E.30,ZZ2)   ! check with NR recipe
        
 end subroutine nspline

