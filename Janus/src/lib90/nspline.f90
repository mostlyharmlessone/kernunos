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
!    lapack bandform
!    AB(KL+KU+1+i-j,j) = A(i,j) KU includes main diagonal
     AB(3,i-1)=a(i-1)
     AB(4,i-1)=b(i-1)
     AB(5,i-1)=c(i-1)   
   end do       
   call dgbsv(n-2,1,2,1,AB,5,ipiv,zz2,n-2,info)
!   call thomas(a,b,c,d,z2,n-2) ! can use to check against lapack 
   do i=2,n-1
     z2(i)=zz2(i-1)
   end do   
 end subroutine nspline

