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
    end do 
     zz2(:)=d(:) ! for lapack
!    lapack bandform
!    AB(KL+KU+1+i-j,j) = A(i,j) KU includes main diagonal
     AB(3,:)=c(:)
     AB(4,:)=b(:)
     AB(5,:)=a(:)                 
   call dgbsv(n-2,1,2,1,AB,5,ipiv,zz2,n-2,info)    ! general band;  AB(3,1) and AB(5,n-2) are ignored
!   call thomas(a,b,c,d,zz2,n-2) ! can use to check against lapack, doesn't use a(1) or c(n); overwrites b and d
!
   do i=2,n-1
     z2(i)=zz2(i-1)
   end do  
        
 end subroutine nspline

