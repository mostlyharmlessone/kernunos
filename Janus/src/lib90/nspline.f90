 subroutine nspline(r,z,n,z2)
 use set_precision, only : wp
 use LapackInterface, ONLY : dgtsv
  real(wp), INTENT(IN) ::  r(n),z(n)
  integer, INTENT(IN) :: n
  real(wp), INTENT(OUT) :: z2(n)
  real(wp) ::  a(n-2),b(n-2),c(n-2),d(n-2),zz2(n-2),a_short(n-3)
  integer :: info     
  INFO=0 ; a=0  ;  b=0  ;   c=0  ;  d=0 
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
    do i=1,n-3
     a_short(i)=a(i+1)                                 ! truncated "a" for dgtsv, don't have to truncate "c"
    end do
     zz2(:)=d(:) ! for lapack
   call thomas(a,b,c,d,zz2,n-2,1) ! can use to check against lapack, doesn't use a(1) or c(n); overwrites b and d
!  call dgtsv( n-2, 1, a_short, b, c, zz2, n-2, INFO )     ! overwrites b and d into solution
   do i=2,n-1
     z2(i)=zz2(i-1)
   end do  
        
 end subroutine nspline

