subroutine thomas(a,b,c,d,z,n) ! "Llewellyn Thomas" algorithm for tridiagonal banded matrices"
use set_precision, only : wp
integer, INTENT(IN) :: n
real(wp), INTENT(INOUT) :: a(n),b(n),c(n),d(n)
real (wp), INTENT(OUT) :: z(n)
real(wp) :: g
integer i
do i=2,n
 g=a(i)/b(i-1)
 b(i)= b(i)-g*c(i-1)
 d(i)=d(i)-g*d(i-1)
end do
 z(n)= d(n)/b(n)
do i=n-1,1,-1
 z(i)=(d(i)-c(i)*z(i+1))/b(i)
end do 
return
end
