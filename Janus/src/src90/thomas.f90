subroutine thomas(a,b,c,d,z,n,k) ! "Llewellyn Thomas" algorithm for tridiagonal banded matrices"
! Adapted from https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm
use set_precision, only : wp
!use,intrinsic :: ieee_arithmetic
implicit none
integer, INTENT(IN) :: n,k
real(wp), INTENT(INOUT) :: a(n),b(n),c(n),d(k,n)
real (wp), INTENT(OUT) :: z(k,n)
real(wp) :: g !,f
integer i
!logical :: IsNaN
do i=2,n
! if (b(i-1) > 0) then
   g=a(i)/b(i-1)
! else
!  write(*,*) 'Zero pivot in thomas',i-1,b(i-1)
! endif
 b(i)= b(i)-g*c(i-1)
 d(:,i)=d(:,i)-g*d(:,i-1)
end do
!if (b(n) > 0) then
 z(:,n)= d(:,n)/b(n)
!else
! write(*,*) 'Zero pivot in thomas',n,b(n)
!endif
do i=n-1,1,-1
! if (b(i) > 0) then
 z(:,i)=(d(:,i)-c(i)*z(:,i+1))/b(i)
! else
!  write(*,*) 'Zero pivot in thomas',i,b(i)
! endif
end do 
return
end subroutine thomas
