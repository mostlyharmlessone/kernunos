! returns z2 not as second derivatives but as parameters for linear fit; should be equal to z if r() corresponds to even spaced knots
 SUBROUTINE linspline(r,z,n,z2,err_report)
 USE set_precision, ONLY  : wp
 use LapackInterface, ONLY  : dgtsv
 USE spline_interfaces, ONLY  : thomas
 USE, INTRINSIC :: ieee_arithmetic
  INTEGER, INTENT(IN) :: n
  REAL(wp), INTENT(IN) ::  r(n),z(n)
  REAL(wp), INTENT(OUT) :: z2(n) 
  INTEGER, INTENT(OUT) :: err_report
  REAL(wp),allocatable ::  a(:),b(:),c(:),d(:),u(:),zz2(:),a_short(:)
  INTEGER :: i,info
  REAL(wp) :: f,dr
  LOGICAL :: IsNaN    
  INFO=0   ; err_report = 0 
  if ( n < 2 ) then  ! invalid parameter
   INFO=-1
   return
  endif 
! boundary conditions    
  z2(1)=z(1)      
  z2(n)=z(n)
  if ( n == 2 ) then
   write(*,*) "Warning degenerate nspline",r,z
   err_report=-1
   return  ! degenerate case  
  else 
    allocate (a(n-2),b(n-2),c(n-2),d(n-2),zz2(n-2),u(n-2))
  endif
  a=0  ;  b=0  ;   c=0  ;  d=0
! spline equation at internal knots; this is a trivial identity when r=u
    dr=(r(n)-r(1))/(n-1)
    do i=2,m-1
     u(i-1)=r(1)+(i-2)*dr  ! evenly spaced knots
     a(i-1)=(r(i)-u(i-1))/(u(i)-u(i-1))
     b(i-1)=1-a(i-1)
     c(i-1)=0
     d(i-1)=z(i-1)
    end do
!    lapack
    if (m > 3) then                                   ! n > 3 only if using LAPACK dgtsv
    allocate (a_short(n-2))
     do i=1,n-3
      a_short(i)=a(i+1)                               ! truncated "a" for dgtsv, don't have to truncate "c"
     end do
    endif
!   call thomas(a,b,c,d,zz2,n-2,1) ! can use to check against lapack, doesn't use a(1) or c(n); overwrites b and d
   if (n > 3) then
    zz2(:)=d(:) ! for lapack
    call dgtsv( n-2, 1, a_short, b, c, zz2, n-2, INFO )     ! overwrites b and d into solution
    err_report=INFO
    deallocate(a_short)
   else  ! have to use thomas for n=3
    call thomas(a,b,c,d,zz2,n-2,1) ! can use to check against lapack, doesn't use a(1) or c(n); overwrites b and d
   endif
   do i=2,n-1
     z2(i)=zz2(i-1)
   end do 
   f= dot_product(z2,z2)
   IsNaN=.not.ieee_is_finite(f) .or. .not.ieee_is_finite(dot_product(r,r)) .or. .not.ieee_is_finite(dot_product(z,z))
   If(IsNaN) then
    write(*,*) 'Warning from nspline: NaN terms, check r,z,z2; '
    err_report=1
    return
   endif 
   deallocate (a,b,c,d,u,zz2)
 END SUBROUTINE linspline


 



