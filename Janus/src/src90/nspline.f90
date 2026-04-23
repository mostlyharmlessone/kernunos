 subroutine nspline(r,z,n,z2,err_report)
 use set_precision, only : wp
 use LapackInterface, only : dgtsv
 use spline_interfaces, only : thomas
 use,intrinsic :: ieee_arithmetic
 implicit none
  integer, INTENT(IN) :: n
  real(wp), INTENT(IN) ::  r(n)
  real(wp), INTENT(IN) ::  z(n)
  real(wp), INTENT(OUT) :: z2(n) 
  integer, INTENT(OUT) :: err_report
  real(wp),allocatable ::  a(:),b(:),c(:),d(:),zz2(:),a_short(:)
  integer :: i,info    ! for lapack use below
  real(wp) :: f      ! error handling
  logical :: IsNaN    
  INFO=0   ; err_report = 0
 ! if(r(1) .eq. r(2)) then
 !  write(*,*) 'Bad r in nspline'
 !  err_report=2
 !  return
 ! endif
  if ( n < 2 ) then  ! invalid parameter
   err_report=-2
   return
  endif 
! boundary conditions for natural spline   
  z2(1)=0.      
  z2(n)=0  
  if ( n == 2 ) then
   write(*,*) "Warning degenerate nspline",r,z
   err_report=-1
   return  ! degenerate case  
  else 
    allocate (a(n-2),b(n-2),c(n-2),d(n-2),zz2(n-2))
  endif  
  a=0  ;  b=0  ;   c=0  ;  d=0
! spline equation at internal knots
    do i=2,n-1
     a(i-1)=(r(i)-r(i-1))/6.0
     b(i-1)=(r(i+1)-r(i-1))/3.0
     c(i-1)=(r(i+1)-r(i))/6.0
     d(i-1)=(z(i+1)-z(i))/(r(i+1)-r(i))-(z(i)-z(i-1))/(r(i)-r(i-1))
    end do
!    lapack
    if (n > 3) then                                   ! n > 3 only if using LAPACK dgtsv
    allocate (a_short(n-2))
     do i=1,n-3
      a_short(i)=a(i+1)                               ! truncated "a" for dgtsv, don't have to truncate "c"
     end do
    endif
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
    write(*,*) 'FATAL from nspline: NaN terms, check r,z,z2, matrix entries; ',INFO
    do i=2,n-1
     a(i-1)=(r(i)-r(i-1))/6.0
     b(i-1)=(r(i+1)-r(i-1))/3.0
     c(i-1)=(r(i+1)-r(i))/6.0
     d(i-1)=(z(i+1)-z(i))/(r(i+1)-r(i))-(z(i)-z(i-1))/(r(i)-r(i-1))
    end do
    do i=2,n-2
     write(*,*) r(i-1),z(i-1),z2(i-1),a(i-1),b(i-1),c(i-1),d(i-1)
    end do
    err_report=1
    stop
   endif
 
   deallocate (a,b,c,d,zz2)
        
 end subroutine nspline

