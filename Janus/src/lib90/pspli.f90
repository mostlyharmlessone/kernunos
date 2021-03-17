       subroutine pspli(t,z,n,zt2)
       use cornea_arrays, only : PI, MM, EPS
       use set_precision, only :  wp
       USE special_fct, ONLY : OPERATOR(.p.) !tensor summation convention
       use,intrinsic :: ieee_arithmetic

!      PERIODIC BOUNDARY CONDITION SPLINE
       REAL(wp), intent(in) :: t(n),z(n)
       INTEGER, intent(in) :: n
       REAL(wp), intent(out) ::zt2(n)
       REAL(wp) :: PERD,error
       REAL(wp) :: d(n),a(n),b(n),c(n),zt2c(n),thta(MM) 
       INTEGER :: m,j 
       logical :: IsInf 

       PERD=2*PI
!      ill-conditioning with PERD close to t(n)-t(1)   
!      CASE WHERE c(n)/=a(1) AND z(1)/=z(n) AND t(1)-t(n)+PERD/=0        
       if (ABS(t(1)-t(n)+PERD) > EPS) then  ! this test implicitly assumes t(i) are spaced MM apart over PERD 
        m=n                   
       else
!      REDUCE TO n-1 POINTS, BECAUSE THERE ARE ONLY n-1 UNIQUE POINTS AND t(n),z(n) ARE DEGENERATE
!       if (ABS((z(1)-z(n))/(z(1)+z(n))) > EPS) because if t(1)==t(n) and z(fct(t)) then z(1)==z(n)    
        m=n-1   !this effectively takes the next point and skips t(n)
       endif 
!      INITIALIZE           
       a(1)=(t(1)-t(m)+PERD)/6.0
       b(1)=(t(2)-t(m)+PERD)/3.0
       c(1)=(t(2)-t(1))/6.0
       d(1)=(z(2)-z(1))/(t(2)-t(1))-&
            (z(1)-z(m))/(t(1)-t(m)+PERD)
       a(m)=(t(m)-t(m-1))/6.0
       b(m)=(t(1)-t(m-1)+PERD)/3.0
       c(m)=(t(1)-t(m)+PERD)/6.0
       d(m)=(z(1)-z(m))/(t(1)-t(m)+PERD)-&
            (z(m)-z(m-1))/(t(m)-t(m-1))           
       do j=2,m-1
        a(j)=(t(j)-t(j-1))/6.0
        b(j)=(t(j+1)-t(j-1))/3.0
        c(j)=(t(j+1)-t(j))/6.0
        d(j)=(z(j+1)-z(j))/(t(j+1)-t(j))-(z(j)-z(j-1))/(t(j)-t(j-1))
       end do 
        
!      SOLVE THE TRIDIAGONAL PERIODIC CASE                     
       call DCTSV( m,1, a, b, c, d, m, INFO ) ! d is overwritten                    
       zt2=d             
       if (m < n) then  !degenerate case where these points are identical
       zt2(n)=zt2(1)
       else
!      DONE if m == n
       endif 

   error=zt2 .p. zt2
   IsInf=ieee_is_finite(error)
   If(.not.IsInf) then
    write(*,*) 'Warning from pspli',m,n,t(1),t(n),ABS(t(1)-t(n)+PERD),PERD/MM+EPS
    stop
   endif
       
       end subroutine pspli
       
