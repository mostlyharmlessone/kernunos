       subroutine pspli(t,z,n,zt2, err_report)
       use cornea_arrays, only : PI, EPS
       use set_precision, only :  wp
       USE special_fct, ONLY : OPERATOR(.p.) !tensor summation convention
       use LapackInterface, ONLY : dctsv !, dgesv, GaussJordan
       use,intrinsic :: ieee_arithmetic

!      PERIODIC BOUNDARY CONDITION SPLINE
       REAL(wp), intent(in) :: t(n)
       REAL(wp), intent(in) :: z(n)
       INTEGER, intent(in) :: n
       REAL(wp), intent(out) ::zt2(n)
       integer, INTENT(OUT) :: err_report
       REAL(wp) :: PERD,error
       REAL(wp) :: d(n),a(n),b(n),c(n) !,AA(n,n)
       INTEGER :: m,j,info !,ipiv(n)
       logical :: IsInf 

       INFO=0   ; err_report = 0
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
       zt2=0
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
       call DCTSV( m, 1, a, b, c, d, m, INFO ) ! d is overwritten
!      lapack full matrix routine
!      aa=0
!      do j=1,m-1
!       AA(j,j)=b(j)
!       AA(j+1,j)=c(j)
!       AA(j,j+1)=a(j+1)
!      end do
!      AA(m,m)=b(m)
!      AA(1,m)=c(m)
!      AA(m,1)=a(1)
!      call DGESV(m, 1, AA, m, IPIV, D, m, INFO ) ! d is overwritten
!      call GaussJordan( m, 1, AA, m, D, m, INFO )
       if (info > 0) then
        write(*,*) 'pspli matrix solving error, info:',info
        err_report = info
       endif

       zt2=d             
       if (m < n) then  !degenerate case where these points are identical
       zt2(n)=zt2(1)
       else
!      DONE if m == n
       endif 

       error=zt2 .p. zt2
       IsInf=ieee_is_finite(error)

       if(.not.IsInf) then
        write(*,*) 'Warning from pspli',m,n,t(1),t(m),ABS(t(1)-t(m)+PERD),ieee_is_finite(z .p. z)
        err_report = -1
        return
       endif
       
       end subroutine pspli
       
