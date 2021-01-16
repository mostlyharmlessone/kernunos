       subroutine pspli(t,z,n,zt2)
       use cornea_arrays, only : PI, MM, EPS, RadSlope
       use set_precision, only :  wp
       USE zernicke, ONLY : OPERATOR(.p.) !tensor summation convention
       use,intrinsic :: ieee_arithmetic

!      THIS VERSION USES MY ALGORITHM
!      PERIODIC BOUNDARY CONDITION SPLINE
       REAL(wp), intent(in) :: t(n),z(n)
       INTEGER, intent(in) :: n
       REAL(wp), intent(out) ::zt2(n)
       REAL(wp) :: DET,PERD,error
       REAL(wp) :: d(n),a(n),b(n),c(n),zt2c(n),thta(MM)
       REAL(wp) :: ud(2,2,n/2+1),ue(2,n/2+1)  !  ud is my set of matrices Aj ue is my vectors vj 
       INTEGER :: m,i,j 
       logical :: IsInf 

       REAL(wp) :: AB(3,n)   ! for testing of  DCBSv only

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
!      SOLVE THE TRIADIAGONAL PERIODIC CASE       
       do j=2,m-1
        a(j)=(t(j)-t(j-1))/6.0
        b(j)=(t(j+1)-t(j-1))/3.0
        c(j)=(t(j+1)-t(j))/6.0
        d(j)=(z(j+1)-z(j))/(t(j+1)-t(j))-(z(j)-z(j-1))/(t(j)-t(j-1))
       end do                
!      FIRST EQUATION (zt2(j-1),zt(n-j+2)=A(j-1).p.((zt2(j),zt(n-j+1))+v(j-1) A(0)=((0,1),(1,0) v(0)=(0,0)
       j=1
        DET=b(j)*b(1-j+m)-a(j)*c(1-j+m)
        ud(1,1,j)=-c(j)*b(1-j+m)/DET
        ud(1,2,j)=a(1-j+m)*c(1-j+m)/DET
        ud(2,1,j)=a(j)*c(j)/DET
        ud(2,2,j)=-a(1-j+m)*b(j)/DET
        ue(1,j)=(d(j)*b(1-j+m)-c(1-j+m)*d(1-j+m))/DET
        ue(2,j)=(-a(j)*d(j)+d(1-j+m)*b(j))/DET               
!      ALL BUT THE LAST EQUATION (zt2(j-1),zt(n-j+2)=A(j-1).p.((zt2(j),zt(n-j+1))+v(j-1)
       do j=2,INT(1+(m-1)/2)
        DET=b(j)*b(1-j+m)+a(j)*b(1-j+m)*ud(1,1,-1+j)-& 
            a(j)*c(1-j+m)*ud(1,2,-1+j)*ud(2,1,-1+j)+&
            b(j)*c(1-j+m)*ud(2,2,-1+j)+&
            a(j)*c(1-j+m)*ud(1,1,-1+j)*ud(2,2,-1+j)
        ud(1,1,j)=-((c(j)*(b(1-j+m)+c(1-j+m)*ud(2,2,-1+j)))/DET)
        ud(1,2,j)=(a(j)*a(1-j+m)*ud(1,2,-1+j))/DET
        ud(2,1,j)=(c(j)*c(1-j+m)*ud(2,1,-1+j))/DET
        ud(2,2,j)=-((a(1-j+m)*(b(j)+a(j)*ud(1,1,-1+j)))/DET)        
        ue(1,j)=(-a(j)*(d(1-j+m)-c(1-j+m)*ue(2,-1+j))*ud(1,2,-1+j)+&
                (d(j)-a(j)*ue(1,-1+j))*(b(1-j+m)+c(1-j+m)*ud(2,2,-1+j)))/DET
        ue(2,j)=((d(1-j+m)-c(1-j+m)*ue(2,-1+j))*(b(j)+a(j)*ud(1,1,-1+j))-&
                c(1-j+m)*(d(j)-a(j)*ue(1,-1+j))*ud(2,1,-1+j))/DET                
       end do 
!      LAST EQUATION 
       j=INT(m/2)
        if ( mod(j,2) == 0 ) then
!      EVEN CASE (2x2) 
!      NO ud(,,j); ue(,j) iS THE SOLUTION AT j,j+1
        DET=b(j)*b(1+j)-a(1+j)*c(j)+a(j)*b(1+j)*ud(1,1,-1+j)-&
            a(j)*a(1+j)*ud(1,2,-1+j)-c(j)*c(1+j)*ud(2,1,-1+j)-&
            a(j)*c(1+j)*ud(1,2,-1+j)*ud(2,1,-1+j)+b(j)*c(1+j)*ud(2,2,-1+j)+&
            a(j)*c(1+j)*ud(1,1,-1+j)*ud(2,2,-1+j)            
        ue(1,j)=((d(1+j)-c(1+j)*ue(2,-1+j))*(-c(j)-a(j)*ud(1,2,-1+j))+&
                (d(j)-a(j)*ue(1,-1+j))*(b(1+j)+c(1+j)*ud(2,2,-1+j)))/DET                 
        ue(2,j)=((d(1+j)-c(1+j)*ue(2,-1+j))*(b(j)+a(j)*ud(1,1,-1+j))+&
                (d(j)-a(j)*ue(1,-1+j))*(-a(1+j)-c(1+j)*ud(2,1,-1+j)))/DET 
        zt2(j)=ue(1,j)
        zt2(j+1)=ue(2,j)       
        else
!      (m-2*j+1 == 2)
!       ODD CASE (3x3)
!       NO ud(,,j); zt2(j ETC.) iS THE SOLUTiON AT j,j+1,j+2
        DET=b(j)*b(1+j)*b(2+j)-a(1+j)*b(2+j)*c(j)-a(2+j)*b(j)*c(1+j)+&
            a(j)*b(1+j)*b(2+j)*ud(1,1,-1+j)-a(j)*a(2+j)*c(1+j)*ud(1,1,-1+j)+&
            a(j)*a(1+j)*a(2+j)*ud(1,2,-1+j)+c(j)*c(1+j)*c(2+j)*ud(2,1,-1+j)-&
            a(j)*b(1+j)*c(2+j)*ud(1,2,-1+j)*ud(2,1,-1+j)+b(j)*b(1+j)*c(2+j)*ud(2,2,-1+j)-&
            a(1+j)*c(j)*c(2+j)*ud(2,2,-1+j)+a(j)*b(1+j)*c(2+j)*ud(1,1,-1+j)*ud(2,2,-1+j)
            
        zt2(j)=((d(2+j)-c(2+j)*ue(2,-1+j))*(c(j)*c(1+j)-a(j)*b(1+j)*ud(1,2,-1+j))+&
                 d(j)-a(j)*ue(1,-1+j)*(b(1+j)*b(2+j)-a(2+j)*c(1+j)+&
                 b(1+j)*c(2+j)*ud(2,2,-1+j))+d(1+j)*(-b(2+j)*c(j)+&
                 a(j)*a(2+j)*ud(1,2,-1+j))+d(1+j)*(-c(j)*c(2+j)*ud(2,2,-1+j)))/DET 
                                 
        zt2(j+1)=((d(2+j)-c(2+j)*ue(2,-1+j))*(-b(j)*c(1+j)-a(j)*c(1+j)*ud(1,1,-1+j)+&
                 a(j)*a(1+j)*ud(1,2,-1+j))+(d(j)-a(j)*ue(1,-1+j))*(-a(1+j)*b(2+j)+&
                 c(1+j)*c(2+j)*ud(2,1,-1+j)-a(1+j)*c(2+j)*ud(2,2,-1+j))+&
                 d(1+j)*(b(j)*b(2+j)+a(j)*b(2+j)*ud(1,1,-1+j)-a(j)*c(2+j)*ud(1,2,-1+j)*&
                 ud(2,1,-1+j)+b(j)*c(2+j)*ud(2,2,-1+j)+a(j)*c(2+j)*ud(1,1,-1+j)*ud(2,2,-1+j)))/DET
                 
        zt2(j+2)=((d(2+j)-c(2+j)*ue(2,-1+j))*(b(j)*b(1+j)-a(1+j)*c(j)+&
                 a(j)*b(1+j)*ud(1,1,-1+j))+(d(j)-a(j)*ue(1,-1+j))*(a(1+j)*a(2+j)-&
                 b(1+j)*c(2+j)*ud(2,1,-1+j))+d(1+j)*(-a(2+j)*b(j)-a(j)*a(2+j)*ud(1,1,-1+j)+&
                 c(j)*c(2+j)*ud(2,1,-1+j)))/DET                 
        endif
!      BACKSUBSTITUTION
       do i=j-1,1,-1
        zt2(i)=ue(1,i)+ud(1,1,i)*zt2(i+1)+ud(1,2,i)*zt2(m-i)
        zt2(m-i+1)=ue(2,i)+ud(2,1,i)*zt2(i+1)+ud(2,2,i)*zt2(m-i)
       end do
       if (m < n) then  
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

   call cyclic(t,z,n,zt2c)
   AB(1,:)=a
   AB(2,:)=b
   AB(3,:)=c
   call BADMATRIX(N,1,1,AB,3,d,n,INFO)
write(*,*) 'made it here 1'   

   call DCBSV( N,1,1,AB,3,d,n,INFO)       ! overwrites d into solution
!   call DCTSV( n, 1, a, b, c, d, n, INFO ) ! overwrites d into solution 

write(*,*) 'made it here'

   error=SQRT((zt2-d) .p. (zt2-d))
   If(error > 40000) then    
    write(*,*) 'pspli-DCTSV',SQRT((zt2-d) .p. (zt2-d))
    write(*,*) 'diff',FLOOR(ABS(zt2-d))   
    stop
   endif

   error=SQRT((zt2-zt2c) .p. (zt2-zt2c))
   If(error > 40000) then   
    write(*,*) 'pspli-cyclic',SQRT((zt2-zt2c) .p. (zt2-zt2c))
    write(*,*) 'diff',FLOOR(ABS(zt2-zt2c))   
    stop
   endif

    
       end subroutine pspli
       
