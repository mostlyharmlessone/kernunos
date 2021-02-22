 subroutine SplineCenter(r,z,zr2,n,u)
 use set_precision, only : wp
 use cornea_arrays, only : eps
 USE spline_interfaces, ONLY : bsearch, SplineEval
  real(wp), INTENT(IN) ::  r(n),z(n),zr2(n)
  integer, INTENT(IN) :: n
  real(wp), INTENT(OUT) :: u
  real(wp) :: g,gr
  integer :: high, low, j
  
! find center
  call bsearch(0.0_wp,r,n,high,low)
! there's an algebraic solution with the real root of the cubic 0=f=A*y(i)+B*y(i1)+((A**3-A)*y2(i)+(B**3-B)*y2(i1))*(dr**2)/6.0_wp
!  Q=(a1**2-3*a2)/9
!  R=(2*a1**3-9*a1*a2+27*a3)/54
!  if ( (R**2-Q**3) > 0 ) then
!  u=-sgn(R)*((Sqrt(R**2-Q**3)+Abs(R))**(1/3)+Q/((Sqrt(R**2-Q**3)+Abs(R))**(1/3)))-a1/3
!  else
!  write(*,*) 'Error in SplineCenter, multiple real roots'
!  stop
!  end if
 
! Newton's method is quicker and cleaner 
  j=0
  g=(z(high)-z(low))/2.0_wp
  u=(r(high)+r(low))/2.0_wp
  gr=2*g/(r(high)-r(low))
  do while ((j < 10) .AND. (ABS(g/gr) > eps)) 
   j=j+1
      
        if (ABS(u) > 10) then 
        
     write(*,*) 'Probable error on iterations in SplineCenter', high,low      
         write(*,*) 'j,u,g,gr,g/gr:',j,u,g,gr,g/gr
         write(*,*) ' '
           write(*,*) 'r:',r
   write(*,*) ' '
   write(*,*) 'z:',z
   write(*,*) ' '
   write(*,*) 'zr2:',zr2
   write(*,*) ' ' 
   stop 
         
        endif
   
   call SplineEval(0,r,z,zr2,n,u,g,gr)
      
   u=u-g/gr
   
  end do
! should take under 10 iterations  
  if (j > 9) then
   write(*,*) 'Probable error on iterations in SplineCenter', high,low
   write(*,*) 'r:',r
   write(*,*) ' '
   write(*,*) 'z:',z
   write(*,*) ' '
   write(*,*) 'z2:',zr2
   write(*,*) ' '   
   stop
  endif 
   
  end subroutine SplineCenter
