subroutine RCNVRTT(MM,N)

USE set_precision, ONLY : wp
USE cornea_arrays

 INTEGER :: i,j
 INTEGER, INTENT(IN) :: MM,N
 REAL(wp) :: DIST,R,A,B,X,YP,POW,D,YT,YZ

! fake EyeSys

  RadSlope%MV=0.0_wp
  do i=1,MM
    EyeSys%DEG(i)=i-1
    RadSlope%thta(i)=PI*EyeSys%DEG(i)/180.0_wp
    do j=1,N+1     

!     ROUND MIRES, SINGLE AXIAL POWER SPHERE    
       if (j > N) then
        R=48.0_wp
        A=46.0_wp
        B=50.0_wp
        D=0.0_wp ; X=0.0_wp
       else 
        DIST=0.2_wp+(j-1)*0.13_wp
!        DIST=(j-1)/(N-1.)
!       ELLIPSOID WITH ASTIGMATISM Z=R-R*SQRT(1-(rCOSt/A)^2-(rSINt)/B)^2)
        R=48.0_wp
        A=46.0_wp
        B=50.0_wp
        X=DIST*A/5.
        D=X
       endif 
        YP=(-(R/A**2)*COS(RadSlope%thta(i))**2-(R/B**2)*SIN(RadSlope%thta(i))**2)
        YP=YP*X/SQRT(1-(X*COS(RadSlope%thta(i))/A)**2-(X*SIN(RadSlope%thta(i))/B)**2)
        YP=(-(R/A**2)*COS(RadSlope%thta(i))**2-(R/B**2)*SIN(RadSlope%thta(i))**2)
        YP=YP*D/SQRT(1-(D*COS(RadSlope%thta(i))/A)**2-(D*SIN(RadSlope%thta(i))/B)**2)
!       THE SIGN OF YP IS BY CONVENTION THIS WAY, NOT THE REVERSE BY THE USUAL CONVENTION
!       SIGN OF SLOPE IS ANNIHILATED BY POWER CONVERSION
!        IF(ITHETA.GE.180) THEN 
!        YP1=YP
!        YP=-YP1
!        else
!        YP1=YP
!        YP=YP1            
!        ENDIF 	
!       NEED A CHECK ON MY THETA DERIVATIVES SINCE THESE MIRES HAVE CONSTANT RADII
        YT=COS(RadSlope%thta(i))*SIN(RadSlope%thta(i))*(X**2/B**2-X**2/A**2)
        YT=YT*R/SQRT(1-(X*COS(RadSlope%thta(i))/A)**2-(X*SIN(RadSlope%thta(i))/B)**2) 
        YT=COS(RadSlope%thta(i))*SIN(RadSlope%thta(i))*(D**2/B**2-DIST**2/A**2)
        YT=YT*R/SQRT(1-(D*COS(RadSlope%thta(i))/A)**2-(D*SIN(RadSlope%thta(i))/B)**2)
!       NEED A CHECK ON ELEVATION
        YZ=R-R*SQRT(1-(X*COS(RadSlope%thta(i))/A)**2-(X*SIN(RadSlope%thta(i))/B)**2) 
        YZ=R-R*SQRT(1-(D*COS(RadSlope%thta(i))/A)**2-(D*SIN(RadSlope%thta(i))/B)**2)
      if (j > N) then
        POW=48  ! SAGC undefined when YP=0
      else    
        POW=ABS(X/YP)*SQRT(1+YP**2)
        POW=ABS(D/YP)*SQRT(1+YP**2)
      endif
      if (j > N) then
      else            
       if (POW > 0 .AND. DIST > 0) then   ! should always be true
        RadSlope%MV(i)=RadSlope%MV(i)+1             
        EyeSys%XX(i,j)=RFCT/POW
        EyeSys%RA(i,j)=DIST*100
       else
        write(*,*) 'error in RCNVRTT'  
       endif
      endif
     end do 
  end do

 end subroutine RCNVRTT     
