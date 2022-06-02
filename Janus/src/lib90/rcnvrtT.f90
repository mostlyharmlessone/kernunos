subroutine RCNVRTT(MM,N,NP)

USE set_precision, ONLY : wp
USE cornea_arrays

 INTEGER :: ITH,i,j 
 REAL(wp) :: DIST,R,A,B,X,YP,POW,XDIST,YDIST,XX,YY,DX,DY

! EyeSys or Atlas Simulation data

  RadSlope%MV=0.0_wp
  do i=1,MM
    if (MM == 360) then
    EyeSys%DEG(i)=i-1
    RadSlope%thta(i)=PI*EyeSys%DEG(i)/180.0_wp
    else  !MM == 180
    Atlas%DEG(i)=2*i-1
    RadSlope%thta(i)=PI*Atlas%DEG(i)/180.0_wp
    endif
    do j=1,N       

!     ROUND MIRES, SINGLE AXIAL POWER SPHERE         

        DIST=0.2_wp+(j-1)*0.15_wp
!       ELLIPSOID WITH ASTIGMATISM Z=R-R*SQRT(1-(rCOSt/A)^2-(rSINt)/B)^2)
        R=50.0_wp
	A=40.0_wp
	B=30.0_wp
	X=DIST*A/5.0
!	D=DIST  alternate version without scale for derivative magnitude check
        D=X
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
        POW=ABS(X/YP)*SQRT(1+YP**2)
        POW=ABS(D/YP)*SQRT(1+YP**2)
             
      if (POW > 0 .AND. DIST > 0) then   ! should always be true
        RadSlope%MV(i)=RadSlope%MV(i)+1             
        EyeSys%XX(i,j)=RFCT/POW
        EyeSys%RA(i,j)=DIST*100	

        Atlas%AR(I,J)=DIST
        Atlas%AD(I,J)=DIST
        Atlas%AP(I,J)=POW  
        Atlas%AY(I,J)=0.0_wp
      else
        write(*,*) 'error in RCNVRTT'  
      endif       	
	    		        	  
     end do 
  end do

!    PentaCam Simulation data
  do i=1,NP
   do j=1,NP

!     ROUND MIRES, SINGLE AXIAL POWER SPHERE         

        XDIST=(-7.00+((i-1)*14.00)/(NP-1.0))/400.0
        YDIST=(-7.00+((j-1)*14.00)/(NP-1.0))/400.0

!       ELLIPSOID WITH ASTIGMATISM Z=R-R*SQRT(1-(X/A)^2-(Y/B)^2)
        R=50.0_wp
	A=40.0_wp
	B=30.0_wp
	XX=XDIST*A/5.0
	YY=YDIST*B/5.0
!	DX=XDIST  alternate version without scale for derivative magnitude check
!	DY=YDIST  alternate version without scale for derivative magnitude check
        DX=XX
        DY=YY
        D=sqrt(DX*DX+DY*DY)

        YP=-(DX/A)**2 - (DY/B)**2
        YP=YP*D/SQRT(1 - (DX/A)**2 - (DY/B)**2 )

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

        YT=(DY/B**2-DX/A**2)
        YT=YT*R/SQRT(1-(DX/A)**2-(DY/B)**2)

!       NEED A CHECK ON ELEVATION 
        YZ=R-R*SQRT(1-(DX/A)**2-(DY/B)**2) 
        POW=ABS(D/YP)*SQRT(1+YP**2) 
  
   if ( D < R/400.0 ) then
    Penta%ELE(i,j)=YZ
    Penta%CUR(i,j)=POW 

   else
    Penta%ELE(i,j)=-1
    Penta%CUR(i,j)=-1
   endif
   end do
  end do 

write (*,*) Penta%ELE
   
stop

 end subroutine RCNVRTT     
