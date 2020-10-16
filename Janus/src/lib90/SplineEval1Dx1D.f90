      subroutine SplineEval1Dx1D(iflag,u,v,f,fr,ft,frt,frr,ftt) 
      USE cornea_arrays, ONLY : DiaSlope, RadSlope, MM, N
      USE set_precision, ONLY : wp
      USE spline_interfaces, ONLY : pspli, SplineEval
      use,intrinsic :: ieee_arithmetic
      implicit none
      integer, INTENT(IN) :: iflag     ! iflag=0 no integration
      real(wp), INTENT(IN) :: u, v
      real(wp), INTENT(OUT),OPTIONAL ::  f,fr,ft,frt,frr,ftt
      real(wp) :: g,g0,gr,grr
      real(wp) :: fTmp(MM),frTmp(MM),frrTmp(MM)
      real(wp) :: thta(MM),fttTmp(MM),frttTmp(MM),frrttTmp(MM)
      real(wp) :: r(2*N),z(2*N),zr2(2*N)
      integer :: L2,j,L
      logical :: IsInf
                            
      do j=1,MM/2 
        L2=DiaSlope%L2(j)
        thta(j)=RadSlope%thta(j)
        r=DiaSlope%rd(j,:)
        z=DiaSlope%Zpd(j,:)
        zr2=DiaSlope%Zpd2(j,:)
        L=j+MM/2
        thta(L)=RadSlope%thta(L)
        if (iflag == 0) then
         call SplineEval(0,r,z,zr2,L2,u,g,gr,grr) !first parameter = 0 nonperiodic
         fTmp(j)=g
        else                         
         call SplineEval(0,r,z,zr2,L2,u,gr,grr) 
         call CubicSplineQuad(r,z,zr2,L2,0._wp,g0)    
         call CubicSplineQuad(r,z,zr2,L2,u,g) 
         fTmp(j)=g-g0
        endif      
        frTmp(j)=gr
        frrTmp(j)=grr 
!       odd as it seems, each angle j is also angle L since we're on a diagonal  
        fTmp(L)=fTmp(j)   
        frTmp(L)=frTmp(j)
        frrTmp(L)=frrTmp(j)
	 	
      end do

!       FIRST CALL FOR PERIODIC SPLINE OF f0, fttTmp is d2Y/dTHETA2 
        if (Present(ftt)) then
         call pspli(thta,fTmp,MM,fttTmp)
         call SplineEval(1,thta,fTmp,fttTmp,MM,v,f,ft,ftt) !first parameter = 1 periodic
        else
         if (Present(ft)) then
          call pspli(thta,fTmp,MM,fttTmp)  
          call SplineEval(1,thta,fTmp,fttTmp,MM,v,f,ft)
         else          
          if (Present(f)) then
           call pspli(thta,fTmp,MM,fttTmp)
           call SplineEval(1,thta,fTmp,fttTmp,MM,v,f)
          endif
         endif 	
	endif
!       SECOND CALL FOR PERIODIC SPLINE OF fr (df/dR), frrtTmp is d3Y/dRdTHETA2 	
	if (Present(frt)) then
	 call pspli(thta,frTmp,MM,frttTmp)
	 call SplineEval(1,thta,frTmp,frttTmp,MM,v,fr,frt) 
	else 
	 if (Present(fr)) then
          call pspli(thta,frTmp,MM,frttTmp)
	  call SplineEval(1,thta,frTmp,frttTmp,MM,v,fr)
	 endif 
	endif
!       THIRD CALL FOR PERIODIC SPLINE OF frr (d2f/dR2), frrttTmp is d4Y/dR2dTHETA2	
	if (Present(frr)) then
	 call pspli(thta,frrTmp,MM,frrttTmp)
         call SplineEval(1,thta,frrTmp,frrttTmp,MM,v,frr) 
	endif

        RETURN
        END
