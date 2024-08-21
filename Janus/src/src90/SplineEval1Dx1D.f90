      subroutine SplineEval1Dx1D(iflag,u,v,f,fr,ft,frt,frr,ftt)
      USE cornea_arrays, ONLY : DiaSlope, RadSlope, PI
      USE set_precision, ONLY : wp
      USE spline_interfaces, ONLY : pspli, SplineEval, SplineEvalCenter, trapez, CubicSplineQuad
      USE special_fct, ONLY : OPERATOR(.p.) !tensor summation convention      
      use,intrinsic :: ieee_arithmetic
      implicit none
      integer, INTENT(IN) :: iflag     ! iflag=0 no integration; iflag=1 trapezoidal integration; iflag=2 cubic integration;
                                       ! iflag=3 integration but flagged
      real(wp), INTENT(INOUT) :: u, v
      real(wp), INTENT(OUT),OPTIONAL ::  f,fr,ft,frt,frr,ftt
      real(wp) :: g,g0,gr,grr,h,hr,hrr,error
      real(wp) :: fTmp(size(RadSlope%r,2)),frTmp(size(RadSlope%r,2)),frrTmp(size(RadSlope%r,2))
      real(wp) :: thta(size(RadSlope%r,2)),fttTmp(size(RadSlope%r,2)),frttTmp(size(RadSlope%r,2)),frrttTmp(size(RadSlope%r,2))
      real(wp) :: r(2*size(RadSlope%r,1)),z(2*size(RadSlope%r,1)),zr2(2*size(RadSlope%r,1))
      integer :: L2,j,L,MM,N
      logical :: IsInf

      MM=size(RadSlope%r,2)
      N=size(RadSlope%r,1)

      do j=1,MM/2
        L2=DiaSlope%L2(j)
        thta(j)=RadSlope%thta(j)
        r=DiaSlope%rd(1:2*N,j)
        z=DiaSlope%Zpd(1:2*N,j)
        zr2=DiaSlope%Zpd2(1:2*N,j)
!       odd as it seems, each angle j is also angle L since we're on a diagonal
        L=j+MM/2
        thta(L)=RadSlope%thta(L)

        if (mod(iflag,10) == 0) then     ! no integration
         if (((iflag-mod(iflag,10))/10) == 0) then    ! no central node
          call SplineEval(0,r,z,zr2,L2,u,g,gr,grr)  !first parameter = 0 nonperiodic
          call SplineEval(0,r,z,zr2,L2,-u,h,hr,hrr)  !first parameter = 0 nonperiodic
         endif
         if (((iflag-mod(iflag,10))/10) == 1) then    !non-periodic center node radial spline
          call SplineEvalCenter(j,r,z,zr2,L2,u,g,gr,grr)
          call SplineEvalCenter(j,r,z,zr2,L2,-u,h,hr,hrr)
         endif
         fTmp(j)=g
         fTmp(L)=h
        else  !iflag=1 or 2
         call SplineEval(0,r,z,zr2,L2,u,gr,grr)
         call SplineEval(0,r,z,zr2,L2,u,hr,hrr)
         if (mod(iflag,10) == 2) then     !cubic integration
          call CubicSplineQuad(j,iflag,r,z,zr2,L2,0._wp,g0)
          call CubicSplineQuad(j,iflag,r,z,zr2,L2,u,g)
          call CubicSplineQuad(j,iflag,r,z,zr2,L2,-u,h)
         endif
         if (mod(iflag,10) == 1) then   !trapezoidal integration
          call trapez(j,iflag,r,z,zr2,L2,0._wp,g0)
          call trapez(j,iflag,r,z,zr2,L2,u,g)
          call trapez(j,iflag,r,z,zr2,L2,-u,h)
         endif         
         fTmp(j)=g-g0
         fTmp(L)=h-g0
        endif      
        frTmp(j)=gr
        frrTmp(j)=grr 
        frTmp(L)=hr
        frrTmp(L)=hrr
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
