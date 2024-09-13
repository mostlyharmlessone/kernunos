      subroutine SplineEval1Dx1D(iflag,u,v,f,fr,frr,ft,frt,ftt)
      USE cornea_arrays, ONLY : DiaSlope, RadSlope, PI, EPS
      USE set_precision, ONLY : wp
      USE spline_interfaces, ONLY : pspli, SplineEval, SplineEvalCenter, trapez, CubicSplineQuad
      USE special_fct, ONLY : bsearch, OPERATOR(.p.)
      use,intrinsic :: ieee_arithmetic
      implicit none
      integer, INTENT(IN) :: iflag     ! iflag=0 no integration; iflag=1 trapezoidal integration; iflag=2 cubic integration;
      real(wp), INTENT(IN) :: u, v
      real(wp), INTENT(OUT),OPTIONAL ::  f,fr,ft,frt,frr,ftt
      real(wp) :: g,g0,gr,grr,h,hr,hrr,error,usignd
      real(wp) :: fTmp(size(RadSlope%r,2)),frTmp(size(RadSlope%r,2)),frrTmp(size(RadSlope%r,2))
      real(wp) :: thta(size(RadSlope%r,2)),fttTmp(size(RadSlope%r,2)),frttTmp(size(RadSlope%r,2)),frrttTmp(size(RadSlope%r,2))
      real(wp) :: r(2*size(RadSlope%r,1)),z(2*size(RadSlope%r,1)),zr2(2*size(RadSlope%r,1))
      integer :: L2,j,L,MM,N,i,i1
      logical :: IsInf

      MM=size(RadSlope%r,2)
      N=size(RadSlope%r,1)

      if(.not.Present(ft)) then                      ! if only f,fr,frr, no need for theta derivatives
       call bsearch(v,RadSlope%thta,MM,i1,i)         ! find the radial
       if ((abs(RadSlope%thta(i))-v) .le. eps) then  ! and if on a theta knot
!       only 1D splining necessary
!       odd as it seems, each angle i is repeated since we're on a diagonal, and u has a sign
        usignd = u
        if (i .gt. MM/2) then
         i = i-MM/2
         usignd = -u
        endif
        L2=DiaSlope%L2(i)
        r=DiaSlope%rd(1:2*N,i)
        z=DiaSlope%Zpd(1:2*N,i)
        zr2=DiaSlope%Zpd2(1:2*N,i)
        if (mod(iflag,10) == 0) then                  ! no integration
         if (((iflag-mod(iflag,10))/10) == 0) then    ! no central node
          call SplineEval(0,r,z,zr2,L2,usignd,g,gr,grr)    ! first parameter = 0 nonperiodic
         endif
         if (((iflag-mod(iflag,10))/10) == 1) then    ! non-periodic center node radial spline
          call SplineEvalCenter(j,r,z,zr2,L2,usignd,g,gr,grr)
         endif
         fTmp(i)=g
        else  !iflag=1 or 2
         if (((iflag-mod(iflag,10))/10) == 0) then    ! no central node
          call SplineEval(0,r,z,zr2,L2,usignd,gr,grr)    ! first parameter = 0 nonperiodic
         endif
         if (((iflag-mod(iflag,10))/10) == 1) then    ! non-periodic center node radial spline
          call SplineEvalCenter(j,r,z,zr2,L2,usignd,gr,grr)
         endif
         if (mod(iflag,10) == 2) then                  ! cubic integration, iflag passes centernode option
          call CubicSplineQuad(j,iflag,r,z,zr2,L2,0._wp,g0)
          call CubicSplineQuad(j,iflag,r,z,zr2,L2,usignd,g)
         endif
         if (mod(iflag,10) == 1) then                  ! trapezoidal integration, iflag passes centernode option
          call trapez(j,iflag,r,z,zr2,L2,0._wp,g0)
          call trapez(j,iflag,r,z,zr2,L2,usignd,g)
         endif
         fTmp(i)=g-g0
        endif
        if (Present(f)) then
         f=fTmp(i)
        endif
        if (Present(fr)) then
         fr=gr
        endif
        if (Present(frr)) then
         frr=grr
        endif
        return
       endif
      endif

!     1D x 1D splining needed if ft,frt, or ftt are needed or if theta is not on a knot
!     generate all the radials at u for circumferential splining
      do i=1,MM/2
        L2=DiaSlope%L2(i)
        thta(i)=RadSlope%thta(i)
        r=DiaSlope%rd(1:2*N,i)
        z=DiaSlope%Zpd(1:2*N,i)
        zr2=DiaSlope%Zpd2(1:2*N,i)
!       odd as it seems, each angle i is also angle L since we're on a diagonal and u has a sign
        L=i+MM/2
        thta(L)=RadSlope%thta(L)
        if (mod(iflag,10) == 0) then                  ! no integration
         if (((iflag-mod(iflag,10))/10) == 0) then    ! no central node
          call SplineEval(0,r,z,zr2,L2,u,g,gr,grr)    ! first parameter = 0 nonperiodic
          call SplineEval(0,r,z,zr2,L2,-u,h,hr,hrr)    ! first parameter = 0 nonperiodic
         endif
         if (((iflag-mod(iflag,10))/10) == 1) then    ! non-periodic center node radial spline
          call SplineEvalCenter(j,r,z,zr2,L2,u,g,gr,grr)
          call SplineEvalCenter(j,r,z,zr2,L2,-u,h,hr,hrr)
         endif
         fTmp(i)=g
         fTmp(L)=h
        else  !iflag=1 or 2
         if (((iflag-mod(iflag,10))/10) == 0) then    ! no central node
         call SplineEval(0,r,z,zr2,L2,u,gr,grr)
         call SplineEval(0,r,z,zr2,L2,-u,hr,hrr)    ! first parameter = 0 nonperiodic
         endif
         if (((iflag-mod(iflag,10))/10) == 1) then    ! non-periodic center node radial spline
          call SplineEvalCenter(j,r,z,zr2,L2,u,gr,grr)
          call SplineEvalCenter(j,r,z,zr2,L2,-u,hr,hrr)
         endif
         if (mod(iflag,10) == 2) then                  ! cubic integration
          call CubicSplineQuad(j,iflag,r,z,zr2,L2,0._wp,g0)
          call CubicSplineQuad(j,iflag,r,z,zr2,L2,u,g)
          call CubicSplineQuad(j,iflag,r,z,zr2,L2,-u,h)
         endif
         if (mod(iflag,10) == 1) then                  ! trapezoidal integration
          call trapez(j,iflag,r,z,zr2,L2,0._wp,g0)
          call trapez(j,iflag,r,z,zr2,L2,u,g)
          call trapez(j,iflag,r,z,zr2,L2,-u,h)
         endif         
         fTmp(i)=g-g0
         fTmp(L)=h-g0
        endif      
        frTmp(i)=gr
        frrTmp(i)=grr
        frTmp(L)=hr
        frrTmp(L)=hrr
      end do

!     FIRST CALL FOR PERIODIC SPLINE OF f0, fttTmp is d2Y/dTHETA2
      if (Present(ftt)) then
       call pspli(thta,fTmp,MM,fttTmp)
       call SplineEval(1,thta,fTmp,fttTmp,MM,v,f,ft,ftt) !first parameter = 1 periodic
      else
       if (Present(ft)) then
        call pspli(thta,fTmp,MM,fttTmp)
        call SplineEval(1,thta,fTmp,fttTmp,MM,v,f,ft)
       else
        if (Present(f)) then
        ! We spline around the points in order to generate the angular spline derivatives; the actual function point
        ! and radial derivatives were already generated above as long as v is a knot.
         call bsearch(v,thta,MM,i1,i)
         if (abs(thta(i)-v) .le. eps) then  ! if on a theta knot, skip the spline
          f=fTmp(i)
         else
          call pspli(thta,fTmp,MM,fttTmp)
          call SplineEval(1,thta,fTmp,fttTmp,MM,v,f)
         endif
        endif
       endif
      endif
!     SECOND CALL FOR PERIODIC SPLINE OF fr (df/dR), frrtTmp is d3Y/dRdTHETA2
      if (Present(frt)) then
       call pspli(thta,frTmp,MM,frttTmp)
       call SplineEval(1,thta,frTmp,frttTmp,MM,v,fr,frt)
      else
       if (Present(fr)) then
       ! We spline around the points in order to generate the angular spline derivatives; the actual function point
       ! and radial derivatives were already generated above as long as v is a knot.
        call bsearch(v,thta,MM,i1,i)
        if (abs(thta(i)-v) .le. eps) then  ! if on a theta knot, skip the spline
         fr=frTmp(i)
        else
         call pspli(thta,frTmp,MM,frttTmp)
         call SplineEval(1,thta,frTmp,frttTmp,MM,v,fr)
        endif
       endif
      endif
!     THIRD CALL FOR PERIODIC SPLINE OF frr (d2f/dR2), frrttTmp is d4Y/dR2dTHETA2
      if (Present(frr)) then
      ! We spline around the points in order to generate the angular spline derivatives; the actual function point
      ! and radial derivatives were already generated above as long as v is a knot.
       call bsearch(v,thta,MM,i1,i)
       if (abs(thta(i)-v) .le. eps) then  ! if on a theta knot, skip the spline
        frr=frrTmp(i)
       else
        call pspli(thta,frrTmp,MM,frrttTmp)
        call SplineEval(1,thta,frrTmp,frrttTmp,MM,v,frr)
       endif
      endif
      RETURN
      END
