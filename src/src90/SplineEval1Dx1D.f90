      SUBROUTINE SplineEval1Dx1D(iflag,u,v,f,fr,frr,ft,frt,ftt)
      USE cornea_arrays, ONLY : DiaSlope, RadSlope
      USE parameters
      USE set_precision, ONLY : wp
      USE spline_interfaces, ONLY : lsqfit, LSQEval, pspli, SplineEval, SplineEvalCenter, trapez, CubicSplineQuad
      USE special_fct, ONLY : bsearch, OPERATOR(.p.)
      USE, INTRINSIC :: ieee_arithmetic
      IMPLICIT NONE
 !    first digit iflag=1 -> use lsq iflag=0 -> use circumferential spline in second step
 !    second digit iflag=1 central node 0 = no central node
 !    third digit iflag=0 no integration; iflag=1 trapezoidal integration; iflag=2 cubic integration
      INTEGER, INTENT(IN) :: iflag
      REAL(wp), INTENT(IN) :: u, v
      REAL(wp), INTENT(OUT),OPTIONAL ::  f,fr,ft,frt,frr,ftt
      REAL(wp) :: g,g0,gr,grr,h,hr,hrr,usignd !,error
      REAL(wp) :: fTmp(size(RadSlope%r,2)),frTmp(size(RadSlope%r,2)),frrTmp(size(RadSlope%r,2))
      REAL(wp) :: thta(size(RadSlope%r,2)),fttTmp(size(RadSlope%r,2)),frttTmp(size(RadSlope%r,2)),frrttTmp(size(RadSlope%r,2))
      REAL(wp) :: r(2*size(RadSlope%r,1)),z(2*size(RadSlope%r,1)),zr2(2*size(RadSlope%r,1)),c(M2)
      INTEGER :: L2,L,MM,N,i,i1,err_report
!      LOGICAL :: IsInf

      MM=size(RadSlope%r,2)
      N=size(RadSlope%r,1)

      if(.not.Present(ft)) then                      ! if only f,fr,frr, no need for theta derivatives
       call bsearch(v,RadSlope%thta,MM,i1,i)         ! find the radial
       if ((abs(RadSlope%thta(i))-v) .le. eps) then  ! and if on a theta knot, which also means i=i1 with latest bsearch
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
        if (mod(iflag,10) == 0) then                  ! no integration load elevations
         if (mod((iflag-mod(iflag,10))/10,10) == 0) then    ! no central node
          call SplineEval(0,r,z,zr2,L2,usignd,g,gr,grr)    ! first parameter = 0 nonperiodic = 2 no extrapolation
         endif
         if (mod((iflag-mod(iflag,10))/10,10) == 1) then    ! non-periodic center node radial spline
          call SplineEvalCenter(i,r,z,zr2,L2,usignd,g,gr,grr)
         endif
         fTmp(i)=g
        else  ! integration, load derivatives
         if (mod((iflag-mod(iflag,10))/10,10) == 0) then     ! no central node
          call SplineEval(0,r,z,zr2,L2,usignd,gr,grr)    ! first parameter = 0 nonperiodic = 2 no extrapolation
         endif
         if (mod((iflag-mod(iflag,10))/10,10) == 1) then    ! non-periodic center node radial spline
          call SplineEvalCenter(i,r,z,zr2,L2,usignd,gr,grr)
         endif
         if (mod(iflag,10) == 2) then                  ! cubic integration, iflag passes centernode option
          call CubicSplineQuad(i,iflag,r,z,zr2,L2,0._wp,g0)
          call CubicSplineQuad(i,iflag,r,z,zr2,L2,usignd,g)
         endif
         if (mod(iflag,10) == 1) then                  ! trapezoidal integration, iflag passes centernode option
          call trapez(i,iflag,r,z,zr2,L2,0._wp,g0)
          call trapez(i,iflag,r,z,zr2,L2,usignd,g)
         endif
         fTmp(i)=g-g0
        endif
        if (Present(f)) then
         f=fTmp(i)
        endif
        if (Present(fr)) then
          fr=sign(gr,u)
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
         if (mod((iflag-mod(iflag,10))/10,10) == 0) then    ! no central node
          call SplineEval(0,r,z,zr2,L2,u,g,gr,grr)    ! first parameter = 0 nonperiodic
          call SplineEval(0,r,z,zr2,L2,-u,h,hr,hrr)    ! first parameter = 0 nonperiodic
         endif
         if (mod((iflag-mod(iflag,10))/10,10) == 1) then    ! non-periodic center node radial spline
          call SplineEvalCenter(i,r,z,zr2,L2,u,g,gr,grr)
          call SplineEvalCenter(i,r,z,zr2,L2,-u,h,hr,hrr)
         endif
         fTmp(i)=g
         fTmp(L)=h
        else  !iflag=1 or 2
         if (mod((iflag-mod(iflag,10))/10,10) == 0) then    ! no central node
          call SplineEval(0,r,z,zr2,L2,u,gr,grr)
          call SplineEval(0,r,z,zr2,L2,-u,hr,hrr)    ! first parameter = 0 nonperiodic
         endif
         if (mod((iflag-mod(iflag,10))/10,10) == 1) then    ! non-periodic center node radial spline
          call SplineEvalCenter(i,r,z,zr2,L2,u,gr,grr)
          call SplineEvalCenter(i,r,z,zr2,L2,-u,hr,hrr)
         endif
         if (mod(iflag,10) == 2) then                  ! cubic integration
          call CubicSplineQuad(i,iflag,r,z,zr2,L2,0._wp,g0)
          call CubicSplineQuad(i,iflag,r,z,zr2,L2,u,g)
          call CubicSplineQuad(i,iflag,r,z,zr2,L2,-u,h)
         endif
         if (mod(iflag,10) == 1) then                  ! trapezoidal integration
          call trapez(i,iflag,r,z,zr2,L2,0._wp,g0)
          call trapez(i,iflag,r,z,zr2,L2,u,g)
          call trapez(i,iflag,r,z,zr2,L2,-u,h)
         endif         
         fTmp(i)=g-g0
         fTmp(L)=h-g0
        endif      
        frTmp(i)=sign(gr,u)
        frrTmp(i)=grr
        frTmp(L)=sign(hr,u)
        frrTmp(L)=hrr
      end do
!     FIRST CALL FOR PERIODIC SPLINE/LSQ OF f0, fttTmp is d2Y/dTHETA2
      if (Present(ftt)) then
       if (mod((iflag-mod(iflag,100))/100,100) == 0) then ! spline
        call pspli(thta,fTmp,MM,fttTmp,err_report)
        call SplineEval(1,thta,fTmp,fttTmp,MM,v,f,ft,ftt) !first parameter = 1 periodic
       else   ! LSQ  
        call lsqfit(thta,fTmp,MM,M2,c)
        call LSQEval(M2,c,v,f,ft,ftt)
       endif
      else
       if (Present(ft)) then
        if (mod((iflag-mod(iflag,100))/100,100) == 0) then ! spline vs lsq
         call pspli(thta,fTmp,MM,fttTmp,err_report)
         call SplineEval(1,thta,fTmp,fttTmp,MM,v,f,ft)
        else ! LSQ
         call lsqfit(thta,fTmp,MM,M2,c)
         call LSQEval(M2,c,v,f,ft)
        endif
       else
        if (Present(f)) then
        ! We spline/lsq around the points in order to generate the derivatives; the actual function point
        ! and radial derivatives were already generated above as long as v is a knot.
         call bsearch(v,thta,MM,i1,i)
         if (abs(thta(i)-v) .le. eps) then  ! if on a theta knot, skip the spline
          f=fTmp(i)
         else
          if (mod((iflag-mod(iflag,100))/100,100) == 0) then ! spline vs lsq
           call pspli(thta,fTmp,MM,fttTmp,err_report)
           call SplineEval(1,thta,fTmp,fttTmp,MM,v,f)
          else ! LSQ
           call lsqfit(thta,fTmp,MM,M2,c)
           call LSQEval(M2,c,v,f)
          endif
         endif
        endif
       endif
      endif
!     SECOND CALL FOR PERIODIC SPLINE/LSQ OF fr (df/dR), frrtTmp is d3Y/dRdTHETA2
      if (Present(frt)) then
       if (mod((iflag-mod(iflag,100))/100,100) == 0) then ! spline vs lsq
        call pspli(thta,frTmp,MM,frttTmp,err_report)
        call SplineEval(1,thta,frTmp,frttTmp,MM,v,fr,frt)
       else
        call lsqfit(thta,frTmp,MM,M2,c)
        call LSQEval(M2,c,v,fr,frt)
       endif
      else
       if (Present(fr)) then
       ! We spline around the points in order to generate the angular spline derivatives; the actual function point
       ! and radial derivatives were already generated above as long as v is a knot.
        call bsearch(v,thta,MM,i1,i)
        if (abs(thta(i)-v) .le. eps) then  ! if on a theta knot, skip the spline
         fr=frTmp(i)
        else
         if (mod((iflag-mod(iflag,100))/100,100) == 0) then ! spline vs lsq
          call pspli(thta,frTmp,MM,frttTmp,err_report)
          call SplineEval(1,thta,frTmp,frttTmp,MM,v,fr)
         else
          call lsqfit(thta,frTmp,MM,M2,c)
          call LSQEval(M2,c,v,fr)
         endif
        endif
       endif
      endif
!     THIRD CALL FOR PERIODIC SPLINE/LSQ OF frr (d2f/dR2), frrttTmp is d4Y/dR2dTHETA2
      if (Present(frr)) then
      ! We spline/lsq around the points in order to generate the derivatives; the actual function point
      ! and radial derivatives were already generated above as long as v is a knot.
       call bsearch(v,thta,MM,i1,i)
       if (abs(thta(i)-v) .le. eps) then  ! if on a theta knot, skip the spline
        frr=frrTmp(i)
       else
        if (mod((iflag-mod(iflag,100))/100,100) == 0) then ! spline vs lsq
         call pspli(thta,frrTmp,MM,frrttTmp,err_report)
         call SplineEval(1,thta,frrTmp,frrttTmp,MM,v,frr)
        else
         call lsqfit(thta,frrTmp,MM,M2,c)
         call LSQEval(M2,c,v,frr)
        endif
       endif
      endif
      RETURN
      END
