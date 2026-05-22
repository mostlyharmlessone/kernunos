!     diagnostic to see where each splines center is, perhaps a measure of decentration 
      subroutine MakeRadSplineCenter(dat,error_report)
      use, INTRINSIC :: iso_c_binding, ONLY : c_int, c_int64_t
      USE cornea_arrays, ONLY : DiaSlope, RadSlope, RadSplineCenter
      USE set_precision, ONLY : wp
      USE spline_interfaces, ONLY : SplineCenter
      IMPLICIT NONE
      INTEGER(c_int64_t), INTENT(IN) :: dat
      INTEGER(c_int), INTENT(OUT) :: error_report
      REAL(wp) :: r(2*size(RadSlope%r,1)),z(2*size(RadSlope%r,1)),zr2(2*size(RadSlope%r,1)),w
      INTEGER :: L2,j,L,MM,N
      INTEGER(c_int) :: err_report
      err_report = 0
      MM=size(RadSlope%r,2)
      N=size(RadSlope%r,1)
      do j=1,MM/2 
        L2=DiaSlope%L2(j)
        r=DiaSlope%rd(1:2*N,j)
        z=DiaSlope%Zpd(1:2*N,j)
        zr2=DiaSlope%Zpd2(1:2*N,j)
        call SplineCenter(dat,j,r,z,zr2,L2,w,err_report) !each call can potentionally have a call to read RadSplineCenter(:,j)
                                              !   if btest(dat,0) = .true., needs call to DiaSplineCenter=.nc.->nsplinecenter first
        if (err_report .ne. 0) then
!         write(*,*) 'Error in MakeRadSplineCenter'
          error_report = -1
        endif
        RadSplineCenter(1,j)=w
!       odd as it seems, each angle j is also angle L since we're on a diagonal 
        L=j+MM/2
        RadSplineCenter(1,L)=RadSplineCenter(1,j)
      end do
      return
      end subroutine MakeRadSplineCenter
