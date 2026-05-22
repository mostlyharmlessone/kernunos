!     shifts each meridional curve over by the deviation of the maximum from the origin 
!     relies on MakeRadSplineCenter
      subroutine AdjustRadSplineCenter 
      USE cornea_arrays, ONLY : DiaSlope, RadSlope, RadSplineCenter
      IMPLICIT NONE
      INTEGER :: i,j,MM,N
      MM=size(RadSlope%r,2)
      N=size(RadSlope%r,1)
      do j=1,MM/2
       do i=1,2*N
        DiaSlope%rd(i,j)=DiaSlope%rd(i,j)-RadSplineCenter(1,j)
       end do
      end do
      return
      end subroutine AdjustRadSplineCenter
