subroutine lsqfit(t,z,M1,M2,c)
use set_precision, only :  wp
use LapackInterface, ONLY : dgetrf, dgetrs !, dgels, GaussJordan
REAL(wp), intent(in) :: t(M1),z(M1)
INTEGER, intent(in) :: M1,M2
REAL(wp), intent(out) ::c(M2)
 integer :: M1,j,k,info,ipvt(M2)
 real(wp) :: X(M2,M1),zpX(M2),XTX(M2,M2)
 logical :: Q
  zpX=0 ; XTX = 0
  c=0 ; X=0
! X is cosine terms of fourier, t are angles, z are radii for current ring
  do j=1,M2
   do k=1,M1
    Q=(ABS(z(k)) > 0)
    if (Q) then ! means it is  =/ 0
     X(j,k)=cos((j-1)*t(k))    ! cosine series including 0 term
    endif
   end do
  end do
  XTX=matmul(X,Transpose(X))
  zpX=matmul(X,z)
! get solution fit coefficients c to XTX.c=z.X
  c=0
!  call GaussJordan(M2, 1, XTX, M2, zpX, M2, INFO )
!  or
!  call DGESV(M2, 1, XTX, M2, ipvt, zpX, M2, INFO )
!  or
!  LWORK1 = min(M1,M2) + max( min(M1,M2), 1 )
!  allocate (WORK1(LWORK1))! WORK is dimension LWORK
!  call DGELS( 'T', M2, M1, 1, X, M2, z , M1, WORK1, LWORK1, INFO ) ! overwrites z (only to M2)
  call dgetrf(M2,M2,XTX,M2,ipvt,info)
  if (info .ne. 0) write(*,*) 'Error in lsqfit: dgetrf'
  call dgetrs('N',M2,1,XTX,M2,ipvt,zpX,M2,info)
  if (info .ne. 0) write(*,*) 'Error in lsqfit: dgetrs'
!deallocate(WORK1)
  c=zpX
end subroutine
