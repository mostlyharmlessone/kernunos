SUBROUTINE lsqfit(t,z,M1,M2,c)
USE set_precision, ONLY  :  wp
use LapackInterface, ONLY : dgetrf, dgetrs !, dgels, GaussJordan
REAL(wp), INTENT(IN) :: t(M1),z(M1)
INTEGER, INTENT(IN) :: M1,M2
REAL(wp), INTENT(OUT) ::c(M2)
 INTEGER :: M1,j,k,info,ipvt(M2) !,LWORK1
 REAL(wp) :: X(M2,M1),zpX(M2),XTX(M2,M2) !,zwork(M1)
! REAL(wp), allocatable :: WORK1(:)
 LOGICAL :: Q
  zpX=0 ; XTX = 0
  c=0 ; X=0
! X is terms of fourier, t are angles, z are radii for current ring
  do k=1,M1
   do j=1,M2/2
    Q=(ABS(z(k)) > 0)
    if (Q) then ! means it is  =/ 0
     X(j,k)=cos((j-1)*t(k))    ! cosine series including 0 term
    endif
   end do
   do j=M2/2+1,M2
    Q=(ABS(z(k)) > 0)
    if (Q) then ! means it is  =/ 0
     X(j,k)=sin((j-M2/2)*t(k))    ! sine series
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
!  allocate (WORK1(LWORK1))! WORK1 is dimension LWORK1
!  call DGELS( 'T', M2, M1, 1, X, M2, zwork , M1, WORK1, LWORK1, INFO ) ! overwrites z (only to M2)
!  if (info .ne. 0) write(*,*) 'Error in lsqfit: dgels',info
!  c=zwork(1:M2)
  call dgetrf(M2,M2,XTX,M2,ipvt,info)
  if (info .ne. 0) then
   write(*,*) 'Error in lsqfit: dgetrf',info
   write(*,*) t
   write(*,*) z
   stop
  endif
  call dgetrs('N',M2,1,XTX,M2,ipvt,zpX,M2,info)
  if (info .ne. 0) write(*,*) 'Error in lsqfit: dgetrs',info
! deallocate(WORK1)
  c=zpX
END SUBROUTINE
