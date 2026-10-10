SUBROUTINE lsqfit(t,z,input_length,M2,c,err_)
USE set_precision, ONLY  :  wp
USE LapackInterface, ONLY : dgetrf, dgetrs !, dgels, GaussJordan
USE special_fct, ONLY : OPERATOR(.p.)
USE parameters, ONLY : eps
REAL(wp), INTENT(IN) :: t(input_length),z(input_length)
INTEGER, INTENT(IN) :: input_length, M2
REAL(wp), INTENT(OUT) ::c(M2)
INTEGER, INTENT(OUT) :: err_
 INTEGER :: j,k,info,ipvt(M2) !,LWORK1
 REAL(wp) :: X(M2,input_length),zpX(M2),XTX(M2,M2) !,zwork(input_length)
! REAL(wp), allocatable :: WORK1(:)
 LOGICAL :: Q
  zpX=0 ; XTX = 0
  c=0 ; X=0 ; err_ = 0
! X is terms of fourier, t are angles, z are radii for current ring
outer_loop:  do k=1,input_length
inner_loop_1:   do j=1,M2/2
    Q=(ABS(z(k)) > 0)
    if (Q) then ! means it is  =/ 0
     X(j,k)=cos((j-1)*t(k))    ! cosine series including 0 term
    else
     cycle outer_loop
    endif
   end do inner_loop_1
inner_loop_2:   do j=M2/2+1,M2
    Q=(ABS(z(k)) > 0)
    if (Q) then ! means it is  =/ 0
     X(j,k)=sin((j-M2/2)*t(k))    ! sine series
    else
     cycle outer_loop
    endif
   end do inner_loop_2
  end do outer_loop
  XTX=matmul(X,Transpose(X))
  zpX=matmul(X,z)
! get solution fit coefficients c to XTX.c=z.X
  c=0

  if ((z .p. z) .le. eps) then
   write(*,*) 'Warning degenerate LSQ, near zero input',z .p. z
   c = 0
   return
  endif

!  call GaussJordan(M2, 1, XTX, M2, zpX, M2, INFO )
!  or
!  call DGESV(M2, 1, XTX, M2, ipvt, zpX, M2, INFO )
!  or
!  LWORK1 = min(input_length,M2) + max( min(input_length,M2), 1 )
!  allocate (WORK1(LWORK1))! WORK1 is dimension LWORK1
!  call DGELS( 'T', M2, input_length, 1, X, M2, zwork , input_length, WORK1, LWORK1, INFO ) ! overwrites z (only to M2)
!  if (info .ne. 0) write(*,*) 'Error in lsqfit: dgels',info
!  c=zwork(1:M2)

  call dgetrf(M2,M2,XTX,M2,ipvt,info)
  if (info .ne. 0) then
   write(*,*) 'Error in lsqfit: dgetrf',info
   err_ = info
   write(*,*) t
   write(*,*) z
   c = 0
   return
  endif

  call dgetrs('N',M2,1,XTX,M2,ipvt,zpX,M2,info)
  if (info .ne. 0) write(*,*) 'Error in lsqfit: dgetrs',info
! deallocate(WORK1)
  c=zpX
END SUBROUTINE
