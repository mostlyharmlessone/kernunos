MODULE cornea_arrays
! defines arrays and functions ued for corneal topography
 USE set_precision, ONLY : wp, sk, int3d
 USE LapackInterface, ONLY : dgetrf, dgetrs, dgesv, dsyev
 USE spline_interfaces 
 use, intrinsic ::  ieee_arithmetic
 use, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char
 REAL(wp), PARAMETER :: PI=3.1415926535897932384626433832795_wp
 REAL(wp), PARAMETER :: RFCT=33750.0_wp
 REAL(wp), PARAMETER :: EPS=0.0001_wp  ! used in pspli,SplineCenter,corneal calc fcts
! INTEGER, PARAMETER :: NP=141         ! PentaCam
! INTEGER, PARAMETER :: MM=180, N=22   ! Atlas
! INTEGER, PARAMETER :: MM=360, N=16  ! EyeSys
 integer, PARAMETER :: M2=3 ! lsq fourier cosine series terms
! integer :: LWORK1
! real(wp), allocatable :: WORK1(:)

! Defining common data arrays
 
 TYPE wpEyeSysMatrix
!  RA, XX are undocumented but assumed to compute to powers and radii using Zfct, there are 360 rows
   REAL (wp), ALLOCATABLE :: RA(:,:), XX(:,:),PU(:)
   INTEGER, ALLOCATABLE :: DEG(:)
   REAL (wp) :: Pupil_Center(2)
 END TYPE wpEyeSysMatrix
 
 TYPE wpRadSlopeMatrix
!  theta, r are polar coordinates, Z,Zp,Zp2,Zt2 are elevation, slope, and second derivatives
   REAL (wp), ALLOCATABLE :: thta(:), r(:,:), Z(:,:), Zp(:,:), Zp2(:,:), Zt2(:,:)
   INTEGER, ALLOCATABLE :: MV(:)
 END TYPE wpRadSlopeMatrix
 
 TYPE wpAtlasMatrix
!  AR radius (polar coord), AD "distance?",AP is axial(sagittal) power in diopters ,AY elevation, DEG polar coord
   REAL (wp), ALLOCATABLE :: AR(:,:),AD(:,:),AP(:,:),AY(:,:),DEG(:),PU(:,:)
   REAL (wp) :: Pupil_Center(2)
 END TYPE wpAtlasMatrix

 TYPE wpJMatrix
!  Computed results: R is from make rings or Penta version; need to declare one of each of these for each data set for comparison
!  each array except for R,THT, is (N+1,MM) to include values at each ring and also at center RC==RadSplineCenter pseudo ring
!  each matching name has the value at origin, min value and max value
   REAL (wp), ALLOCATABLE :: R(:,:),Z(:,:),THT(:),SAGC(:,:),INSTC(:,:),INSTC2(:,:),MEANC(:,:),MONGEA(:,:),Warp(:,:)
   REAL (wp), ALLOCATABLE :: RC(:,:),YPR(:,:),YPTHETA(:,:),PU(:)  ! RC is RadSplineCenter, compare to R0
   INTEGER, ALLOCATABLE :: MV(:)
   REAL (wp) :: R0,THT0,Z0(3),SAGC0(3),INSTC0(3),INSTC20(3),MEANC0(3),MONGEA0(3),Warp0(3),Pupil_Center(2)
   ! 12 up to 15 zernike coordinates
   REAL(wp),ALLOCATABLE :: ZC(:,:,:)
   REAL(wp) :: ZC0(3,15) !origin,min,max for each
 END TYPE wpJMatrix

 TYPE wpPentaMatrix
!  DAT is sagittal/axial curvature or elevation in mm, on 141x141 grid of -7.00 mm to +7.00 mm, no data=-1 or 0
   REAL (wp), ALLOCATABLE :: DAT(:,:),PU(:,:)
   REAL (wp) :: Pupil_Center(2)
 END TYPE wpPentaMatrix

 TYPE wpSkyline
!  DAT is sagittal/axial curvature or elevation in mm, on 141x141 grid of -7.00 mm to +7.00 mm, no data=-1 or 0
   REAL (wp), ALLOCATABLE :: DAT(:,:),x(:,:),y(:,:),z2DAT(:,:)
   INTEGER, ALLOCATABLE :: L2x(:),L2y(:),index_col(:)
   INTEGER :: rows,cols,first_row   ! skyline needed rows and columns
 END TYPE wpSkyline
 
 TYPE wpDiaSlopeMatrix
!  rd is the radius positive and negative along the diagonal, rearranged from RadSlope above
   REAL (wp), ALLOCATABLE :: rd(:,:), Zd(:,:), Zpd(:,:), Zpd2(:,:)
   REAL (wp), ALLOCATABLE :: rOutMin(:),rInMin(:),rOutMax(:),rInMax(:)
   INTEGER, ALLOCATABLE :: L2(:)
 END TYPE wpDiaSlopeMatrix

 TYPE wpsplinevect
   REAL (wp), ALLOCATABLE :: r(:),z(:),zp2(:)
   INTEGER, ALLOCATABLE :: mvjr(:)
 END TYPE wpsplinevect 
 
! all subroutines not in a module need an explicit INTERFACE section below 

! overloading of assignments for operations, defining operators

INTERFACE ASSIGNMENT (=)  
!Type(FirstArg) = Type(SecondArg) converts/populates to rhs to lhs
 MODULE PROCEDURE Atlas_eq_RadSlope
 MODULE PROCEDURE Skyline_eq_Penta
 MODULE PROCEDURE RadSlope_eq_EyeSys
 MODULE PROCEDURE RadSlope_eq_Atlas
 MODULE PROCEDURE DiaSlope_eq_RadSlope
 MODULE PROCEDURE RadSlope_eq_DiaSlope
 MODULE PROCEDURE RadSlope_eq_JMatrix
 ! Type(oneofthesebelow) = INTEGER(0) deallocates the matrix
 MODULE PROCEDURE destroy_EyeSys
 MODULE PROCEDURE destroy_Penta
 MODULE PROCEDURE destroy_Atlas
 MODULE PROCEDURE destroy_RadSlope
 MODULE PROCEDURE destroy_DiaSlope
 MODULE PROCEDURE destroy_Skyline
 MODULE PROCEDURE destroy_JMatrix
END INTERFACE

INTERFACE OPERATOR (.n.) ! unary operator
! .n. TypeDiaSlopeMatrix populates the matrix with second radial derivatives of z
 MODULE PROCEDURE DiaSpline ! uses nspline.f90
END INTERFACE 
INTERFACE OPERATOR (.nc.) ! unary operator
! .n. TypeDiaSlopeMatrix populates the matrix with second radial derivatives of z
 MODULE PROCEDURE DiaSplineCenter ! uses nspline.f90
END INTERFACE

! declaring common global data arrays
 real(wp), allocatable :: RadSplineCenter(:,:)
 TYPE(wpJMatrix) :: JMatrix,JMatrix1,JMatrix2,JMatrix3
 TYPE(wpEyeSysMatrix) :: EyeSys
 TYPE(wpRadSlopeMatrix) :: RadSlope
 TYPE(wpAtlasMatrix) :: Atlas
 TYPE(wpAtlasMatrix) :: AtlasSave
 TYPE(wpPentaMatrix) :: Penta
 TYPE(wpSkyline) :: Skyline
 TYPE(wpDiaSlopeMatrix) :: DiaSlope

 CONTAINS
 
subroutine init_mat_Penta(NP,Penta,Skyline) ! allocate PentaCam arrays
  INTEGER, INTENT(IN) :: NP
  INTEGER :: ERROR
  CHARACTER :: ERR_MSG
  TYPE(wpPentaMatrix) :: Penta 
  TYPE(wpSkyline) :: Skyline  
  allocate (Penta%DAT(NP,NP),Penta%PU(256,2), STAT=ERROR, ERRMSG=ERR_MSG)
  if (ERROR .NE. 0) then 
   write(*,*) 'Allocation error: ',ERROR,ERR_MSG
   return
  endif 
  Penta%DAT(:,:)=0 ; Penta%PU(:,:)=0
  allocate (Skyline%DAT(NP,NP),Skyline%x(NP,NP),&
            Skyline%z2DAT(NP,NP),Skyline%L2x(NP),Skyline%L2y(NP),&
            Skyline%index_col(NP), STAT=ERROR, ERRMSG=ERR_MSG)
  if (ERROR .NE. 0) then 
   write(*,*) 'Allocation error: ',ERROR,ERR_MSG
   return
  endif
  Skyline%DAT(:,:)=0
  Skyline%x(:,:)=0
  Skyline%z2DAT(:,:)=0
  Skyline%L2x(:)=0
  Skyline%L2y(:)=0
  Skyline%index_col(:)=0
end subroutine init_mat_Penta

subroutine init_mat_JMatrix(MM,N,b) ! allocate common storage arrays
  INTEGER, INTENT(IN) :: MM,N
  TYPE(wpJMatrix) :: b
   allocate (b%R(N,MM),b%Z(N+1,MM),b%THT(MM),b%YPR(N,MM),b%YPTHETA(N,MM),b%SAGC(N+1,MM),&
            b%INSTC(N+1,MM),b%INSTC2(N+1,MM),b%MEANC(N+1,MM),b%MONGEA(N+1,MM))
   allocate (b%MV(MM),b%RC(3,MM),b%Warp(N+1,MM),b%PU(MM))
   b%R(:,:)=0 ; b%Z(:,:)=0 ; b%THT(:)=0 ; b%YPR(:,:)=0 ; b%YPTHETA(:,:)=0 ; b%SAGC(:,:)=0
   b%INSTC(:,:)=0 ; b%INSTC2(:,:)=0 ; b%MEANC(:,:)=0 ; b%MONGEA(:,:)=0 ;  b%Warp(:,:)=0
   b%MV(:)=0 ; b%RC(:,:)=0 ; b%PU(:)=0; b%Pupil_Center(:)=0
   allocate (b%ZC(N+1,MM,15))
   b%ZC(:,:,:)=0
end subroutine init_mat_JMatrix

subroutine init_mat_EyeSys(MM,N,EyeSys) ! allocate EyeSys arrays
  INTEGER, INTENT(IN) :: MM,N
  TYPE(wpEyeSysMatrix) :: EyeSys
  allocate (EyeSys%RA(MM,N),EyeSys%XX(MM,N),EyeSys%PU(MM),EyeSys%DEG(MM))
  EyeSys%RA(:,:)=0 ; EyeSys%XX(:,:)=0 ; EyeSys%PU(:)=0
  EyeSys%DEG(:)=0 ; EyeSys%Pupil_Center=0
end subroutine init_mat_EyeSys

subroutine init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter) ! allocate common arrays
  INTEGER, INTENT(IN) :: MM,N
  real(wp), allocatable :: RadSplineCenter(:,:)
  TYPE(wpRadSlopeMatrix) :: RadSlope  
  TYPE(wpDiaSlopeMatrix) :: DiaSlope
  allocate (RadSlope%r(N,MM),RadSlope%Z(N,MM),RadSlope%Zp(N,MM),RadSlope%Zp2(N,MM),&
            Radslope%Zt2(N,MM),RadSlope%thta(MM),RadSlope%MV(MM))  
  allocate (DiaSlope%rd(2*N,MM/2),DiaSlope%Zd(2*N,MM/2),DiaSlope%Zpd(2*N,MM/2),&
            DiaSlope%Zpd2(2*N,MM/2),DiaSlope%L2(MM/2))
  allocate (DiaSlope%rOutMax(MM/2),DiaSlope%rInMax(MM/2),&
            DiaSlope%rOutMin(MM/2),DiaSlope%rInMin(MM/2))
  allocate (RadSplineCenter(3,MM))
  RadSlope%r(:,:)=0 ; RadSlope%Z(:,:)=0 ; RadSlope%Zp(:,:)=0 ; RadSlope%Zp2(:,:)=0
  Radslope%Zt2(:,:)=0 ; RadSlope%thta(:)=0 ; RadSlope%MV(:)=0
  DiaSlope%rd(:,:)=0 ; DiaSlope%Zd(:,:)=0 ; DiaSlope%Zpd(:,:)=0
  DiaSlope%Zpd2(:,:)=0 ; DiaSlope%L2(:)=0
  DiaSlope%rOutMax(:)=0 ; DiaSlope%rInMax(:)=0
  DiaSlope%rOutMin(:)=0 ; DiaSlope%rInMin(:)=0
  RadSplineCenter(:,:)=0
end subroutine init_mat

subroutine init_mat_Atlas(MM,N,Atlas) ! allocate common arrays
  INTEGER, INTENT(IN) :: MM,N
  TYPE(wpAtlasMatrix) :: Atlas
  allocate (Atlas%AR(MM,N),Atlas%AD(MM,N),Atlas%AP(MM,N),&
            Atlas%AY(MM,N),Atlas%DEG(MM),Atlas%PU(MM,2))
  Atlas%AR(:,:)=0 ; Atlas%AD(:,:)=0 ; Atlas%AP(:,:)=0 ; Atlas%PU(:,:)=0
  Atlas%AY(:,:)=0 ; Atlas%DEG(:)=0 ; Atlas%Pupil_Center=0
end subroutine init_mat_Atlas

! Type()=0 deallocates storage

subroutine destroy_EyeSys(EyeSys,iflag)
  TYPE(wpEyeSysMatrix), INTENT(INOUT) :: EyeSys
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
   deallocate (EyeSys%RA,EyeSys%XX,EyeSys%PU,EyeSys%DEG)
  ENDIF
end subroutine destroy_EyeSys

subroutine destroy_Atlas(Atlas,iflag)
  TYPE(wpAtlasMatrix), INTENT(INOUT) :: Atlas
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
   deallocate (Atlas%AR,Atlas%AD,Atlas%AP,Atlas%AY,Atlas%DEG,Atlas%PU)
  ENDIF
end subroutine destroy_Atlas

subroutine destroy_JMatrix(JMatrix,iflag)
  TYPE(wpJMatrix), INTENT(INOUT) :: JMatrix
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
   deallocate (JMatrix%R,JMatrix%Z,JMatrix%THT,JMatrix%SAGC,JMatrix%INSTC,JMatrix%INSTC2,&
              JMatrix%MEANC,JMatrix%MONGEA,JMatrix%MV,JMatrix%RC,JMatrix%ZC)
  ENDIF
end subroutine destroy_JMatrix

subroutine destroy_RadSlope(RadSlope,iflag)
  TYPE(wpRadSlopeMatrix), INTENT(INOUT) :: RadSlope
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
   deallocate (RadSlope%r,RadSlope%Z,RadSlope%Zp,RadSlope%Zp2,&
            Radslope%Zt2,RadSlope%thta,RadSlope%MV)
  ENDIF
end subroutine destroy_RadSlope

subroutine destroy_DiaSlope(DiaSlope,iflag)
  TYPE(wpDiaSlopeMatrix), INTENT(INOUT) :: DiaSlope
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (DiaSlope%rd,DiaSlope%Zd,DiaSlope%Zpd,DiaSlope%Zpd2,DiaSlope%L2,&
              DiaSlope%rOutMax,DiaSlope%rInMax,DiaSlope%rOutMin,DiaSlope%rInMin)
  ENDIF
end subroutine destroy_DiaSlope

subroutine destroy_Penta(Penta,iflag)
  TYPE(wpPentaMatrix), INTENT(INOUT) :: Penta
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (Penta%DAT,Penta%PU)
  ENDIF
end subroutine destroy_Penta

subroutine destroy_Skyline(Skyline,iflag)
  TYPE(wpSkyline), INTENT(INOUT) :: Skyline
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (Skyline%DAT,Skyline%x,Skyline%z2DAT,&
              Skyline%L2x,Skyline%L2y,Skyline%index_col)
  ENDIF
end subroutine destroy_Skyline
!!array conversion routines

! Skyline strategy saves 33% storage space and computing time vs straight grid
subroutine Skyline_eq_Penta(Skyline,Penta)  ! Arrange data Skyline, that will allow loading into SplineEval                                            
  TYPE(wpSkyline), INTENT(INOUT) :: Skyline                ! x,f(x) knots, number of knots(length) and u (test point)
  TYPE(wpPentaMatrix), INTENT(IN) :: Penta              ! This is the equivalent of DiaSlope=RadSlope
  Integer :: i,j,NP,ii,jj                   ! skyline x by rows, generate y using indices later
  Integer :: first_row,last_row,col(size(Penta%DAT,1)),index_row(size(Penta%DAT,1))                                
  Integer :: first_col,last_col,row(size(Penta%DAT,1)),index_col(size(Penta%DAT,1))
  NP=size(Penta%DAT,1)
! Find edges of data, Penta "data" is assumed contiguous and simply connected
  index_row=0 ; col=0 ; Skyline%cols=0 ; first_row=0 ; last_row=141  
  index_col=0 ; row=0 ; Skyline%rows=0 ; first_col=0 ; last_col=141
  Skyline%x=0 ; Skyline%DAT=0 ; Skyline%L2x=0 ; Skyline%L2y=0
  do i=1,NP
   do j=1,NP
    if (Penta%DAT(i,j) > 0) then 
     col(i)=col(i)+1                 ! count number of nonnegative (columns) entries (data points) in row (i)
     if ( index_row(i) < 1 ) then
      index_row(i)=j                 ! remember starting point on col(i), assumes no holes in data
     endif
     if (first_row < 1) then         ! remember first_row
       first_row=i
     endif
    endif
!   For calculating L2y, transpose the matrix
    if (Penta%DAT(j,i) > 0) then
     row(i)=row(i)+1                 ! count number of nonnegative (rows) entries (data points) in column (j)
     if ( index_col(i) < 1 ) then
      index_col(i)=j                 ! remember starting point on row(j), assumes no holes in data
     endif
     if (first_col < 1) then         ! remember first_col
       first_col=i
     endif
    endif
   end do
   if (last_row .eq. NP .and. first_row > 0 .and. col(i) .eq. 0) then  ! remember last row, this assumes contiguous data
    last_row=i-1
   endif
  end do
! Write Skyline
  do i=1,last_row-first_row+1             ! number of rows
   do j=1,col(first_row+i-1)              ! number of columns col(first_row+i-1) for that row
!   skyline dat i,j Penta ii jj
!   starting column jj = index_row(first_row+i-1)
!   row ii = first_row+i-1
    ii=first_row+i-1
    jj=index_row(first_row+i-1)+j-1
    Skyline%x(i,j)=-7.00+((jj-1)*14.00)/(NP-1.0)   
    Skyline%DAT(i,j)=Penta%DAT(ii,jj)
!   Count rows in each column
    Skyline%L2y(j)=row(first_col+j-1)              ! number of rows == length of each splining vector
    Skyline%index_col(j)=index_col(first_col+j-1)  ! Skyline these for border calculation
    if (Skyline%L2y(j) > Skyline%rows) then        ! Skyline%rows is the maximum length of L2y
     Skyline%rows=Skyline%L2y(j)
    endif
   end do
!  Count columns in each row
   Skyline%L2x(i)=col(first_row+i-1)         ! number of columns == length of each splining vector
   if (Skyline%L2x(i) > Skyline%cols) then   ! Skyline%cols is the maximum length of L2x
    Skyline%cols=Skyline%L2x(i)
   endif
  end do
  Skyline%first_row=first_row                         ! needed for offset
  if  ( Skyline%rows .ne. last_row-first_row+1 ) then
   write(*,*) 'Inconsistent row count in Skyline',Skyline%rows,last_row-first_row+1 ! numbers of rows should be maximum length of columns
   return
  endif
!  ii = 0 ; jj= 0
!  do i=1,NP
!   ii=ii+row(i)
!   jj=jj+col(i) 
!  end do
!  write (*,*) 'Total number vertices in Skyline: ',ii,jj
end subroutine Skyline_eq_Penta

subroutine RadSlope_eq_Skyline(JMatrix, RadSlope, Skyline, Penta)      ! initially populates JMatrix & RadSlope with splining
  TYPE(wpSkyline), INTENT(INOUT) :: Skyline                                             
  TYPE(wpPentaMatrix), INTENT(IN) :: Penta
  TYPE(wpRadSlopeMatrix), INTENT(INOUT) :: RadSlope
  TYPE(wpJMatrix), INTENT(INOUT) :: JMatrix  
  integer :: M1,N1,i,j,k,kk,L2,offset,NP,ITH
  integer :: imv(size(JMatrix%Z,2))
  real(wp) :: rBo,rBi,DAT,u,v,xx,yy,f,fTmp(Skyline%rows),f2Tmp(Skyline%rows)
  real(wp) :: r(size(RadSlope%r,2)),z(Skyline%cols),z2(Skyline%cols)
  real(wp) :: x(Skyline%cols)              ! maximum size needed, don't need NP
  real(wp) :: y(Skyline%rows)
  real(wp) :: rmin,check,firstcheck,secondcheck
  M1=size(RadSlope%r,2)
  N1=size(RadSlope%r,1)
  NP=size(Skyline%DAT,1)                                                   
  imv=0
  rmin=1E30
! Spline in x                               (this is the equivalent of DiaSpline)
  do i=1,Skyline%rows
   L2=Skyline%L2x(i)                                       
   x(1:L2)=Skyline%x(i,1:L2)
   z(1:L2)=Skyline%DAT(i,1:L2)
   call nspline(x,z,L2,z2)                                ! generate zxDAT 
   Skyline%z2DAT(i,1:L2)=z2(1:L2)
  end do
! make rings
! scale in 14x 14 mm of Penta matrix 141x141 divided by 2
  rBo=7.0                                   ! try to make radius at least out to 7 (theoretical max on PentaCam)
  rBi=0.05*rBo                              ! donut 
  JMatrix%SAGC0(2)=1E30                     ! bound setting
  JMatrix%SAGC0(3)=-1E30
  JMatrix%Z0(2)=1E30
  JMatrix%Z0(3)=-1E30  
  do i=1,M1
   ITH=2*(i-1)                             ! every 2 degrees
   RadSlope%thta(i)=PI*ITH/180.0_wp   
   do j=1,N1+1                             ! include center point
    if (j > N1) then
     u=0 ; v=0
    else
     r(j)=(j-1)*(rBo-rBi)/(N1-1)+rBi  
     u=r(j)*COS(RadSlope%thta(i))        ! x,y coordinates of ring point
     v=r(j)*SIN(RadSlope%thta(i))
    endif
!    Populate JMatrix with Splined PentaCam
!    Spline in y      (this is the equivalent of Spline1Dx1D)
     firstcheck=0
     do k=1,Skyline%rows
      L2=Skyline%L2x(k)
      x(1:L2)=Skyline%x(k,1:L2)
      z(1:L2)=Skyline%DAT(k,1:L2)
      z2(1:L2)=Skyline%z2DAT(k,1:L2)
!     f is value at u, fTmp is a new 1:(Skyline%rows) column of values at u
      call SplineEval(0,x(1:L2),z(1:L2),z2(1:L2),L2,u,f)   ! first parameter = 0 nonperiodic
      call SplineEval(2,x(1:L2),z(1:L2),z2(1:L2),L2,u,check)   ! first parameter = 2 extrapolation check
      if (check == 0) firstcheck=firstcheck+1 ! how much extrapolation
      fTmp(k)=f
     end do
     offset=Skyline%first_row-1
     L2=Skyline%rows       ! this L2 will introduce bogus values at the end of the splines needing trimming
     do kk=1,L2            ! fTmp has to align with y; but ftmp starts at Skyline%first_row, y starts at 1 for calculation
      y(kk)=7.00-((kk-1+offset)*14.00)/(NP-1.0)
     end do
     call nspline(y(1:L2),fTmp(1:L2),L2,f2Tmp(1:L2))             ! spline in Y
     call SplineEval(0,y(1:L2),fTmp(1:L2),f2Tmp(1:L2),L2,v,DAT)  ! first parameter = 0 nonperiodic
     call SplineEval(2,y(1:L2),fTmp(1:L2),f2Tmp(1:L2),L2,v,secondcheck)  ! first parameter = 2 extrapolation check
     if (j > N1) then
      JMatrix%R0=0 ; JMatrix%THT0=0
      if (ABS(DAT) > 0) then
       JMatrix%SAGC0(1)=RFCT/(100.0*ABS(DAT))  ! if curvatures
      endif
       JMatrix%Z0(1)=ABS(DAT)                  ! if elevation
     else
!   boundary check here
     xx=u*(NP-1)/14.0 ; yy=v*(NP-1)/14.0
     if (Penta%DAT(1+(NP-1)/2+sign(floor(ABS(xx)),floor(xx)),&
                &1+(NP-1)/2+sign(floor(ABS(yy)),floor(yy))) > 0) then  ! test for >0 is why Penta needed here
      if (firstcheck < 70 .and. secondcheck /= 0) then ! better extrapolation check; the 70 here is arbitrary, 40 is getting too low
       imv(i)=imv(i)+1
       if (ABS(DAT) > 0 ) then
        RadSlope%Z(j,i)=ABS(DAT)/10.0              ! if DAT is elevation
        CALL ZFCT(M1,i,100.0*ABS(r(j)),100.0*ABS(DAT),RadSlope%R(j,i),RadSlope%Zp(j,i))  !only for DAT is curvature/SAGC
       endif
       JMatrix%Z(j,i)=RadSlope%Z(j,i)   !only for elevations, put in RadSlope%Zp(j,i) in janus
      else  ! outside boundary
       if (r(j) < rmin) then
        rmin=r(j)
       endif
      endif
     endif
    endif
   end do !j to N1
  end do !i to M1
!  write(*,*) 'rmin from RadSlope_eq_Skyline',rmin
!  write(*,*) imv(:)
! trim down imv for r > rmin
  do i=1,M1
   if ((imv(i)-1)*(rBo-rBi)/(N1-1)+rBi .gt. rmin) imv(i)=int(1+(N1-1)*(rmin-rBi)/(rBo-rBi))
  end do
  RadSlope%MV(:)=imv(:)  ! save boundary
end subroutine RadSlope_eq_Skyline

! uses ZFCT converts lhs to rhs
subroutine RadSlope_eq_EyeSys(RadSlope,EyeSys) ! initially populates r, thta, Zp, MV
  TYPE(wpEyeSysMatrix), INTENT(INOUT) :: EyeSys
  TYPE(wpRadSlopeMatrix), INTENT(INOUT) :: RadSlope
  INTEGER :: i,j,MM,N
  integer :: imv(size(RadSlope%r,2))
  REAL(wp) :: ZIX,ZJX,YA3,X2A1
  MM=size(RadSlope%r,2)
  N=size(RadSlope%r,1)
  do i=1,MM
    imv(i)=0
    RadSlope%thta(i)=PI*EyeSys%DEG(i)/180.0_wp  ! RadSlope%thta(i)=PI*(i-1)/180.0_wp should always be true for EyeSys
      do j=1,N
       ZIX=EyeSys%XX(i,j)
       ZJX=EyeSys%RA(i,j)
       if (ZIX > 0 .AND. ZJX > 0) then
        imv(i)=imv(i)+1
        call ZFCT(MM,i,ZJX,ZIX,X2A1,YA3)
        RadSlope%Zp(imv(i),i)=YA3
        RadSlope%r(imv(i),i)=X2A1
        else
        RadSlope%Zp(j,i)=0._wp  ! sets border
       endif
        RadSlope%Zp2(j,i)=1/803.0_wp ! nonzero fallback value before splining for Atlas=RadSlope      
      end do
      RadSlope%MV(i)=imv(i)
   end do
end subroutine RadSlope_eq_EyeSys

subroutine RadSlope_eq_JMatrix(RadSlope,JMatrix) 
  TYPE(wpRadSlopeMatrix), INTENT(INOUT) :: RadSlope
  TYPE(wpJMatrix), INTENT(INOUT) :: JMatrix
  REAL(wp) :: ZIX,YA3,X2A1
  INTEGER :: i,j,MM
    MM=size(RadSlope%r,2)
    RadSlope%thta(:)=JMatrix%THT(:)
    RadSlope%MV(:)=JMatrix%MV(:)
    do i=1,MM
     do j=1,JMatrix%MV(i)
      if (ABS(JMatrix%SAGC(j,i)) > 0) then
       ZIX=RFCT/JMatrix%SAGC(j,i)
       if (ZIX > ABS(JMatrix%R(j,i)) ) then
        call ZFCT(MM,i,ABS(JMatrix%R(j,i)),ZIX,X2A1,YA3)
       else
        RadSlope%r(j,i)=JMatrix%R(j,i)
        RadSlope%Zp(j,i)=0._wp  ! sets border
        RadSlope%Z(j,i)=JMatrix%Z(j,i)
        RadSlope%Zp2(j,i)=1/803.0_wp ! fallback value before splining
        cycle
       endif
       RadSlope%r(j,i)=X2A1
       RadSlope%Zp(j,i)=YA3
       RadSlope%Z(j,i)=JMatrix%Z(j,i)
       RadSlope%Zp2(j,i)=1/803.0_wp ! fallback value before splining
      else
       RadSlope%r(j,i)=JMatrix%R(j,i)
       RadSlope%Zp(j,i)=0._wp  ! sets border
       RadSlope%Z(j,i)=JMatrix%Z(j,i)
       RadSlope%Zp2(j,i)=1/803.0_wp ! fallback value before splining
       cycle      
      endif       
     end do
    end do
end subroutine RadSlope_eq_JMatrix

! aka SLOPE2POWER using AXIALP converts lhs to rhs
subroutine Atlas_eq_RadSlope(Atlas,RadSlope)
  TYPE(wpRadSlopeMatrix), INTENT(INOUT) :: RadSlope
  TYPE(wpAtlasMatrix), INTENT(INOUT) :: Atlas
  INTEGER :: i,j,MM,N
  REAL(wp) :: X2,YP,Y2X,POW
  imv=0
  Atlas%AP=0._wp
  MM=size(RadSlope%r,2)
  N=size(RadSlope%r,1)
  do i=1,MM
     Atlas%DEG(i)=180.0_wp*RadSlope%thta(i)/PI
      do j=1,N
      X2=RadSlope%r(j,i)
      YP=RadSlope%Zp(j,i)
      Y2X=RadSlope%Zp2(j,i)
      if (ABS(YP) > 0._wp) then
       CALL AXIALP(X2,YP,Y2X,POW)
       Atlas%AD(i,j)=ABS(RadSlope%r(j,i))/100.0_wp ! scale value
       Atlas%AR(i,j)=Atlas%AD(i,j)  !just to make something
       Atlas%AP(i,j)=POW
       Atlas%AY(i,j)=RadSlope%Z(j,i)  !completely scaled wrong
      endif
      end do
  end do
end subroutine Atlas_eq_RadSlope

!aka power2slope using ZFCT converts lhs to rhs
subroutine RadSlope_eq_Atlas(RadSlope,Atlas) ! initially populates r, thta, Zp, MV
  TYPE(wpRadSlopeMatrix), INTENT(INOUT) :: RadSlope
  TYPE(wpAtlasMatrix), INTENT(INOUT) :: Atlas
  REAL(wp) :: ZIX,ZJX,YA3,X2A1
  REAL(wp) :: DIST,R,POW
  INTEGER :: i,j,MM,imv(size(RadSlope%r,2))
    MM=size(RadSlope%r,2)
    N=size(RadSlope%r,1)
    imv=0
    do i=1,MM
     RadSlope%thta(i)=PI*Atlas%DEG(i)/180.0_wp
     do j=1,N
      if ((Atlas%AP(i,j) > 0) .AND. (Atlas%AR(i,j) > 0) .AND. (Atlas%AD(i,j) > 0) .AND. (Atlas%AY(i,j) > 0)) then    ! Only for Atlas with valid data /= 0
       DIST=Atlas%AD(i,j)
       R=Atlas%AR(i,j)
       POW=Atlas%AP(i,j)
       ZIX=RFCT/POW
!      only use AD == DIST not R here
       ZJX=DIST*100
       if (ZIX > ZJX) then
        imv(i)=imv(i)+1                                            
        CALL ZFCT(MM,i,ZJX,ZIX,X2A1,YA3)
       else
        cycle
       endif
       RadSlope%r(imv(i),i)=X2A1
       RadSlope%Zp(imv(i),i)=YA3
    !  These are not even close; Y is tiny, AY range is in the 2's
       RadSlope%Z(imv(i),i)=Atlas%AY(i,j) 
       RadSlope%Zp2(imv(i),i)=1/803.0_wp ! fallback value before splining
      endif
     end do
    end do
    RadSlope%MV(:)=imv(:)
end subroutine RadSlope_eq_Atlas

subroutine DiaSlope_eq_RadSlope(DiaSlope,RadSlope)
 TYPE(wpDiaSlopeMatrix), INTENT(INOUT) :: DiaSlope
 TYPE(wpRadSlopeMatrix), INTENT(INOUT) :: RadSlope
 integer :: i,j
 integer :: M1,N1
 real(wp) :: rB
 ASSOCIATE(MV=>RadSlope%MV,rOMIN=>DiaSlope%rOutMin,rIMIN=>DiaSlope%rInMin,&
                           rOMAX=>DiaSlope%rOutMax,rIMAX=>DiaSlope%rInMax)
   M1=size(DiaSlope%rd,2) !M1=MM/2
   N1=size(DiaSlope%rd,1) !N1=2*N
   do i=1,M1
    DiaSlope%L2(i)=MV(i)+MV(i+M1)
!   initialize bounds    
    rOMIN(i)=1E30
    rOMAX(i)=-1E30
    rIMIN(i)=-1E30
    rIMAX(i)=1E30
    do j=1,N1   ! only go to 2*N, or out of bounds for RadSlope, central value DiaSlope provided in nSplineCenter
      if (j <= MV(i+M1)) then
!      NO SIGN CHANGE HERE FOR RADIUS, ALREADY DONE IN RCNVRT 
       DiaSlope%rd(j,i)=RadSlope%r(MV(i+M1)-j+1,i+M1)
       DiaSlope%Zd(j,i)=RadSlope%Z(MV(i+M1)-j+1,i+M1)
       DiaSlope%Zpd(j,i)=RadSlope%Zp(MV(i+M1)-j+1,i+M1)
       DiaSlope%Zpd2(j,i)=RadSlope%Zp2(MV(i+M1)-j+1,i+M1)
!      FIND BOUNDS          
       rB=DiaSlope%rd(j,i) 
       if (rB <= rOMIN(i)) rOMIN(i)=rB
       if (rB >= rIMIN(i)) rIMIN(i)=rB                    
      endif
      if (j <= MV(i)) then
       DiaSlope%rd(j+MV(i+M1),i)=RadSlope%r(j,i)
       DiaSlope%Zd(j+MV(i+M1),i)=RadSlope%Z(j,i)
       DiaSlope%Zpd(j+MV(i+M1),i)=RadSlope%Zp(j,i)
       DiaSlope%Zpd2(j+MV(i+M1),i)=RadSlope%Zp2(j,i)    
!      FIND BOUNDS          
       rB=DiaSlope%rd(j+MV(i+M1),i)
       if (rB <= rIMAX(i)) rIMAX(i)=rB
       if (rB >= rOMAX(i)) rOMAX(i)=rB                        
      endif
    end do
   end do
 end ASSOCIATE
end subroutine DiaSlope_eq_RadSlope

subroutine RadSlope_eq_DiaSlope(RadSlope,DiaSlope)
 integer :: i,j
 integer :: M1,N1
 TYPE(wpRadSlopeMatrix), INTENT(INOUT) :: RadSlope
 TYPE(wpDiaSlopeMatrix), INTENT(INOUT) :: DiaSlope
 ASSOCIATE(MV => RadSlope%MV) 
   M1=size(DiaSlope%rd,2) !M1=MM/2
   N1=size(DiaSlope%rd,1) !N1=2*N
   do i=1,M1
    do j=1,N1  ! only go to 2*N, or out of bounds for RadSlope, central value DiaSlope provided in nSplineCenter
      if (j <= MV(i+M1)) then
       RadSlope%r(MV(i+M1)-j+1,i+M1)=DiaSlope%rd(j,i)
       RadSlope%Z(MV(i+M1)-j+1,i+M1)=DiaSlope%Zd(j,i)
       RadSlope%Zp(MV(i+M1)-j+1,i+M1)=DiaSlope%Zpd(j,i)
       RadSlope%Zp2(MV(i+M1)-j+1,i+M1)=DiaSlope%Zpd2(j,i)
      endif
      if (j <= MV(i)) then
       RadSlope%r(j,i)=DiaSlope%rd(j+MV(i+M1),i)
       RadSlope%Z(j,i)=DiaSlope%Zd(j+MV(i+M1),i)
       RadSlope%Zp(j,i)=DiaSlope%Zpd(j+MV(i+M1),i)
       RadSlope%Zp2(j,i)=DiaSlope%Zpd2(j+MV(i+M1),i)
      endif
    end do
   end do
 end ASSOCIATE   
end subroutine RadSlope_eq_DiaSlope

! spline b%rd(:,i),b%Zpd(:,i)
function DiaSpline(b) result(a) 
 TYPE(wpDiaSlopeMatrix),INTENT(IN) :: b
 integer :: M1,N1,i
 real(wp) :: a(size(b%rd,1),size(b%rd,2))
 N1=size(b%rd,1) !N1=2*N*M
 M1=size(b%rd,2) !M1=MM/2
  a=0  !initialize else the damn thing will fill with NaN
  do i=1,M1 
   call nspline(b%rd(:,i),b%Zpd(:,i),b%L2(i),a(:,i))
  end do
end function DiaSpline

function DiaSplineCenter(b) result(a)
 TYPE(wpDiaSlopeMatrix),INTENT(IN) :: b
 real(wp) :: a(size(b%rd,1),size(b%rd,2))
 integer :: M1,N1,i
 N1=size(b%rd,1) !N1=2*N*M
 M1=size(b%rd,2) !M1=MM/2
 a=0
  do i=1,M1 
   call nsplineCenter(i,b%rd(:,i),b%Zpd(:,i),b%L2(i),a(:,i))
  end do
end function DiaSplineCenter

! this version is for matrices that are MxN, ie Atlas
function splinefillin(b) result(a)
 real(wp),INTENT(IN) :: b(:,:)
 TYPE(wpsplinevect) :: spline
 integer :: M1,N1,i,j,k
 real(wp) :: a(size(b,1),size(b,2)),tht(size(b,1)),RTEMP,Q,radianK
! if Atlas then  size(b,2)->N and size(b,1)->M
 N1=size(b,2) !N1=N
 M1=size(b,1) !M1=MM
 a=0 ; tht=0  !initialize else the damn thing will fill with NaN
 allocate (spline%r(M1),spline%z(M1),spline%zp2(M1),spline%mvjr(N1))
 associate (t=>spline%r,z=>spline%z,zt2=>spline%zp2,mvjr=>spline%mvjr)
  mvjr=0
  do j=1,M1
   tht(j)=PI*(j-1)/90.0_wp  ! every 2 degrees
  end do
  do i=1,N1
     do j=1,M1
       Q=b(j,i)
        if (ABS(Q) > 0.) then ! ABS is optional for AR or AP
         mvjr(i)=mvjr(i)+1
         t(mvjr(i))=tht(j)
         z(mvjr(i))=Q
        endif
      end do
!     no splining if less than half the points available
      if (mvjr(i) .gt. (M1/2)) then
       call pspli(t,z,mvjr(i),zt2)
      else
       cycle
      endif
      do k=1,M1
      radianK=tht(k)
       call SplineEval(1,t,z,zt2,mvjr(i),radianK,RTEMP)
        if(ABS(b(K,I)-RTEMP) > EPS) then
         if(ABS(b(K,I)) > EPS) then
         write(*,*) 'spline error in cornea_arrays fillin',K,I,b(K,I),RTEMP
         endif
        endif
        a(k,i)=RTEMP
      end do
   end do
   end associate
   deallocate (spline%r,spline%z,spline%zp2,spline%mvjr)
end function splinefillin

! this version is for matrices that are NxM, ie. JMatrix
function splinefillintranspose(b) result(a)
 real(wp),INTENT(IN) :: b(:,:)
 TYPE(wpsplinevect) :: spline
 integer :: M1,N1,i,j,k
 real(wp) :: a(size(b,1),size(b,2)),tht(size(b,2)),RTEMP,Q,radianK
! if JMatrix then  size(b,2)->M and size(b,1)->N
 N1=size(b,1) !N1=N
 M1=size(b,2) !M1=MM
 a=0 ; tht=0  !initialize else the damn thing will fill with NaN
 allocate (spline%r(M1),spline%z(M1),spline%zp2(M1),spline%mvjr(N1))
 associate (t=>spline%r,z=>spline%z,zt2=>spline%zp2,mvjr=>spline%mvjr)
  do j=1,M1
   tht(j)=PI*(j-1)/90.0_wp  ! every 2 degrees
  end do
  mvjr=0
  do i=1,N1
     do j=1,M1
       Q=b(i,j)
        if (ABS(Q) > 0.) then ! ABS is optional for AR or AP
         mvjr(i)=mvjr(i)+1
         t(mvjr(i))=tht(j)
         z(mvjr(i))=Q
        endif
      end do
!     no splining if less than half the points available
      if (mvjr(i) .gt. (M1/2)) then
       call pspli(t,z,mvjr(i),zt2)
       else
        cycle
       endif
      do k=1,M1
      radianK=tht(k)
       call SplineEval(1,t,z,zt2,mvjr(i),radianK,RTEMP)
        if(ABS(b(i,k)-RTEMP) > EPS) then
         if(ABS(b(i,k)) > EPS) then
         write(*,*) 'spline error in cornea_arrays fillin',K,I,b(i,k),RTEMP
         endif
        endif
        a(i,k)=RTEMP
      end do
   end do
   end associate
   deallocate (spline%r,spline%z,spline%zp2,spline%mvjr)
end function splinefillintranspose

! not sure if this is safe with too few points or zero points in a ring
function lsqfillin(b) result(a)
 real(wp),INTENT(IN) :: b(:,:)
 integer :: M1,N1,i,j,k
 real(wp) :: a(size(b,1),size(b,2)),t(size(b,1)),z(size(b,1))
 real(wp) :: c(M2)
 logical :: Q
 N1=size(b,2) !N1=N
 M1=size(b,1) !M1=MM
 a=0  !initialize else the damn thing will fill with NaN
 z=0
 t=0
 do i=1,N1
  do j=1,M2
   do k=1,M1
    Q=ABS(b(k,i)) > 0
    if (Q) then ! means it is  =/ 0
     z(k)=b(k,i)
     t(k)=PI*(k-1)/90.0_wp  ! every 2 degrees
    endif
   end do 
  end do
  call lsqfill(t,z,M1,M2,c)
! generate lsq fillin values
  do k=1,M1
    Q=ABS(b(k,i)) > 0
    if (Q) then ! means it is  =/ 0
     a(k,i)=b(k,i)   ! retain old values where they exist
    else
    call LSQEval(M2,c,t(k),a(k,i))
    endif
  end do
 end do   
end function lsqfillin

function pca(M3,b) result(a) 
 use set_precision, ONLY : wp
 TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
 TYPE(wpRadSlopeMatrix) :: a
 integer, INTENT(IN) :: M3  ! pca terms, 2 or 3
 integer :: M1,N1,i,j,k,l,info,lwork,M
 real(wp) :: X(M3,size(b%r,1)),XTX(M3,M3),work(3*M3),w(M3) 
 logical :: Q
 lwork=size(work)
 N1=size(b%r,1) !N1=N 
 M1=size(b%r,2) !M1=MM
 a=0  !initialize else the damn thing will fill with NaN
 if (M3==2) then ! each ring
  do i=1,N1 
   do k=1,M1  
    Q=ABS(b%r(k,i)) > 0  
    if (Q) then ! means it is  =/ 0
     X(1,k)=b%r(k,i)*cos(360*b%thta(k)/M1)
     X(2,k)=b%r(k,i)*sin(360*b%thta(k)/M1) 
    endif       
   end do   
! X transpose X
  XTX=0
  M=0
  do j=1,M3
   do l=1,M3 
    do k=1,M1 
     Q=ABS(b%r(k,i)) > 0  
     if (Q) then ! means it is  =/ 0    
      XTX(j,l)=XTX(j,l)+X(j,k)*X(l,k)
      M=M+1
     endif
    end do
   end do
  end do
  XTX=M3*M3*XTX/M
! compute the eigenvalues
  call DSYEV( 'V', 'U', M3, XTX, M3, W, WORK, LWORK, INFO )
   if ( info /= 0 ) then
    WRITE (*,'(''Argument '',i3,'' has an illegal value'')') - info
   endif
   write(*,*) i,'th eigenvalues W from pca in cornea_arrays: ',SQRT(W)  
  end do 

 else          ! whole data set

  do i=1,N1   
   do k=1,M1  
    Q=ABS(b%r(k,i)) > 0  
    if (Q) then ! means it is  =/ 0
     X(1,k)=b%r(k,i)*cos(360*b%thta(k)/M1)
     X(2,k)=b%r(k,i)*sin(360*b%thta(k)/M1) 
     X(3,k)=b%Zp(k,i)
    endif       
   end do 
! X transpose X
  XTX=0
  M=0
  do j=1,M3
   do l=1,M3 
    do k=1,M1 
     Q=ABS(b%r(k,i)) > 0  
     if (Q) then ! means it is  =/ 0    
      XTX(j,l)=XTX(j,l)+X(j,k)*X(l,k)
      M=M+1
     endif
    end do
   end do
  end do
  end do
  XTX=M3*M3*XTX/M          ! same as division by M1 if no missing points
! compute the eigenvalues
  call DSYEV( 'V', 'U', M3, XTX, M3, W, WORK, LWORK, INFO )
   if ( info /= 0 ) then
    WRITE (*,'(''Argument '',i3,'' has an illegal value'')') - info
   endif
   write(*,*) 'The eigenvalues W from pca in cornea_arrays: ',SQRT(W)
 endif
 end function pca

!! corneal calculation subroutines

! signed slope and radius from eyesys style data, or ZIX=RFCT/POW, POW is axial power from Atlas style data
subroutine ZFCT(MM,ITH,ZJX,ZIX,X2A1,YA3)
 REAL(wp), INTENT(IN) :: ZIX,ZJX
 REAL(wp), INTENT(OUT) :: YA3,X2A1
 REAL(wp) :: YA1,YA2
 INTEGER, INTENT(IN) :: ITH,MM
 !     INVERSE IS AXIALP	
      if (ZIX <= ZJX) WRITE (*,*) 'ERROR IN ARCTAN'
 !     CONVERTS ZIX TO DZ/DR
      YA1=ZJX/(ZIX-ZJX)
      YA2=ZJX/(ZIX+ZJX)
      if (ITH > (MM/2)) then  ! PI
 !       SIGN CHANGE HERE FOR R, OR DZ/DR  *ONLY* WHEN SPLINING ALONG R
        YA3=-SQRT(YA1*YA2)
        X2A1=-ZJX   
      else
        YA3=SQRT(YA1*YA2)
        X2A1=ZJX 
      endif
end subroutine ZFCT

! axial power from slope and derivatives
subroutine AXIALP(X2,Y1X,Y2X,SAGC)
 real(wp), INTENT(IN) :: X2,Y1X,Y2X
 real(wp), INTENT(OUT) :: SAGC
 if (ABS(X2) < eps) then
! UNDEFINED AT ORIGIN X2=0, LIMIT IS RFCT*Y2X             
  SAGC=RFCT*Y2X
 else
  SAGC=RFCT*Y1X/(X2*SQRT(1+Y1X**2))
 endif     
end subroutine AXIALP

! instantaneous "tangential" power using slope/derivatives with and without use of calculated angular derivatives
subroutine instantp(X2,Y1X,Y1T,Y2X,TANC)
 real(wp), INTENT(IN) :: X2,Y1X,Y1T,Y2X
 real(wp), INTENT(OUT) :: TANC
 TANC=RFCT*Y2X/(SQRT(1+(Y1X)**2)**3)
end subroutine instantp 

! mean power of slope/derivatives with use of calculated angular derivatives
subroutine meanp(X1,X2,Y1XIN,Y1T,Y1XT,Y2T,Y2X,ZMM)
 real(wp), INTENT(IN) :: X1,X2,Y1XIN,Y1T,Y1XT,Y2T,Y2X
 real(wp), INTENT(OUT) :: ZMM 
 real(wp) :: ZMX,Y,Y1X
! MONGE MEAN CURVATURE
! WHEN X2<0 X1>PI
  if (ABS(X2) > EPS) then
   IF (X2 > 0 .AND. X1 > PI) THEN
      Y=X2
      Y1X=Y1XIN      
      ZMM=2*(1+Y1X**2+Y1T**2/Y**2)**(3/2.)
      ZMX=-2*Y1X*Y1T**2/(Y**3)+(2*Y1X*Y1XT*Y1T-Y2T)/(Y**2)-&
           Y2X*(Y1T/Y)**2-Y2T*(Y1X/Y)**2-(Y1X+Y1X**3)/Y-Y2X
      ZMM=-RFCT*ZMX/ZMM
   ELSE
      Y=-X2
      Y1X=-Y1XIN
      ZMM=2*(1+Y1X**2+Y1T**2/Y**2)**(3/2.)
      ZMX=-2*Y1X*Y1T**2/(Y**3)+(2*Y1X*Y1XT*Y1T-Y2T)/(Y**2)-&
           Y2X*(Y1T/Y)**2-Y2T*(Y1X/Y)**2-(Y1X+Y1X**3)/Y-Y2X
      ZMM=-RFCT*ZMX/ZMM
   ENDIF
   ELSE
!      UNDEFINED AT ORIGIN X2=0, LIMIT IS 1/Y2X         
      ZMM=RFCT*Y2X
   ENDIF
end subroutine meanp

! Monge astigmatism of slope/derivatives with use of calculated angular derivatives
subroutine mongea(X1,X2,Y1XIN,Y1T,Y1XT,Y2T,Y2X,ZA)
  real(wp), INTENT(IN) :: X1,X2,Y1XIN,Y1T,Y1XT,Y2T,Y2X
  real(wp), INTENT(OUT) :: ZA    
  real(wp) :: ZA1,ZA2,Y,Y1X
! MONGE ASTIG, plotted on log scale
! WATCH OUT FOR ZERO AT UMBILICAL POINTS!
! WHEN X2>0 X1<PI
   if (ABS(X2) > EPS) then
     if (X2 > 0 .AND. X1 > PI) then
       Y=X2
       Y1X=Y1XIN
       ZA1=(2*Y1X*Y1T**2+(Y1X**2+Y2X*Y1T**2+Y2T-2*Y1X*Y1XT*Y1T*Y2T)*Y&
                                       +(Y1X+Y1X**3)*Y**2+Y2X*Y**3)**2
       ZA1=((Y1T**2+Y**2+(Y1X*Y)**2)**3)*ZA1
       ZA2=((Y1T**2+Y**2+(Y1X*Y)**2)**4)*&
            (Y1T**2-2*Y1XT*Y1T*Y+(Y1XT*Y)**2-Y2X*Y2T*Y**2-Y1X*Y2X*Y**3)
       ZA=ABS(ZA1+4*ZA2)**(1/2.)
       ZA=RFCT*ZA/2.
     else
       Y=-X2
       Y1X=-Y1XIN
       ZA1=(2*Y1X*Y1T**2+(Y1X**2+Y2X*Y1T**2+Y2T-2*Y1X*Y1XT*Y1T*Y2T)*Y&
                                       +(Y1X+Y1X**3)*Y**2+Y2X*Y**3)**2
       ZA1=((Y1T**2+Y**2+(Y1X*Y)**2)**3)*ZA1
       ZA2=((Y1T**2+Y**2+(Y1X*Y)**2)**4)*&
            (Y1T**2-2*Y1XT*Y1T*Y+(Y1XT*Y)**2-Y2X*Y2T*Y**2-Y1X*Y2X*Y**3)
       ZA=ABS(ZA1+4*ZA2)**(1/2.)
       ZA=RFCT*ZA/2.
     endif
     else
!    UNDEFINED AT ORIGIN X2=0          
       ZA=1E30_wp  
     endif
end subroutine mongea

! principal curvature calculations
subroutine principal(t,r,hr,ht,hrt,htt,hrr,K,H,k1,k2,A)
  real(wp), INTENT(INOUT) :: t,r,hr,ht,hrt,htt,hrr
  real(wp), INTENT(OUT) :: K,H,k1,k2,A
  real(wp) :: hu,hv,huu,hvv,huv,g
   r=abs(r) ; hr=abs(hr)
   if (ABS(r) > EPS) then
!   cartesian conversion
    hu = hr*cos(t)-sin(t)*ht/r
    hv = hr*sin(t)+cos(t)*ht/r
    huu=hrr-(sin(t)**2)*(hrr-hr/r-htt/(r**2))+2*cos(t)*sin(t)*(ht/(r**2)-hrt/r)
    hvv=hrr-(cos(t)**2)*(hrr-hr/r-htt/(r**2))-2*cos(t)*sin(t)*(ht/(r**2)-hrt/r)
    huv=cos(t)*sin(t)*(hrr-hr/r-htt/(r**2))+(sin(t)**2-cos(t)**2)*(ht/(r**2)-hrt/r)
    g = 1 + hr**2 + (ht/r)**2
    K=(huu*hvv-huv*huv)/(g*g)
    H=((1+hv**2)*huu-2*hu*hv*huv+(1+hu**2)*hvv)/(2*(sqrt(g)**3))
    if (H**2-K < 0) write(*,*) 'error in principal'
     k1=H-sqrt(abs(H**2-K))
     k2=H+sqrt(abs(H**2-K))
     A=2*sqrt(abs(H**2-K))
   else
!   AT ORIGIN r = 0, things get weird at the limit
    if ( ABS(ht) > EPS ) then  ! but really it depends on ht/r and htt/r^2
     K=(htt/ht)**2
    else ! axisymmetric answer
     K=hrr*hrr
    endif
    H=hrr
    k1=hrr
    k2=hrr
    A=0
   endif
end subroutine principal


! axisymmetric_principal curvature calculations
subroutine axisymmetric_principal(r,hr,hrr,K,H,k1,k2,A)
  real(wp), INTENT(INOUT) :: r,hr,hrr
  real(wp), INTENT(OUT) :: K,H,k1,k2,A
    k1 = hrr/(SQRT(1+(hr)**2)**3)
   if (ABS(r) < eps) then
!   UNDEFINED AT ORIGIN X2=0, LIMIT IS RFCT*Y2X
    k2=hrr
   else
    k2=abs(hr/(r*SQRT(1+hr**2)))
   endif
   A=abs(k1-k2)
   H=(k1+k2)/2.
   K=k1*k2
end subroutine axisymmetric_principal


! Lines of Curvature
subroutine LIOC_Fortran(X1,X2,Y1X,Y1T,UTPOS,VTPOS)
   real(wp), intent(in) :: X1,X2,Y1X,Y1T
   real(wp), intent(inout) :: UTPOS,VTPOS
!  CARTESIAN TANGENT VECTOR COMPONENTS (-UTPOS,-VTPOS,1)  
   if (ABS(X2) > EPS) then  ! and ill conditioned even farther than that
     if (X2 > 0) then     
      UTPOS=Y1X*COS(X1)-Y1T*SIN(X1)/X2
      VTPOS=Y1X*SIN(X1)+Y1T*COS(X1)/X2
     else
      UTPOS=-Y1X*COS(X1)+Y1T*SIN(X1)/X2
      VTPOS=-Y1X*SIN(X1)-Y1T*COS(X1)/X2
     endif
   else
    write(*,*) 'Warning ill conditioned attempt at UT,VT'
   endif
end subroutine LIOC_Fortran

! instantaneous "tangential" power and mean power in terms of axial/"sagittal" power,radius and radial derivative of axial power 
 subroutine sagc2(X2,SAGC,DSAGC,TANC,ZMM)  
  real(wp), INTENT(IN) :: X2,SAGC,DSAGC
  real(wp), INTENT(OUT) :: TANC,ZMM   
  TANC=SAGC+X2*DSAGC
  ZMM=0.5_wp*(SAGC+TANC)
  end subroutine sagc2

!! select function
subroutine selectfunction(iflag,b,flag,powctr,powmin,powmax)
implicit none
integer(c_int), intent(in) :: flag
integer,intent(in) :: iflag
integer :: dat,fct,i,j,M1
real (wp), intent(out) :: powctr,powmin,powmax
TYPE(wpJMatrix), INTENT(INOUT) :: b
dat=(flag-mod(flag,1000000))/1000000 ! first two digits
fct=mod(((flag-mod(flag,10000))/10000),100) ! second two digits, color map functions
M1=size(b%r,2)
if (fct .lt. 16 .and. fct .gt. 0) then
  powctr=b%ZC0(1,fct)
  powmin=b%ZC0(2,fct)
  powmax=b%ZC0(3,fct)
else
SELECT CASE (fct)
  CASE (0)
  if (iflag == 0) then ! iflag == 0 load center/min/max into powctr/powmin/powmax
   powctr=b%SAGC0(1)
   powmin=b%SAGC0(2)
   powmax=b%SAGC0(3)
  endif
  if (iflag == 1) then ! iflag == 1 remake JMatrix (b) including center
   do i=1,M1
    do j=1,RadSlope%MV(i)
      RadSlope%Zp(j,i)=b%SAGC(j,i)
    end do
   end do
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope
   do i=1,M1
    do j=1,b%MV(i)
     if (btest(dat,0)) then
      call SplineEval1Dx1D(10,b%R(j,i),b%THT(i),b%SAGC(j,i))
     else
      call SplineEval1Dx1D(0,b%R(j,i),b%THT(i),b%SAGC(j,i))
     endif
     if (b%SAGC(j,i) <= b%SAGC0(2)) b%SAGC0(2)=b%SAGC(j,i)
     if (b%SAGC(j,i) >= b%SAGC0(3)) b%SAGC0(3)=b%SAGC(j,i)
    end do
   end do
   if (btest(dat,0)) then
    call SplineEval1Dx1D(10,b%R0,b%THT0,b%SAGC0(1))  ! center value
   else
    call SplineEval1Dx1D(0,b%R0,b%THT0,b%SAGC0(1))  ! center value
   endif
   if (b%SAGC0(1) <= b%SAGC0(2)) b%SAGC0(2)=b%SAGC0(1)
   if (b%SAGC0(1) >= b%SAGC0(3)) b%SAGC0(3)=b%SAGC0(1)
  endif
  CASE (16)
  powctr=b%INSTC0(1)
  powmin=b%INSTC0(2)
  powmax=b%INSTC0(3)
  CASE (17)
  powctr=b%INSTC20(1)
  powmin=b%INSTC20(2)
  powmax=b%INSTC20(3)
  CASE (18)
  powctr=b%MEANC0(1)
  powmin=b%MEANC0(2)
  powmax=b%MEANC0(3)
  CASE (19)
  powctr=b%MONGEA0(1)
  powmin=b%MONGEA0(2)
  powmax=b%MONGEA0(3)
  CASE (20)
  powctr=b%Z0(1)
  powmin=b%Z0(2)
  powmax=b%Z0(3)
  CASE (21)
  powctr=b%Warp0(1)
  powmin=b%Warp0(2)
  powmax=b%Warp0(3)
  CASE DEFAULT
  powctr=b%SAGC0(1)
  powmin=b%SAGC0(2)
  powmax=b%SAGC0(3)
END SELECT
endif
! always do Z to display the geometry
if (iflag == 1) then ! iflag == 1 remake JMatrix (b) including center
 do i=1,M1
  do j=1,RadSlope%MV(i)
    RadSlope%Zp(j,i)=b%Z(j,i)
  end do
 end do
 DiaSlope=RadSlope              ! move to diagonal format
 DiaSlope%Zpd2 = .n. DiaSlope
 do i=1,M1
  do j=1,b%MV(i)
   if (btest(dat,0)) then
    call SplineEval1Dx1D(10,b%R(j,i),b%THT(i),b%Z(j,i))
   else
    call SplineEval1Dx1D(0,b%R(j,i),b%THT(i),b%Z(j,i))
   endif
   if (b%Z(j,i) <= b%Z0(2)) b%Z0(2)=b%Z(j,i)
   if (b%Z(j,i) >= b%Z0(3)) b%Z0(3)=b%Z(j,i)
  end do
 end do
 if (btest(dat,0)) then
  call SplineEval1Dx1D(10,b%R0,b%THT0,b%Z0(1))  ! center value
 else
  call SplineEval1Dx1D(0,b%R0,b%THT0,b%Z0(1))  ! center value
 endif
 if (b%Z0(1) <= b%Z0(2)) b%Z0(2)=b%Z0(1)
 if (b%Z0(1) >= b%Z0(3)) b%Z0(3)=b%Z0(1)
endif
endsubroutine selectfunction

END MODULE cornea_arrays


