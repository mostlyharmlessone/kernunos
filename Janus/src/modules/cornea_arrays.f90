MODULE cornea_arrays

 USE set_precision, ONLY : wp
 USE LapackInterface, ONLY : dgetrf, dgetrs, dgesv, dsyev
 USE spline_interfaces 
 USE special_fct
!      USE spline_interfaces, ONLY : SplineEval, trapez, CubicSplineQuad, SplineCenter
!      USE special_fct, ONLY : OPERATOR(.p.) ! tensor summation convention 
 REAL(wp), PARAMETER :: PI=3.1415926535897932384626433832795_wp
 REAL(wp), PARAMETER :: RFCT=33750.0_wp
 REAL(wp), PARAMETER :: EPS=0.0001_wp  ! used in pspli,SplineCenter,corneal calc fcts
! INTEGER, PARAMETER :: NP=141         ! PentaCam
! INTEGER, PARAMETER :: MM=180, N=22   ! Atlas
! INTEGER, PARAMETER :: MM=360, N=16  ! EyeSys
 integer, PARAMETER :: M=2 ! augmented multiplier for number of rings
 integer, PARAMETER :: M2=3 ! lsq fourier cosine series terms

! Defining common data arrays
 
 TYPE wpEyeSysMatrix
!  RA, XX are undocumented but assumed to compute to powers and radii using Zfct, there are 360 rows
   REAL (wp), ALLOCATABLE :: RA(:,:), XX(:,:)
   INTEGER, ALLOCATABLE :: DEG(:)
 END TYPE wpEyeSysMatrix
 
 TYPE wpRadSlopeMatrix
!  theta, r are polar coordinates, Zp,Zp2,Zt2 are slope, and second derivatives
   REAL (wp), ALLOCATABLE :: thta(:), r(:,:), Zp(:,:), Zp2(:,:), Zt2(:,:)
   INTEGER, ALLOCATABLE :: MV(:)
 END TYPE wpRadSlopeMatrix
 
 TYPE wpAtlasMatrix
!  AR radius (polar coord), AD "distance?",AP is axial(sagittal) power in diopters ,AY elevation, DEG polar coord
   REAL (wp), ALLOCATABLE :: AR(:,:),AD(:,:),AP(:,:),AY(:,:),DEG(:)
 END TYPE wpAtlasMatrix
 
 TYPE wpPentaMatrix
!  EA is elevation in mm, CA is sagittal/axial curvature in mm, both on 141x141 grid of -7.00 mm to +7.00 mm, no data=-1
   REAL (wp), ALLOCATABLE :: ELE(:,:),CUR(:,:)
 END TYPE wpPentaMatrix

 TYPE wpSkyline
!  ELE is elevation in mm, CUR is sagittal/axial curvature in mm, both on 141x141 grid of -7.00 mm to +7.00 mm, no data=-1
   REAL (wp), ALLOCATABLE :: ELE(:,:),CUR(:,:),x(:,:),y(:,:),z2ELE(:,:),z2CUR(:,:)
   INTEGER, ALLOCATABLE :: L2x(:),L2y(:),index_col(:)
   INTEGER :: rows,cols,first_row   ! skyline needed rows and columns
 END TYPE wpSkyline
 
 TYPE wpDiaSlopeMatrix
!  rd is the radius positive and negative along the diagonal, rearranged from RadSlope above
   REAL (wp), ALLOCATABLE :: rd(:,:), Zpd(:,:), Zpd2(:,:)
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
 ! Type(oneofthesebelow) = INTEGER(0) deallocates the matrix
 MODULE PROCEDURE destroy_EyeSys
 MODULE PROCEDURE destroy_Penta
 MODULE PROCEDURE destroy_Atlas
 MODULE PROCEDURE destroy_RadSlope
 MODULE PROCEDURE destroy_DiaSlope
 MODULE PROCEDURE destroy_Skyline
END INTERFACE
   
INTERFACE OPERATOR (.i.)
! .i. TypeDiaSlopeMatrix integrates the matrix slope values 
 MODULE PROCEDURE DiaIntegrate ! uses CubicSplineQuad.f90, assumes DiaSpline already done, only modifies Zpd
END INTERFACE 

INTERFACE OPERATOR (.n.) ! unary operator
! .n. TypeDiaSlopeMatrix populates the matrix with second radial derivatives of z
! .n. TypeAtlasMatrix populates the matrix with second angular derivatives of r
! .n. TypeRadSlopeMatrix populates the matrix with second angular derivatives of z
 MODULE PROCEDURE DiaSpline ! uses nspline.f90
 MODULE PROCEDURE fillin ! uses pspli.f90
 MODULE PROCEDURE AngSpline ! uses pspli.f90
END INTERFACE 

! declaring common global data arrays
 real(wp), allocatable :: RadSplineCenter(:)
 TYPE(wpEyeSysMatrix) :: EyeSys
 TYPE(wpRadSlopeMatrix) :: RadSlope
 TYPE(wpAtlasMatrix) :: Atlas
 TYPE(wpPentaMatrix) :: Penta
 TYPE(wpSkyline) :: Skyline
 TYPE(wpDiaSlopeMatrix) :: DiaSlope
 TYPE(wpRadSlopeMatrix) :: ARadSlope
 TYPE(wpDiaSlopeMatrix) :: ADiaSlope

 CONTAINS
 
subroutine init_mat_Penta(NP,Penta,Skyline) ! allocate PentaCam arrays
  INTEGER, INTENT(IN) :: NP
  TYPE(wpPentaMatrix) :: Penta 
  TYPE(wpSkyline) :: Skyline  
  allocate (Penta%CUR(NP,NP),Penta%ELE(NP,NP))
  allocate (Skyline%CUR(NP,NP),Skyline%ELE(NP,NP),Skyline%x(NP,NP),&
            Skyline%z2CUR(NP,NP),Skyline%z2ELE(NP,NP),Skyline%L2x(NP),Skyline%L2y(NP),&
            Skyline%index_col(NP)) 
end subroutine init_mat_Penta

subroutine init_mat_EyeSys(MM,N,EyeSys) ! allocate EyeSys arrays
  INTEGER, INTENT(IN) :: MM,N
  TYPE(wpEyeSysMatrix) :: EyeSys
  allocate (EyeSys%RA(MM,N),EyeSys%XX(MM,N),EyeSys%DEG(MM))
end subroutine init_mat_EyeSys

subroutine init_mat(MM,N,Atlas,RadSlope,DiaSlope,RadSplineCenter) ! allocate common arrays
  INTEGER, INTENT(IN) :: MM,N
  real(wp), allocatable :: RadSplineCenter(:)
  TYPE(wpRadSlopeMatrix) :: RadSlope  
  TYPE(wpAtlasMatrix) :: Atlas
  TYPE(wpDiaSlopeMatrix) :: DiaSlope
  allocate (RadSlope%r(MM,N),RadSlope%Zp(MM,N),RadSlope%Zp2(MM,N),&
            Radslope%Zt2(MM,N),RadSlope%thta(MM),RadSlope%MV(MM))  
  allocate (DiaSlope%rd(2*N,MM/2),DiaSlope%Zpd(2*N,MM/2),&
            DiaSlope%Zpd2(2*N,MM/2),DiaSlope%L2(MM/2))
  allocate (DiaSlope%rOutMax(MM/2),DiaSlope%rInMax(MM/2),&
            DiaSlope%rOutMin(MM/2),DiaSlope%rInMin(MM/2))
  allocate (Atlas%AR(MM,N),Atlas%AD(MM,N),Atlas%AP(MM,N),&
            Atlas%AY(MM,N),Atlas%DEG(MM))
  allocate (RadSplineCenter(MM))          
end subroutine init_mat

subroutine init_augmented_mat(MM,N,M,ARadSlope,ADiaSlope) !allocate augmented arrays
  INTEGER, INTENT(IN) :: MM,N,M
  TYPE(wpRadSlopeMatrix) :: ARadSlope  
  TYPE(wpDiaSlopeMatrix) :: ADiaSlope  
  allocate (ARadSlope%r(MM,N*M),ARadSlope%Zp(MM,N*M),ARadSlope%Zp2(MM,N*M),&
            ARadSlope%Zt2(MM,N*M),ARadSlope%thta(MM),ARadSlope%MV(MM))  
  allocate (ADiaSlope%rd(2*N*M,MM/2),ADiaSlope%Zpd(2*N*M,MM/2),&
            ADiaSlope%Zpd2(2*N*M,MM/2),ADiaSlope%L2(MM/2))
  allocate (ADiaSlope%rOutMax(MM/2),ADiaSlope%rInMax(MM/2),&
            ADiaSlope%rOutMin(MM/2),ADiaSlope%rInMin(MM/2))
end subroutine init_augmented_mat

! Type()=0 deallocates storage

subroutine destroyRadSplineCenter(RadSplineCenter)
  real(wp), allocatable :: RadSplineCenter(:)
  deallocate (RadSplineCenter)
end subroutine destroyRadSplineCenter

subroutine destroy_EyeSys(EyeSys,iflag)
  TYPE(wpEyeSysMatrix), INTENT(INOUT) :: EyeSys
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (EyeSys%RA,EyeSys%XX,EyeSys%DEG)
  ENDIF
end subroutine destroy_EyeSys

subroutine destroy_Atlas(Atlas,iflag)
  TYPE(wpAtlasMatrix), INTENT(INOUT) :: Atlas
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (Atlas%AR,Atlas%AD,Atlas%AP,Atlas%AY,Atlas%DEG)
  ENDIF
end subroutine destroy_Atlas

subroutine destroy_RadSlope(RadSlope,iflag)
  TYPE(wpRadSlopeMatrix), INTENT(INOUT) :: RadSlope
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (RadSlope%r,RadSlope%Zp,RadSlope%Zp2,&
              RadSlope%thta,RadSlope%MV)
  ENDIF
end subroutine destroy_RadSlope

subroutine destroy_DiaSlope(DiaSlope,iflag)
  TYPE(wpDiaSlopeMatrix), INTENT(INOUT) :: DiaSlope
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (DiaSlope%rd,DiaSlope%Zpd,DiaSlope%Zpd2,DiaSlope%L2,&
              DiaSlope%rOutMax,DiaSlope%rInMax,DiaSlope%rOutMin,DiaSlope%rInMin)
  ENDIF
end subroutine destroy_DiaSlope

subroutine destroy_Penta(Penta,iflag)
  TYPE(wpPentaMatrix), INTENT(INOUT) :: Penta
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (Penta%CUR,Penta%ELE)
  ENDIF
end subroutine destroy_Penta

subroutine destroy_Skyline(Skyline,iflag)
  TYPE(wpSkyline), INTENT(INOUT) :: Skyline
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (Skyline%CUR,Skyline%ELE,Skyline%x,Skyline%z2CUR,&
              Skyline%z2ELE,Skyline%L2x,Skyline%L2y,Skyline%index_col)
  ENDIF
end subroutine destroy_Skyline
!!array conversion routines

subroutine Skyline_eq_Penta(Skyline,Penta)                                            ! Arrange data Skyline, that will allow loading into SplineEval                                            
  TYPE(wpSkyline) :: Skyline                                                          ! x,f(x) knots, number of knots(length) and u (test point) 
  TYPE(wpPentaMatrix) :: Penta                                                        ! This is the equivalent of DiaSlope=RadSlope
  Integer :: i,j,NP,ii,jj                                                             ! skyline x by rows, generate y using indices later
  Integer :: first_row,last_row,col(size(Penta%CUR,1)),index_row(size(Penta%CUR,1))                                
  Integer :: first_col,last_col,row(size(Penta%CUR,1)),index_col(size(Penta%CUR,1))
  NP=size(Penta%CUR,1)
! Find edges of data, Penta "data" is contiguous
  index_row=0 ; col=0 ; Skyline%cols=0 ; first_row=0 ; last_row=141  
  index_col=0 ; row=0 ; Skyline%rows=0 ; first_col=0 ; last_col=141
  Skyline%x=0 ; Skyline%CUR=0 ; Skyline%ELE=0
  Skyline%L2x=0 ; Skyline%L2y=0
  do i=1,NP
   do j=1,NP
    if (Penta%CUR(i,j) > 0) then 
     col(i)=col(i)+1                 ! count number of nonnegative (columns) entries (data points) in row (i)
     if ( index_row(i) < 1 ) then
      index_row(i)=j                 ! remember starting point on col(i), assumes no holes in data
     endif
     if (first_row < 1) then         ! remember first_row
       first_row=i
     endif
    endif
!   For calculating L2y, transpose the matrix
    if (Penta%CUR(j,i) > 0) then
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
    Skyline%CUR(i,j)=Penta%CUR(ii,jj)
    Skyline%ELE(i,j)=Penta%ELE(ii,jj)
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
   write(*,*) 'Inconsistent row count in Skyline'     ! numbers of rows should be maximum length of columns
   stop
  endif
end subroutine Skyline_eq_Penta

subroutine Atlas_eq_Skyline(Atlas,Skyline,Penta)      ! initially Atlas populates r, thta, POW, elevation with splining
  TYPE(wpSkyline), INTENT(INOUT) :: Skyline                                             
  TYPE(wpAtlasMatrix), INTENT(INOUT) :: Atlas
  TYPE(wpPentaMatrix), INTENT(IN) :: Penta
  integer :: M1,N1,i,j,k,kk,L2,offset,NP,ITH
  integer :: imv(size(Atlas%AR,1))
  real(wp) :: rBo,rBi,CUR,ELE,u,v,xx,yy,f,fTmp(Skyline%rows),f2Tmp(Skyline%rows)
  real(wp) :: r(size(Atlas%AR,1)),z(Skyline%cols),z2(Skyline%cols)
  real(wp) :: x(Skyline%cols),zx(Skyline%cols),zx2(Skyline%cols)           ! maximum size needed, don't need NP
  real(wp) :: y(Skyline%rows),gTmp(Skyline%rows),g2Tmp(Skyline%rows)
  M1=size(Atlas%AR,1)
  N1=size(Atlas%AR,2)
  NP=size(Skyline%CUR,1)                                                   
  imv=0
! Spline both CUR and ELE in x                               (this is the equivalent of DiaSpline)
  do i=1,Skyline%rows
   L2=Skyline%L2x(i)                                       
   x(1:L2)=Skyline%x(i,1:L2)
   zx(1:L2)=Skyline%CUR(i,1:L2)
   call nspline(x,zx,L2,zx2)                                ! generate zxCUR 
   Skyline%z2CUR(i,1:L2)=zx2(1:L2)
   zx(1:L2)=Skyline%ELE(i,1:L2)
   call nspline(x,zx,L2,zx2)                                ! generate zxELE
   Skyline%z2ELE(i,1:L2)=zx2(1:L2)
  end do
! make rings
! scale in 14x 14 mm of Penta matrix 141x141 divided by 2
  rBo=7.0
  rBi=0.10*rBo                              ! donut                                                      			
  do i=1,M1
   ITH=2*(i-1)
   Atlas%DEG(i)=ITH                        ! Atlas style degrees every two
   do j=1,N1
    r(j)=(j-1)*(rBo-rBi)/(N1-1)+rBi  
    u=r(j)*COS(PI*Atlas%DEG(i)/180)        ! x,y coordinates of ring point
    v=r(j)*SIN(PI*Atlas%DEG(i)/180)
!   boundary check here 
    xx=u*(NP-1)/14.0 ; yy=v*(NP-1)/14.0
    if (Penta%CUR(1+(NP-1)/2+sign(floor(ABS(xx)),floor(xx)),1+(NP-1)/2+sign(floor(ABS(yy)),floor(yy))) < 0) then  ! test for -1 why is Penta available here
     cycle ! skip this one
    endif
!   Populate Atlas with Splined PentaCam
!   Spline both CUR and ELE in y      (this is the equivalent of Spline1Dx1D)
    do k=1,Skyline%rows
     L2=Skyline%L2x(k)
     x(1:L2)=Skyline%x(k,1:L2)                                
     z(1:L2)=Skyline%CUR(k,1:L2) 
     z2(1:L2)=Skyline%z2CUR(k,1:L2)
     zx(1:L2)=Skyline%ELE(k,1:L2) 
     zx2(1:L2)=Skyline%z2ELE(k,1:L2)
     call SplineEval(0,x(1:L2),zx(1:L2),zx2(1:L2),L2,u,f) ! first parameter = 0 nonperiodic                                  
     gTmp(k)=f                                            ! f is value at u, fTmp is a new 1:(Skyline%rows) column of values at u     
     call SplineEval(0,x(1:L2),z(1:L2),z2(1:L2),L2,u,f)   ! first parameter = 0 nonperiodic                                    
     fTmp(k)=f                                          
    end do
    offset=Skyline%first_row-1
    L2=Skyline%rows
    do kk=1,L2            ! fTmp has to align with y; but ftmp starts at Skyline%first_row, y starts at 1 for calculation                      
     y(kk)=7.00-((kk-1+offset)*14.00)/(NP-1.0)
    end do
    call nspline(y(1:L2),fTmp(1:L2),L2,f2Tmp(1:L2))             ! spline in Y
    call SplineEval(0,y(1:L2),fTmp(1:L2),f2Tmp(1:L2),L2,v,CUR)  ! first parameter = 0 nonperiodic 
    call nspline(y(1:L2),gTmp(1:L2),L2,g2Tmp(1:L2))             ! spline in Y
    call SplineEval(0,y(1:L2),gTmp(1:L2),g2Tmp(1:L2),L2,v,ELE)  ! first parameter = 0 nonperiodic 
    imv(i)=imv(i)+1
    Atlas%AY(i,imv(i))=ABS(ELE)
    Atlas%AR(i,imv(i))=ABS(r(j))                              
    Atlas%AD(i,imv(i))=Atlas%AR(i,imv(i))                    
    Atlas%AP(i,imv(i))=RFCT/(100.0*ABS(CUR))     ! convert "curvatures" in mm to diopters                                                       
   end do !j to N1
  end do !i to M1

end subroutine Atlas_eq_Skyline

! uses ZFCT converts lhs to rhs
subroutine RadSlope_eq_EyeSys(RadSlope,EyeSys) ! initially populates r, thta, Zp, MV
  TYPE(wpEyeSysMatrix) :: EyeSys
  TYPE(wpRadSlopeMatrix) :: RadSlope
  INTEGER :: i,j,MM,N
  integer :: imv(size(RadSlope%r,1))
  REAL(wp) :: ZIX,ZJX,YA3,X2A1
  MM=size(RadSlope%r,1)
  N=size(RadSlope%r,2)
  do i=1,MM
    imv(i)=0
    RadSlope%thta(i)=PI*EyeSys%DEG(i)/180.0_wp  ! RadSlope%thta(i)=PI*(i-1)/180.0_wp should always be true for EyeSys
      do j=1,N
       ZIX=EyeSys%XX(i,j)
       ZJX=EyeSys%RA(i,j)
       if (ZIX > 0 .AND. ZJX > 0) then
        imv(i)=imv(i)+1
        call ZFCT(MM,i,ZJX,ZIX,X2A1,YA3)
        RadSlope%Zp(i,imv(i))=YA3
        RadSlope%r(i,imv(i))=X2A1
        else
        RadSlope%Zp(i,j)=0._wp  ! sets border
       endif
        RadSlope%Zp2(i,j)=1/803.0_wp ! nonzero fallback value before splining for Atlas=RadSlope      
      end do
      RadSlope%MV(i)=imv(i)
   end do
end subroutine RadSlope_eq_EyeSys

! aka SLOPE2POWER using AXIALP converts lhs to rhs
subroutine Atlas_eq_RadSlope(Atlas,RadSlope)
  TYPE(wpRadSlopeMatrix) :: RadSlope
  TYPE(wpAtlasMatrix) :: Atlas
  INTEGER :: i,j,MM,N
  integer :: imv(size(RadSlope%r,1))
  REAL(wp) :: X2,YP,Y2X,POW
  imv=0
  Atlas%AP=0._wp
  MM=size(RadSlope%r,1)
  N=size(RadSlope%r,2)
  do i=1,MM
    Atlas%DEG(i)=RadSlope%thta(i)
      do j=1,N
      X2=RadSlope%r(i,j)
      YP=RadSlope%Zp(i,j)
      Y2X=RadSlope%Zp2(i,j)
      if (ABS(YP) > 0._wp) then
       imv(i)=imv(i)+1
       CALL AXIALP(X2,YP,Y2X,POW)
       Atlas%AR(i,imv(i))=ABS(RadSlope%r(i,imv(i)))/100.0_wp ! scale value 
       Atlas%AD(i,imv(i))=Atlas%AR(i,imv(i))
       Atlas%AP(i,imv(i))=POW
      endif
      end do
  end do
end subroutine Atlas_eq_RadSlope

!aka power2slope using ZFCT converts lhs to rhs
subroutine RadSlope_eq_Atlas(RadSlope,Atlas) ! initially populates r, thta, Zp, MV
  TYPE(wpRadSlopeMatrix) :: RadSlope
  TYPE(wpAtlasMatrix) :: Atlas
  REAL(wp) :: ZIX,ZJX,YA3,X2A1
  REAL(wp) :: DIST,R,POW
  INTEGER :: i,j,MM,N,imv(size(RadSlope%r,1))
    MM=size(RadSlope%r,1)
    N=size(RadSlope%r,2)
    imv=0
    do i=1,MM
     RadSlope%thta(i)=PI*Atlas%DEG(i)/180.0_wp  
     ! RadSlope%thta(i)=PI*2*(i-1)/180_wp should always be true for "real" Atlas
     ! RadSlope%thta(i)=PI*(i-1)/180_wp should always be true for EyeSys generated Atlas
     do j=1,N
      if ((Atlas%AP(i,j) > 0) .AND. (Atlas%AR(i,j) > 0)) then    ! Only for Atlas with POW /= 0 
       imv(i)=imv(i)+1      
       DIST=Atlas%AD(i,j)
       R=Atlas%AR(i,j)
       POW=Atlas%AP(i,j)
       ZIX=RFCT/POW
!      could use DIST here
       ZJX=R*100                                              
       CALL ZFCT(MM,i,ZJX,ZIX,X2A1,YA3)
        RadSlope%r(i,imv(i))=X2A1
        RadSlope%Zp(i,imv(i))=YA3
      endif 
        RadSlope%Zp2(i,imv(i))=1/803.0_wp ! fallback value before splining  
     end do
    end do
    RadSlope%MV(:)=imv(:)
end subroutine RadSlope_eq_Atlas

subroutine DiaSlope_eq_RadSlope(DiaSlope,RadSlope)
 TYPE(wpDiaSlopeMatrix) :: DiaSlope
 TYPE(wpRadSlopeMatrix) :: RadSlope
 integer :: i,j
 integer :: M1,N1
 real(wp) :: rB
 ASSOCIATE(MV=>RadSlope%MV,rOMIN=>DiaSlope%rOutMin,rIMIN=>DiaSlope%rInMin,&
                           rOMAX=>DiaSlope%rOutMax,rIMAX=>DiaSlope%rInMax)
   M1=size(DiaSlope%rd,2) !M1=MM/2
   N1=size(DiaSlope%rd,1) !N1=2*N for "regular" DiaSlope, N1=2*N*M for augmented 
   do i=1,M1
    DiaSlope%L2(i)=MV(i)+MV(i+M1)
!   initialize bounds    
    rOMIN(i)=1E30
    rOMAX(i)=-1E30
    rIMIN(i)=-1E30
    rIMAX(i)=1E30
    do j=1,N1
      if (j <= MV(i+M1)) then
!      NO SIGN CHANGE HERE FOR RADIUS, ALREADY DONE IN RCNVRT 
       DiaSlope%rd(j,i)=RadSlope%r(i+M1,MV(i+M1)-j+1)
       DiaSlope%Zpd(j,i)=RadSlope%Zp(i+M1,MV(i+M1)-j+1)
       DiaSlope%Zpd2(j,i)=RadSlope%Zp2(i+M1,MV(i+M1)-j+1)
!      FIND BOUNDS          
       rB=DiaSlope%rd(j,i) 
       if (rB <= rOMIN(i)) rOMIN(i)=rB
       if (rB >= rIMIN(i)) rIMIN(i)=rB                    
      endif
      if (j <= MV(i)) then
       DiaSlope%rd(j+MV(i+M1),i)=RadSlope%r(i,j)
       DiaSlope%Zpd(j+MV(i+M1),i)=RadSlope%Zp(i,j)
       DiaSlope%Zpd2(j+MV(i+M1),i)=RadSlope%Zp2(i,j)      
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
 TYPE(wpRadSlopeMatrix) :: RadSlope
 TYPE(wpDiaSlopeMatrix) :: DiaSlope
 ASSOCIATE(MV => RadSlope%MV) 
   M1=size(DiaSlope%rd,2) !M1=MM/2
   N1=size(DiaSlope%rd,1) !N1=2*N for "regular" DiaSlope, N1=2*N*M for augmented
   do i=1,M1
    do j=1,N1
      if (j <= MV(i+M1)) then
       RadSlope%r(i+M1,MV(i+M1)-j+1)=DiaSlope%rd(j,i)
       RadSlope%Zp(i+M1,MV(i+M1)-j+1)=DiaSlope%Zpd(j,i)
       RadSlope%Zp2(i+M1,MV(i+M1)-j+1)=DiaSlope%Zpd2(j,i)
      endif
      if (j <= MV(i)) then
       RadSlope%r(i,j)=DiaSlope%rd(j+MV(i+M1),i)
       RadSlope%Zp(i,j)=DiaSlope%Zpd(j+MV(i+M1),i)
       RadSlope%Zp2(i,j)=DiaSlope%Zpd2(j+MV(i+M1),i)
      endif
    end do
   end do
 end ASSOCIATE   
end subroutine RadSlope_eq_DiaSlope

function DiaSpline(b) result(a) 
 TYPE(wpDiaSlopeMatrix),INTENT(IN) :: b
 integer :: M1,N1,i
 real(wp) :: a(size(b%rd,1),size(b%rd,2))  
 N1=size(b%rd,1) !N1=2*N*M for augmented
 M1=size(b%rd,2) !M1=MM/2
  do i=1,M1 
   call nspline(b%rd(:,i),b%Zpd(:,i),b%L2(i),a(:,i)) 
  end do
end function DiaSpline

function DiaSplineCenter(b) result(a) 
 TYPE(wpDiaSlopeMatrix),INTENT(IN) :: b
 integer :: M1,N1,i
 real(wp) :: a(size(b%rd,1),size(b%rd,2))  
 N1=size(b%rd,1) !N1=2*N*M for augmented
 M1=size(b%rd,2) !M1=MM/2
  do i=1,M1 
   call nsplineCenter(b%rd(:,i),b%Zpd(:,i),b%L2(i),a(:,i)) 
  end do
end function DiaSplineCenter

function DiaIntegrate(b) result(a)
 TYPE(wpDiaSlopeMatrix),INTENT(IN) :: b
 integer :: i,j,M1,N1
 real(wp) :: Q,Q0
 real(wp) :: a(size(b%rd,1),size(b%rd,2))
 N1=size(b%rd,1) !N1=2*N*M for augmented
 M1=size(b%rd,2) !M1=MM/2)
  do i=1,M1
   call CubicSplineQuad(b%rd(:,i),b%Zpd(:,i),b%Zpd2(:,i),b%L2(i),0._wp,Q0)
   do j=1,b%L2(i)    
    call CubicSplineQuad(b%rd(:,i),b%Zpd(:,i),b%Zpd2(:,i),b%L2(i),b%rd(j,i),Q)
    a(i,j)=Q-Q0
   end do
  end do
end function DiaIntegrate

function RadInterpolate(b) result(a) !interpolates values of radslope%Zp in new rings
 TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
 integer :: i,j,M1,N1
 real(wp) :: f0
 real(wp) :: a(size(b%Zp,1),size(b%Zp,2))  
 N1=size(b%Zp,2) !N1=N or N*M for ARadSlope
 M1=size(b%Zp,1) !M1=MM
  do i=1,M1
   do j=1,N1
    if (j .LE. b%MV(i)) then   !bounds
    call SplineEval1Dx1D(0,b%r(i,j),b%thta(i),f0)  ! no integration here
     a(i,j)=f0
    else
     a(i,j)=0  ! zero if out of bounds
    endif
   end do
  end do
end function RadInterpolate

function make_rings(b,Origin) result(a)   ! works on DiaSlope (needs bounds), generates round rings
 TYPE(wpDiaSlopeMatrix),INTENT(IN) :: b
 logical, intent(IN) :: Origin
 real(wp) :: a(2*size(b%rd,2),size(b%rd,1)/2) ! RadSlope size rings and radii
 real(wp) :: rBi,rBo
 integer :: M1,N1,i,j 
 M1=2*size(b%rd,2) !M1=MM convert DiaSlope dimensions to RadSlope
 N1=size(b%rd,1)/2 !N1=N convert "regular" Diaslope to RadSlope, N1=N*M for augmented
 rBi=1E30
 rBo=-1E30
 ASSOCIATE(rOMIN=>b%rOutMin,rIMIN=>b%rInMin,&
           rOMAX=>b%rOutMax,rIMax=>b%rInMax) 
   do i=1,M1/2 !DiaSlope dimension
    if (ABS(rOMIN(i)) >= rBo) rBo=ABS(rOMIN(i))
    if (ABS(rIMIN(i)) <= rBi) rBi=ABS(rIMIN(i))                    
    if (ABS(rIMAX(i)) <= rBi) rBi=ABS(rIMAX(i))
    if (ABS(rOMAX(i)) >= rBo) rBo=ABS(rOMAX(i))                                   
   end do
   if (Origin) then 
    rBi=0
   endif
   do i=1,M1 !RadSlope dimension
    if (i > M1/2) then  ! recreate negative radii convention as in ZFCT       
     do j=1,N1        
      a(i,j)=0.9*((1-j)*(rBo-rBi)/(N1-1)-rBi)
     end do
    else
     do j=1,N1    
      a(i,j)=0.9*((j-1)*(rBo-rBi)/(N1-1)+rBi)
     end do
    endif
   end do 
 end ASSOCIATE
end function make_rings

function make_bad_rings(b,Origin) result(a)   ! works on DiaSlope (needs bounds)
 TYPE(wpDiaSlopeMatrix),INTENT(IN) :: b       ! generates rings based on original reduced boundaries like genfilA
 logical, intent(IN) :: Origin
 real(wp) :: a(2*size(b%rd,2),size(b%rd,1)/2) ! RadSlope size rings
 real(wp) :: rBi(size(b%rd,2)),rBo(size(b%rd,1))
 integer :: M1,N1,i,j 
 ASSOCIATE(rOMIN=>b%rOutMin,rIMIN=>b%rInMin,&
           rOMAX=>b%rOutMax,rIMax=>b%rInMax)
   M1=size(b%rd,2)   !M1=MM/2
   N1=size(b%rd,1)/2 !N1=N convert "regular" Diaslope to RadSlope, N1=N*M for augmented
   if (Origin) then 
    rBi=0
    rBo=0
    else
    rBi=rIMIN
    rBo=rIMAX   
   endif   
   do i=1,M1 !DiaSlope dimension
     ! recreate negative radii convention as in ZFCT       
     do j=1,N1          
      a(i+M1,j)=((j-1)*(rOMIN(i)-rBi(i))/(N1-1)+rBi(i))   
      a(i,j)=((j-1)*(rOMAX(i)-rBo(i))/(N1-1)+rBo(i))
     end do
   end do 
 end ASSOCIATE
end function make_bad_rings

function AngSpline(b) result(a) 
 TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
 TYPE(wpsplinevect) :: spline
 integer :: M1,N1,i,j,k
 real(wp) :: a(size(b%r,1),size(b%r,2)),Q 
 N1=size(b%r,2) !N1=N 
 M1=size(b%r,1) !M1=MM
 allocate (spline%r(M1),spline%z(M1),spline%zp2(M1),spline%mvjr(N1))
 associate (t=>spline%r,z=>spline%z,zt2=>spline%zp2,mvjr=>spline%mvjr)
  mvjr=0
  do i=1,N1 
     do j=1,M1
       Q=b%Zp(j,i)
        if (ABS(Q) > 0.) then ! Q can be positive or negative if it's Zp, exactly 0 means no data point
         mvjr(i)=mvjr(i)+1                  
         t(mvjr(i))=b%thta(j)         
          z(mvjr(i))=Q       
        endif
      end do
      call pspli(t,z,mvjr(i),zt2)
      do k=1,M1                 
       a(k,i)=zt2(mvjr(i))
      end do
   end do   
   end associate
   deallocate (spline%r,spline%z,spline%zp2,spline%mvjr)
end function AngSpline

function fillin(b) result(a) 
 TYPE(wpAtlasMatrix),INTENT(IN) :: b
 TYPE(wpsplinevect) :: spline
 integer :: M1,N1,i,j,k
 real(wp) :: a(size(b%AR,1),size(b%AR,2)),RTEMP,Q,degK
 N1=size(b%AR,2) !N1=N 
 M1=size(b%AR,1) !M1=MM
 allocate (spline%r(M1),spline%z(M1),spline%zp2(M1),spline%mvjr(N1))
 associate (t=>spline%r,z=>spline%z,zt2=>spline%zp2,mvjr=>spline%mvjr)
  mvjr=0
  do i=1,N1 
     do j=1,M1
       Q=b%AR(j,i)        
        if (ABS(Q) > 0.) then ! ABS is optional for AR or AP, contrast with AngSpline above
         mvjr(i)=mvjr(i)+1
         t(mvjr(i))=b%DEG(j)*PI/180.0_wp         
         z(mvjr(i))=Q 
        endif
      end do
      call pspli(t,z,mvjr(i),zt2)
      do k=1,M1 
      degK=b%DEG(k)*PI/180.0_wp  
       call SplineEval(1,t,z,zt2,mvjr(i),degK,RTEMP)
        IF(ABS(b%AR(K,I)-RTEMP) > EPS)then
         IF(ABS(b%AR(K,I)) > EPS)THEN
         write(*,*) 'spline error 3 in cornea_arrays fillin',K,I,b%AR(K,I),RTEMP
         endif
        endif 
        a(k,i)=RTEMP
      end do
   end do   
   end associate
   deallocate (spline%r,spline%z,spline%zp2,spline%mvjr)
end function fillin

function lsqfill(b) result(a) 
 use set_precision, ONLY : wp
 TYPE(wpAtlasMatrix),INTENT(IN) :: b
 integer :: M1,N1,i,j,k,l,info,ipvt(M2)
 real(wp) :: a(size(b%AR,1),size(b%AR,2)),t(size(b%AR,1)),z(size(b%AR,1))
 real(wp) :: c(M2),X(M2,size(b%AR,1)),XpX(M2),zpX(M2),XTX(M2,M2)
 logical :: Q
! real (wp) res(size(b%AR,1)),respres,sumr2,zpz
 N1=size(b%AR,2) !N1=N 
 M1=size(b%AR,1) !M1=MM
 z=0
 t=0
 a=0
 do i=1,N1
  XpX=0
  zpX=0 
  c=0  
! X is cosine terms of fourier, t are angles, Z are radii for current ring
  do j=1,M2
   do k=1,M1  
    Q=ABS(b%AR(k,i)) > 0  
    if (Q) then ! means it is  =/ 0
     t(k)=b%DEG(k)*PI/180.0_wp         
     z(k)=b%AR(k,i)
     X(j,k)=cos((j-1)*t(k))    ! cosine series including 0 term
     XpX(j)=XpX(j)+X(j,k)*X(j,k)
     zpX(j)=zpX(j)+z(k)*X(j,k)
    endif      
   end do 
  end do
! X transpose X
  XTX=0
  do j=1,M2
   do l=1,M2 
    do k=1,M1 
     Q=ABS(b%AR(k,i)) > 0  
     if (Q) then ! means it is  =/ 0    
      XTX(j,l)=XTX(j,l)+X(j,k)*X(l,k)
     endif
    end do
   end do
  end do
! get solution fit coefficients c to XTX.c=z.X
  c=0
!  call gauss_2(XTX,zpX,c,M2) ! simple G-J routine
!  call DGESV(M2, 1, XTX, M2, ipvt, zpX, M2, INFO )
! The lapack insertion below from Hanson & Hopkins chapter 2: exampleLapack90.f90
  call dgetrf(M2,M2,XTX,M2,ipvt,info)
! Check that the Lapack routine has been successful
  if (info<0) then
   WRITE (*,'(''Argument '',i3,'' has an illegal value'')') - info
  ELSE IF (info>0) THEN
   WRITE (*,'(''Zero diagonal value detected in upper ''// &
    &                       ''triangular factor at position &
    &'',i7)') info
  ELSE
  call dgetrs('N',M2,1,XTX,M2,ipvt,zpX,M2,info)
! Check that the Lapack routine has been successful
  IF (info<0) THEN
   WRITE (*,'(''Argument '',i3,'' has an illegal value'')') - info
  END IF
  end if
  c=zpX
! generate lsq fillin values
  do k=1,M1
    Q=ABS(b%AR(k,i)) > 0  
    if (Q) then ! means it is  =/ 0
     a(k,i)=b%AR(k,i)   ! retain old values where they exist
    else
     do j=1,M2
      a(k,i)=a(k,i)+c(j)*cos((j-1)*b%DEG(k)*PI/180.0_wp) ! just replace missing values
     end do 
    endif    
  end do
! quality of fit if not only replacing missing values as immediately above
!  res=0
!  respres=0
!  zpz=0
!   do k=1,M1  
!    Q=ABS(b%AR(k,i)) > 0
!    if (Q) then ! means it is  =/ 0 
!     res(k)=z(k)-a(k,i)
!     respres=respres+res(k)*res(k)
!     zpz=zpz+z(k)*z(k)
!    endif
!   end do
!  sumr2=respres/zpz  
!   if (i == 18) then 
!   do k=1,M1
!    Q=ABS(b%AR(k,i)) > 0
!    if (Q) then ! means it is  =/ 0 
!     write(*,*) a(k,i)*cos(t(k)),a(k,i)*sin(t(k)),z(k)*cos(t(k)),z(k)*sin(t(k))
!    else
!     write(*,*) a(k,i)*cos(t(k)),a(k,i)*sin(t(k))
!    endif
!   end do
!   endif     
!   write(*,*) i,sumr2,c           	 	
 end do   
end function lsqfill

function pca(M3,b) result(a) 
 use set_precision, ONLY : wp
 TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
 TYPE(wpRadSlopeMatrix) :: a
 integer, INTENT(IN) :: M3  ! pca terms, 2 or 3
 integer :: M1,N1,i,j,k,l,info,lwork,M
 real(wp) :: X(M3,size(b%r,1)),XTX(M3,M3),work(3*M3),w(M3) 
 logical :: Q
 lwork=size(work)
 N1=size(b%r,2) !N1=N 
 M1=size(b%r,1) !M1=MM

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
 if (ABS(X2) < 20) then 
! UNDEFINED AT ORIGIN X2=0, LIMIT IS RFCT*Y2X             
  SAGC=RFCT*Y2X
 else
  SAGC=RFCT*Y1X/(X2*SQRT(1+Y1X**2))
 endif     
end subroutine AXIALP

! instantaneous "tangential" power using slope/derivatives with and without use of calculated angular derivatives
subroutine instantp(X2,Y1X,Y1T,Y2X,TANC,ZNMEX)
 real(wp), INTENT(IN) :: X2,Y1X,Y1T,Y2X
 real(wp), INTENT(OUT) :: TANC,ZNMEX
 TANC=RFCT*Y2X/(SQRT(1+(Y1X)**2)**3)
! UNDEFINED AT ORIGIN X2=0, LIMIT IS RFCT*Y2X ALSO TANC BECAUSE Y1X = 0 at ORIGIN X2=0 
 if (ABS(X2) < EPS) then
   ZNMEX=TANC    
 else
   ZNMEX=RFCT*Y2X/((1+Y1X**2)*SQRT(1+(Y1T/X2)**2+Y1X**2))
   If (ABS(ZNMEX) < 1) then
    write(*,*) 'Warning ABS(ZNMEX)<1 instantp:X2,Y1X,Y1T,Y2X,TANC,ZNMEX',X2,Y1X,Y1T,Y2X,TANC,ZNMEX
   endif 
 endif
end subroutine instantp 

! mean power of slope/derivatives with use of calculated angular derivatives
subroutine meanp(X1,X2,Y1XIN,Y1T,Y1XT,Y2T,Y2X,ZMM)
 real(wp), INTENT(IN) :: X1,X2,Y1XIN,Y1T,Y1XT,Y2T,Y2X
 real(wp), INTENT(OUT) :: ZMM 
 real(wp) :: ZMX,Y,Y1X
! MONGE MEAN CURVATURE
! WHEN X2<0 X1>PI
  if (ABS(X2) > EPS) then
   IF (X2 < 0 .AND. X1 >= PI) THEN
      Y=X2
      Y1X=Y1XIN      
      ZMM=(2*(1+Y1X**2+Y1T**2/Y**2)**(3/2.)*Y**3)
      ZMX=-2*Y1X*Y1T**2+2*Y1X*Y1XT*Y1T*Y-Y2X*Y1T**2*Y
      ZMX=ZMX-Y2T*Y-Y1X**2*Y2T*Y-Y1X*Y**2-Y1X**3*Y**2-Y2X*Y**3
      ZMM=-RFCT*ZMX/ZMM
   ELSE
      Y=-X2
      Y1X=-Y1XIN
      ZMM=(2*(1+Y1X**2+Y1T**2/Y**2)**(3/2.)*Y**3)
      ZMX=-2*Y1X*Y1T**2+2*Y1X*Y1XT*Y1T*Y-Y2X*Y1T**2*Y
      ZMX=ZMX-Y2T*Y-Y1X**2*Y2T*Y-Y1X*Y**2-Y1X**3*Y**2-Y2X*Y**3
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
! MONGE ASTIG 
! WATCH OUT FOR ZERO AT UMBILICAL POINTS!
! WHEN X2>0 X1<PI
   if (ABS(X2) > EPS) then
     if (X2 > 0 .AND. X1 <= PI) then
       Y=X2
       Y1X=Y1XIN
       ZA1=2*Y1X*Y1T**2-2*Y1X*Y1XT*Y1T*Y+Y2X*Y1T**2*Y
       ZA1=ZA1+Y2T*Y+Y1X**2*Y2T*Y+Y1X*Y**2+Y1X**3*Y**2+Y2X*Y**3
       ZA1=ZA1**2/(4*(Y1T**2+Y**2+Y1X**2*Y**2)**3)
       ZA2=Y1T**2-2*Y1XT*Y1T*Y+Y1XT**2*Y**2
       ZA2=ZA2-Y2X*Y2T*Y**2-Y1X*Y2X*Y**3
       ZA2=ZA2/(Y1T**2+Y**2+Y1X**2*Y**2)**2
       ZA=ABS(ZA1+ZA2)**(-1/2.)
     else
       Y=-X2
       Y1X=-Y1XIN
       ZA1=2*Y1X*Y1T**2-2*Y1X*Y1XT*Y1T*Y+Y2X*Y1T**2*Y
       ZA1=ZA1+Y2T*Y+Y1X**2*Y2T*Y+Y1X*Y**2+Y1X**3*Y**2+Y2X*Y**3
       ZA1=ZA1**2/(4*(Y1T**2+Y**2+Y1X**2*Y**2)**3)
       ZA2=Y1T**2-2*Y1XT*Y1T*Y+Y1XT**2*Y**2
       ZA2=ZA2-Y2X*Y2T*Y**2-Y1X*Y2X*Y**3
       ZA2=ZA2/(Y1T**2+Y**2+Y1X**2*Y**2)**2
       ZA=ABS(ZA1+ZA2)**(-1/2.)
     endif
     else
!    UNDEFINED AT ORIGIN X2=0          
       ZA=1E30_wp  
     endif
end subroutine mongea

! Lines of Curvature
subroutine LIOC(X1,X2,Y1X,Y1T,UPOS,VPOS,UTPOS,VTPOS)
   real(wp), intent(in) :: X1,X2,Y1X,Y1T
   real(wp), intent(out) :: UPOS,VPOS,UTPOS,VTPOS      
!  CARTESIAN TANGENT VECTOR COMPONENTS (-UTPOS,-VTPOS,1)  
   if (ABS(X2) > EPS) then
     if (X2 > 0) then     
        UPOS=X2*COS(X1)
        VPOS=X2*SIN(X1)
        UTPOS=Y1X*COS(X1)-Y1T*SIN(X1)/X2
        VTPOS=Y1X*SIN(X1)+Y1T*COS(X1)/X2 
     else
        UPOS=-X2*COS(X1)
        VPOS=-X2*SIN(X1)
        UTPOS=-Y1X*COS(X1)+Y1T*SIN(X1)/X2
        VTPOS=-Y1X*SIN(X1)-Y1T*COS(X1)/X2         
     endif
     else
!    UNDEFINED AT ORIGIN X2=0          
        UPOS=0._wp
        VPOS=0._wp
        UTPOS=0._wp
        VTPOS=0._wp     
     endif        
end subroutine LIOC

! instantaneous "tangential" power and mean power in terms of axial/"sagittal" power,radius and radial derivative of axial power 
 subroutine sagc2(X2,SAGC,DSAGC,TANC,ZMM)  
  real(wp), INTENT(IN) :: X2,SAGC,DSAGC
  real(wp), INTENT(OUT) :: TANC,ZMM   
  TANC=SAGC+X2*DSAGC
  ZMM=0.5_wp*(SAGC+TANC)
  end subroutine sagc2

END MODULE cornea_arrays


