MODULE cornea_arrays

 USE set_precision, ONLY : wp
 USE LapackInterface, ONLY : dgetrf, dgetrs
 USE spline_interfaces 
 REAL(wp), PARAMETER :: PI=3.1415926535897932384626433832795_wp
 REAL(wp), PARAMETER :: RFCT=33750.0_wp
 REAL(wp), PARAMETER :: EPS=0.0000001_wp  ! used in pspli and SplineCenter
 INTEGER, PARAMETER :: MM=180, N=22 
! INTEGER, PARAMETER :: MM=360, N=16
 integer, PARAMETER :: M=5 ! augmented multiplier for number of rings
 integer, PARAMETER :: M2=3 ! lsq fourier cosine series terms
 
! Defining common data arrays
 
 TYPE wpEyeSysMatrix
   REAL (wp), ALLOCATABLE :: RA(:,:), XX(:,:)
   INTEGER, ALLOCATABLE :: DEG(:)
 END TYPE wpEyeSysMatrix
 
 TYPE wpRadSlopeMatrix
   REAL (wp), ALLOCATABLE :: thta(:), r(:,:), Zp(:,:), Zp2(:,:), Zt2(:,:)
   INTEGER, ALLOCATABLE :: MV(:)
 END TYPE wpRadSlopeMatrix
 
 TYPE wpAtlasMatrix
   REAL (wp), ALLOCATABLE :: AR(:,:),AD(:,:),AP(:,:),AY(:,:),DEG(:),AR2(:,:)
 END TYPE wpAtlasMatrix
 
 TYPE wpDiaSlopeMatrix
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
 !Type(wpAtlasMatrix) = Type(wpRadSlopeMatrix) converts to rhs to lhs
 MODULE PROCEDURE Atlas_eq_RadSlope
END INTERFACE
 
INTERFACE ASSIGNMENT (=)
  !Type(wpRadSlopeMatrix)=Type(wpEyeSysMatrix) converts to rhs to lhs
 MODULE PROCEDURE RadSlope_eq_EyeSys
END INTERFACE

INTERFACE ASSIGNMENT (=)
 !Type(wpRadSlopeMatrix)=Type(wpAtlasMatrix) converts to rhs to lhs
 MODULE PROCEDURE RadSlope_eq_Atlas
END INTERFACE 

INTERFACE ASSIGNMENT (=)
 !Type(wpDiaSlopeMatrix)=Type(wpRadSlopeMatrix) converts to rhs to lhs
 MODULE PROCEDURE DiaSlope_eq_RadSlope
END INTERFACE 

INTERFACE ASSIGNMENT (=)
 !Type(wpRadSlopeMatrix)=Type(wpDiaSlopeMatrix) converts to rhs to lhs
 MODULE PROCEDURE RadSlope_eq_DiaSlope
END INTERFACE 

INTERFACE ASSIGNMENT (=)
 ! Type(oneofthesebelow) = INTEGER(0) deallocates the matrix
 MODULE PROCEDURE destroy_EyeSys
 MODULE PROCEDURE destroy_Atlas
 MODULE PROCEDURE destroy_RadSlope
 MODULE PROCEDURE destroy_DiaSlope
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

! declaring common data arrays

 real(wp) :: RadSplineCenter(MM)
 TYPE(wpEyeSysMatrix) :: EyeSys
 TYPE(wpRadSlopeMatrix) :: RadSlope
 TYPE(wpAtlasMatrix) :: Atlas
 TYPE(wpDiaSlopeMatrix) :: DiaSlope
 TYPE(wpRadSlopeMatrix) :: ARadSlope
 TYPE(wpDiaSlopeMatrix) :: ADiaSlope

 CONTAINS
 
subroutine init_mat(MM,N,EyeSys,Atlas,RadSlope,DiaSlope) ! allocate arrays
  INTEGER, INTENT(IN) :: MM,N
  TYPE(wpEyeSysMatrix) :: EyeSys
  TYPE(wpRadSlopeMatrix) :: RadSlope  
  TYPE(wpAtlasMatrix) :: Atlas
  TYPE(wpDiaSlopeMatrix) :: DiaSlope  
  allocate (EyeSys%RA(MM,N),EyeSys%XX(MM,N),EyeSys%DEG(MM))
  allocate (RadSlope%r(MM,N),RadSlope%Zp(MM,N),RadSlope%Zp2(MM,N),&
            Radslope%Zt2(MM,N),RadSlope%thta(MM),RadSlope%MV(MM))  
  allocate (DiaSlope%rd(MM/2,2*N),DiaSlope%Zpd(MM/2,2*N),&
            DiaSlope%Zpd2(MM/2,2*N),DiaSlope%L2(MM/2))
  allocate (DiaSlope%rOutMax(MM/2),DiaSlope%rInMax(MM/2),&
            DiaSlope%rOutMin(MM/2),DiaSlope%rInMin(MM/2))
  allocate (Atlas%AR(MM,N),Atlas%AD(MM,N),Atlas%AP(MM,N),&
            Atlas%AY(MM,N),Atlas%DEG(MM),Atlas%AR2(MM,N))
end subroutine init_mat

subroutine init_augmented_mat(MM,N,M,ARadSlope,ADiaSlope) !allocate augmented arrays
  INTEGER, INTENT(IN) :: MM,N,M
  TYPE(wpRadSlopeMatrix) :: ARadSlope  
  TYPE(wpDiaSlopeMatrix) :: ADiaSlope  
  allocate (ARadSlope%r(MM,N*M),ARadSlope%Zp(MM,N*M),ARadSlope%Zp2(MM,N*M),&
            ARadSlope%Zt2(MM,N*M),ARadSlope%thta(MM),ARadSlope%MV(MM))  
  allocate (ADiaSlope%rd(MM/2,2*N*M),ADiaSlope%Zpd(MM/2,2*N*M),&
            ADiaSlope%Zpd2(MM/2,2*N*M),ADiaSlope%L2(MM/2))
  allocate (ADiaSlope%rOutMax(MM/2),ADiaSlope%rInMax(MM/2),&
            ADiaSlope%rOutMin(MM/2),ADiaSlope%rInMin(MM/2))
end subroutine init_augmented_mat

!Type(wpEyeSysMatrix)=INTEGER(0) deallocates matrix 
subroutine destroy_EyeSys(EyeSys,iflag)
  TYPE(wpEyeSysMatrix), INTENT(INOUT) :: EyeSys
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (EyeSys%RA,EyeSys%XX,EyeSys%DEG)
  ENDIF
end subroutine destroy_EyeSys

!Type(wpAtlasMatrix)=INTEGER(0) deallocates matrix
subroutine destroy_Atlas(Atlas,iflag)
  TYPE(wpAtlasMatrix), INTENT(INOUT) :: Atlas
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (Atlas%AR,Atlas%AD,Atlas%AP,Atlas%AY,Atlas%DEG,Atlas%AR2)
  ENDIF
end subroutine destroy_Atlas

!Type(wpRadSlopeMatrix)=INTEGER(0) deallocates matrix
subroutine destroy_RadSlope(RadSlope,iflag)
  TYPE(wpRadSlopeMatrix), INTENT(INOUT) :: RadSlope
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (RadSlope%r,RadSlope%Zp,RadSlope%Zp2)
  deallocate (RadSlope%thta,RadSlope%MV)
  ENDIF
end subroutine destroy_RadSlope

!Type(wpDiaSlopeMatrix)=INTEGER(0) deallocates matrix
subroutine destroy_DiaSlope(DiaSlope,iflag)
  TYPE(wpDiaSlopeMatrix), INTENT(INOUT) :: DiaSlope
  INTEGER, INTENT (IN) :: iflag 
  IF (iflag==0) THEN
  deallocate (DiaSlope%rd,DiaSlope%Zpd,DiaSlope%Zpd2,DiaSlope%L2)
  deallocate (DiaSlope%rOutMax,DiaSlope%rInMax,DiaSlope%rOutMin,DiaSlope%rInMin)
  ENDIF
end subroutine destroy_DiaSlope

!!array conversion routines

! uses ZFCT converts lhs to rhs
subroutine RadSlope_eq_EyeSys(RadSlope,EyeSys)
  TYPE(wpEyeSysMatrix) :: EyeSys
  TYPE(wpRadSlopeMatrix) :: RadSlope
  INTEGER :: i,j
  INTEGER :: imv(MM)
  REAL(wp) :: ZIX,ZJX,YA1,YA2,YA3,X2A1
  do i=1,MM
    imv(i)=0
    RadSlope%thta(i)=PI*EyeSys%DEG(i)/180.0_wp  ! RadSlope%thta(i)=PI*(i-1)/180.0_wp should always be true for EyeSys
      do j=1,N
       ZIX=EyeSys%XX(i,j)
       ZJX=EyeSys%RA(i,j)
       if (ZIX > 0 .AND. ZJX > 0) then
        imv(i)=imv(i)+1
        call ZFCT(i,ZJX,ZIX,X2A1,YA3)
        RadSlope%Zp(i,imv(i))=YA3
        RadSlope%r(i,imv(i))=X2A1
        else
        RadSlope%Zp(i,j)=0._wp  ! sets border
       endif
        RadSlope%Zp2(i,j)=1/803.0_wp ! fallback value before splining       
      end do
      RadSlope%MV(i)=imv(i)
   end do
end subroutine RadSlope_eq_EyeSys

! aka SLOPE2POWER using AXIALP converts lhs to rhs
subroutine Atlas_eq_RadSlope(Atlas,RadSlope)
  TYPE(wpRadSlopeMatrix) :: RadSlope
  TYPE(wpAtlasMatrix) :: Atlas
  INTEGER :: i,j
  INTEGER :: imv(MM)
  REAL(wp) :: X1,X2,Y,YP,Y2X,POW
  imv=0
  Atlas%AP=0._wp
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
subroutine RadSlope_eq_Atlas(RadSlope,Atlas)
  TYPE(wpRadSlopeMatrix) :: RadSlope
  TYPE(wpAtlasMatrix) :: Atlas
  REAL(wp) :: ZIX,ZJX,YA1,YA2,YA3,X2A1
  REAL(wp) :: DIST,R,POW
  INTEGER :: i,j, imv(MM),M ,IMV2(MM)
    imv=0
    do i=1,MM
     RadSlope%thta(i)=PI*Atlas%DEG(i)/180.0_wp  
     ! RadSlope%thta(i)=PI*2*(i-1)/180_wp should always be true for "real" Atlas
     ! RadSlope%thta(i)=PI*(i-1)/180_wp should always be true for EyeSys generated Atlas
     do j=1,N
      if ((Atlas%AP(i,j) > 0) .AND. (Atlas%AR(i,j) > 0)) then    ! Only for Atlas with POW /= 0 
       imv(i)=imv(i)+1      
       DIST=Atlas%AD(i,imv(i))
       R=Atlas%AR(i,imv(i))
       POW=Atlas%AP(i,imv(i))
       ZIX=RFCT/POW
!      could use DIST here
       ZJX=R*100
       CALL ZFCT(i,ZJX,ZIX,X2A1,YA3)
        RadSlope%r(i,imv(i))=X2A1
        RadSlope%Zp(i,imv(i))=YA3
      endif 
        RadSlope%Zp2(i,j)=1/803.0_wp ! fallback value before splining  
     end do
     RadSlope%MV(i)=imv(i)
    end do   
end subroutine RadSlope_eq_Atlas

subroutine DiaSlope_eq_RadSlope(DiaSlope,RadSlope)
 TYPE(wpDiaSlopeMatrix) :: DiaSlope
 TYPE(wpRadSlopeMatrix) :: RadSlope
 integer :: i,j
 integer :: M1,N1
 real(wp) :: rB
 ASSOCIATE(MV=>RadSlope%MV,rOMIN=>DiaSlope%rOutMin,rIMIN=>DiaSlope%rInMin,&
                           rOMAX=>DiaSlope%rOutMax,rIMAX=>DiaSlope%rInMax)
   M1=size(DiaSlope%rd,1) !M1=MM/2
   N1=size(DiaSlope%rd,2) !N1=2*N for "regular" DiaSlope, N1=2*N*M for augmented 
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
       DiaSlope%rd(i,j)=RadSlope%r(i+M1,MV(i+M1)-j+1)
       DiaSlope%Zpd(i,j)=RadSlope%Zp(i+M1,MV(i+M1)-j+1)
       DiaSlope%Zpd2(i,j)=RadSlope%Zp2(i+M1,MV(i+M1)-j+1)
!      FIND BOUNDS          
       rB=DiaSlope%rd(i,j) 
       if (rB <= rOMIN(i)) rOMIN(i)=rB
       if (rB >= rIMIN(i)) rIMIN(i)=rB                    
      endif
      if (j <= MV(i)) then
       DiaSlope%rd(i,j+MV(i+M1))=RadSlope%r(i,j)
       DiaSlope%Zpd(i,j+MV(i+M1))=RadSlope%Zp(i,j)
       DiaSlope%Zpd2(i,j+MV(i+M1))=RadSlope%Zp2(i,j)      
!      FIND BOUNDS          
       rB=DiaSlope%rd(i,j+MV(i+M1))
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
   M1=size(DiaSlope%rd,1) !M1=MM/2
   N1=size(DiaSlope%rd,2) !N1=2*N for "regular" DiaSlope, N1=2*N*M for augmented
   do i=1,M1
    do j=1,N1
      if (j <= MV(i+M1)) then
       RadSlope%r(i+M1,MV(i+M1)-j+1)=DiaSlope%rd(i,j)
       RadSlope%Zp(i+M1,MV(i+M1)-j+1)=DiaSlope%Zpd(i,j)
       RadSlope%Zp2(i+M1,MV(i+M1)-j+1)=DiaSlope%Zpd2(i,j)
      endif
      if (j <= MV(i)) then
       RadSlope%r(i,j)=DiaSlope%rd(i,j+MV(i+M1))
       RadSlope%Zp(i,j)=DiaSlope%Zpd(i,j+MV(i+M1))
       RadSlope%Zp2(i,j)=DiaSlope%Zpd2(i,j+MV(i+M1))
      endif
    end do
   end do
 end ASSOCIATE   
end subroutine RadSlope_eq_DiaSlope

function DiaSpline(b) result(a) 
 TYPE(wpDiaSlopeMatrix),INTENT(IN) :: b
 integer :: M1
 real(wp) :: a(size(b%rd,1),size(b%rd,2))  
 N1=size(b%rd,2) !N1=2*N*M for augmented
 M1=size(b%rd,1) !M1=MM/2
  do i=1,M1 
   call nspline(b%rd(i,:),b%Zpd(i,:),b%L2(i),a(i,:)) 	 
  end do
end function DiaSpline

function DiaSplineCenter(b) result(a) 
 TYPE(wpDiaSlopeMatrix),INTENT(IN) :: b
 integer :: M1
 real(wp) :: a(size(b%rd,1),size(b%rd,2))  
 N1=size(b%rd,2) !N1=2*N*M for augmented
 M1=size(b%rd,1) !M1=MM/2
  do i=1,M1 
   call nsplineCenter(b%rd(i,:),b%Zpd(i,:),b%L2(i),a(i,:)) 	 
  end do
end function DiaSplineCenter

function DiaIntegrate(b) result(a)
 TYPE(wpDiaSlopeMatrix),INTENT(IN) :: b
 integer :: i,j,M1
 real(wp) :: Q,Q0
 real(wp) :: a(size(b%rd,1),size(b%rd,2))
 N1=size(b%rd,2) !N1=2*N*M for augmented
 M1=size(b%rd,1) !M1=MM/2)
  do i=1,M1
   call CubicSplineQuad(b%rd(i,:),b%Zpd(i,:),b%Zpd2(i,:),b%L2(i),0._wp,Q0)
   do j=1,b%L2(i)    
    call CubicSplineQuad(b%rd(i,:),b%Zpd(i,:),b%Zpd2(i,:),b%L2(i),b%rd(i,j),Q)
    a(i,j)=Q-Q0
   end do
  end do
end function DiaIntegrate

function RadInterpolate(b) result(a) !interpolates values of radslope in new rings
 TYPE(wpRadSlopeMatrix),INTENT(IN) :: b  
 integer :: i,j,M1
 real(wp) :: f0
 real(wp) :: a(size(b%r,1),size(b%r,2))
 N1=size(b%r,2) !N1=N or N*M for ARadSlope
 M1=size(b%r,1) !M1=MM
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
 real(wp) :: a(2*size(b%rd,1),size(b%rd,2)/2) ! RadSlope size rings and radii
 real(wp) :: rBi,rBo
 integer :: M1,N1,i,j 
 M1=2*size(b%rd,1) !M1=MM convert DiaSlope dimensions to RadSlope
 N1=size(b%rd,2)/2 !N1=N convert "regular" Diaslope to RadSlope, N1=N*M for augmented
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
 real(wp) :: a(2*size(b%rd,1),size(b%rd,2)/2) ! RadSlope size rings
 real(wp) :: rBi(size(b%rd,1)),rBo(size(b%rd,1))
 integer :: M1,N1,i,j 
 ASSOCIATE(rOMIN=>b%rOutMin,rIMIN=>b%rInMin,&
           rOMAX=>b%rOutMax,rIMax=>b%rInMax)
   M1=size(b%rd,1)   !M1=MM/2
   N1=size(b%rd,2)/2 !N1=N convert "regular" Diaslope to RadSlope, N1=N*M for augmented
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

function Normalize(b) result(a) ! puts b on unit circle
 TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
 TYPE(wpRadSlopeMatrix) :: a
 real(wp) :: rBo
 integer :: M1,N1,i,j 
 N1=size(b%r,2) !N1=N 
 M1=size(b%r,1) !M1=MM
 allocate (a%r(MM,N),a%Zp(MM,N),a%Zp2(MM,N),&
            a%Zt2(MM,N),a%thta(MM),a%MV(MM))
 rBo=-1E30
  do i=1,N1 
   do j=1,M1
    if (ABS(b%r(j,i)) >= rBo) rBo=ABS(b%r(j,i)) !find maximum radius
   end do
  end do
  a%r(:,:)=b%r(:,:)/rBo
  a%Zp(:,:)=b%Zp(:,:)/rBo 
  a%thta(:)=b%thta(:) 
end function Normalize

function AngSpline(b) result(a) 
 TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
 TYPE(wpsplinevect) :: spline
 integer :: M1,N1
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
 integer :: M1,N1
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
        IF(ABS(b%AR(K,I)-RTEMP).GT.1E-5)then
         IF(b%AR(K,I).NE.0)THEN
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
 integer :: M1,N1,i,j,k,info,ipvt(M2)
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

!  finds MV based on Atlas%AR and Atlas%AP
subroutine refineborders(Atlas,RadSlope)
 TYPE(wpAtlasMatrix) :: Atlas
 TYPE(wpRadSlopeMatrix) :: RadSlope
 INTEGER :: i,j,IZ
 INTEGER :: imv(MM)
 REAL(wp) :: R,POW        
! initialize
  IZ=1 
  call initborders(Atlas,imv)
  do while (IZ == 1)
!  SAVE MV(MM)
   do i=1,MM
    RadSlope%MV(i)=imv(i)
   end do
!  REFINE BOUNDARY same as init borders with RadSlope%MV(i) instead of N
   do i=1,MM
    imv(i)=0
    do j=1,RadSlope%MV(i)       
!    BOUNDS CHECKING 
     R=Atlas%AR(i,j)
     POW=Atlas%AP(i,j)                     
     if(POW > 0 .AND. R > 0) then                                       
       imv(I)=imv(I)+1        
     endif
     end do 	 	 		    		       
   end do
!   Are we done
       IZ=0           
       do i=1,MM
        if ((RadSlope%MV(i)-imv(i)) /= 0) then
         IZ=1
        endif
       end do
  end do
end subroutine refineborders

!  finds initial MV based on Atlas%AR and Atlas%AP
subroutine initborders(Atlas,imv)
 TYPE(wpAtlasMatrix) :: Atlas
 INTEGER :: i,j
 INTEGER, INTENT(OUT):: imv(MM)
 REAL(wp) :: R,POW
   do i=1,MM
    imv(i)=0
    do j=1,N       
!    BOUNDS CHECKING 
     R=Atlas%AR(i,j)
     POW=Atlas%AP(i,j)                     
     if(POW > 0 .AND. R > 0) then                                       
       imv(i)=imv(i)+1        
     endif
     end do 	 	 		    		       
   end do
end subroutine initborders
 

!! corneal calculation subroutines

! signed slope and radius from eyesys style data, or ZIX=RFCT/POW, POW is axial power from Atlas style data
subroutine ZFCT(ITH,ZJX,ZIX,X2A1,YA3)
 REAL(wp), INTENT(IN) :: ZIX,ZJX
 REAL(wp), INTENT(OUT) :: YA3,X2A1
 REAL(wp) :: YA1,YA2
 INTEGER, INTENT(IN) :: ITH
 !     INVERSE IS AXIALP	
  if (ZIX <= ZJX) WRITE (*,*) 'ERROR IN ARCTAN'
 !     CONVERTS ZIX TO DZ/DR
      YA1=ZJX/(ZIX-ZJX)
      YA2=ZJX/(ZIX+ZJX)
      if (ITH > (MM/2)) then
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
 if (X2 == 0) then
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
 real(wp) :: ZMX 
! MONGE MEAN CURVATURE
! WHEN X2<0 X1>PI
  IF (X2 /= 0) THEN
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
  real(wp) :: ZA1,ZA2,Y
! MONGE ASTIG 
! WATCH OUT FOR ZERO AT UMBILICAL POINTS!
! WHEN X2>0 X1<PI
    if (X2 /= 0) then
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
   if (X2 /= 0) then
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


