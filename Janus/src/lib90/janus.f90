  PROGRAM janus
! DRIVER PROGRAM FOR SPLINE ROUTINES
  USE set_precision, ONLY : wp
  USE cornea_arrays
  TYPE(wpRadSlopeMatrix) :: atmp
  integer IMV(MM), IZ, i
! character(len=*), intent(in) :: InputDataFile
  character(len=8)::  InputDataFile       
  character(len=16) :: AxialPowerDataKnots
  character(len=8) :: BigGrainyPlot
  character(len=7) :: BigPlot
  character(len=8) :: LinesOfCurv     
  integer ::  IuseG, IuseF, MV(MM)
  real :: time_start, time_end, t(10)
  real(wp) :: POWMIN,POWMAX,POWMIN2,POWMAX2
  
  INTERFACE
    SUBROUTINE fillarray(IuseG,KX1,POWMIN,POWMAX)
!     COMPUTES ATLAS DATA 
!     IuseG to select what to place in RadSlope%Zp AND/OR compute LIOC
      USE cornea_arrays, ONLY : MM,N,RadSlope,AxialP,sagc2,instantp,meanp,mongea,lioc
      USE set_precision, ONLY : wp
      USE spline_interfaces, ONLY : SplineEval1Dx1D
      use,intrinsic :: ieee_arithmetic
      integer, intent(in) :: IuseG 
      character(len=*), intent(in) :: KX1     
      real(wp), intent(out) :: POWMIN, POWMAX
    END SUBROUTINE

    SUBROUTINE WriteOFF(b,KXNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
      character(len=*), intent(in) :: KXNAME 
    END SUBROUTINE  
    
    subroutine WriteCenter(b,KXNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      TYPE(wpRadSlopeMatrix),INTENT(IN) :: b 
      character(len=*), intent(in) :: KXNAME   
    end subroutine      

    SUBROUTINE WRITEARRAY(b,KXNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
      character(len=*), intent(in) :: KXNAME 
    END SUBROUTINE

    SUBROUTINE RCNVRTE(RANAME,XXNAME)
!    EYESYS VERSION
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : EyeSys,N,MM
     character(len=*), intent(in) :: RANAME,XXNAME
    END SUBROUTINE

    subroutine RCNVRTA(KXNAME)
!    ATLAS VERSION
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : Atlas
     CHARACTER*80 KH1,KH2,KH3
     character(len=*), intent(in) :: KXNAME
    end subroutine

    SUBROUTINE PRINTGRAPH(POWMIN,POWMAX,FILENAME)
     use set_precision, only : wp
     REAL(wp), INTENT(IN) :: POWMIN, POWMAX
     character(len=*), intent(in) :: FILENAME
    END SUBROUTINE
    
  END INTERFACE
  
  InputDataFile='TEST.CSV'
  AxialPowerDataKnots='RCNVRTA.ORIG.CAR'
  BigGrainyPlot='BIGG.CAR'
  BigPlot='BIG.CAR'
  LinesOfCurv='LIOC.CAR'
               
  call init_mat(MM,N,EyeSys,Atlas,RadSlope,DiaSlope)  ! initialize the arrays
       
! READ THE EYESYS DATA
! XX????? ARE THE AXIAL DIST. RX???? ARE THE MIRE RADII
  call CPU_TIME(time_start)
!!  call RCNVRTE('RA.DAT','XX.DAT')
! Generate the slope matrix using ZFCT and "Atlas" data from it with AXIALP       
!!  RadSlope=EyeSys
!!  Atlas=RadSlope    
  call CPU_TIME(time_end)
  t(1)=time_end-time_start
  write(*,*) 'Time to read files: ',t(1)*1000
! Done with EyeSys data
  EyeSys=0
     
! READ THE ATLAS DATA
! R OR DIST ARE THE MIRE RADII, USING DIST
  call CPU_TIME(time_start)
  CALL RCNVRTA(InputDataFile)

! READ/GENERATE TEST DATA (ATLAS STYLE)
!!  CALL RCNVRTT
  RadSlope=Atlas
  call CPU_TIME(time_end)
  t(1)=time_end-time_start
  write(*,*) 'Time to read files: ',t(1)*1000
  
! Here IuseG changes the contents of RadSlope via FillArray
! IuseG == -1 import slopes, return SAGC (axialp), no splining necessary
!  IuseG == 0  import SAGC, return SAGC (do nothing), no splining necessary
!  IuseG == 1  import SAGC, return TANC (instantp2)  
!  IuseG == 2  import SAGC, return ZMM (meanp2)

!  IuseG == 0  import slopes, return slopes (do nothing), no splining necessary
!  IuseG == 3  import slopes, return TANC (instantp)
!  IuseG == 4  import slopes, return ZNMEX (instantp)
!  IuseG == 5  import slopes, return ZMM (meanp)
!  IuseG == 6  import slopes, return ZA (mongea)
!  IuseG == 7  import slopes, return Y (elevation)
!  IuseG == 8  import slopes, compute LIOC
!  IuseG == 9  import slopes, return slopes from splining

! (IuseG=-1) then IuseG=0,1 or 2), IuseF=0-> intdifM
! (IuseG=0)  then IuseG=(3..7) , IuseF=0-> intdifR
! (IuseG=0)  then IuseG=(3..7),  IuseF=1-> intdifZ    !DON'T DO THIS 
   
  IuseG=0  ! IuseG=-1 or 0 here only, presplining; 0 just finds POWMIN/MAX can skip entirely here
  CALL FILLARRAY(IuseG,LinesOfCurv,POWMIN,POWMAX) 
  IuseG=4  ! if above IuseG=-1, then change to 0, 1 or 2  ! IuseG=0 then change to 3 through 8

! get rid of holes/find RadSlope%MV based on Atlas array data
  call CPU_TIME(time_start)
  call refineborders(Atlas,RadSlope) ! substitute operator .b. or something: only affects MV in RadSlope
  call CPU_TIME(time_end)
  t(2)=time_end-time_start
  write(*,*) 'Time to refine borders: ',t(2)*1000 
    
  IuseF=0 ! only valid approach is IuseF=0 because    
!  R is not constant; they're not circles, so splining along the curve gives curvatures that
!  are not orthogonal to R, nor z2(deriv of theta)  probably best not to do this
  if (IuseF == 1) then ! partial Atlas or full Atlas via FILL IN MISSING RING DATA USING CIRCUMFERENTIAL SPLINES
    call CPU_TIME(time_start)
!    call fillin2 ! fills in AP and AR
!    Atlas%AR2 = .n. Atlas ! fills in second derivatives of r=Atlas%AR, easy to modify to fill in AR like fillin2
!    Atlas%AR = .n. Atlas ! fills in Atlas%AR  also need to modify commented line in fillin in cornea_arrays
    Atlas%AR = lsqfill(Atlas) ! uses lsq fit with cosine series instead of spline
    call CPU_TIME(time_end)
    t(3)=time_end-time_start
    write(*,*) 'Time to run fillin: ',t(3)*1000
!  filling in AR is better by LSQ fit in missing section; look at these intersecting rings using
!  gnuplot plot 'datafile dumped with >' u 1:2  (don't set polar) first option, or splot second option
   do j=1,N
    do i=1,MM
!     write(*,*) Atlas%DEG(i),Atlas%AR(i,j)
!     write(*,*) Atlas%AR(i,j)*COS(PI*Atlas%DEG(i)/180.0),Atlas%AR(i,j)*SIN(PI*Atlas%DEG(i)/180.0),0
    end do
!    write(*,*) ' '
   end do 
!  stop
   if (IuseG > 2) then  ! don't do if axial powers not slopes    
    call CPU_TIME(time_start)
    RadSlope=Atlas ! recalculate RadSlope based on filled-in Atlas, including MV
    call CPU_TIME(time_end)
    t(4)=time_end-time_start
    write(*,*) 'Time to run radslope: ',t(4)*1000
   endif 
  endif
! done with Atlas
!  Atlas=0  
      
! GENERATE RADIAL SPLINES ACROSS CENTER
  call CPU_TIME(time_start)
  DiaSlope=RadSlope              ! move to diagonal format
  DiaSlope%Zpd2 = .n. DiaSlope   ! generate the splines diagonally (generate zp2)
!  DiaSlope%Zpd2 = DiaSplineCenter(DiaSlope)   ! generate the splines diagonally with center node added (slopes only)
  call CPU_TIME(time_end)
  t(5)=time_end-time_start
  write(*,*) 'Time to run splines: ',t(5)*1000
 
! Rewrite RadSlope with round rings and new values 

! roadmap: now generate round rings, not at previous knots
! generate new Rs using rOMIN, rOMAX, riMIN, riMAX but they have to be constant with theta
! get a global value for those four to generate R's no ORIGIN to avoid singularity
  RadSlope%r=make_rings(DiaSlope,.FALSE.)
!  RadSlope%r=make_bad_rings(DiaSlope,.FALSE.)
!  use fillarray to fill DiaSlope Zp with calculated value based on IuseG, optionally generate LIOC
!  using SplineEval1Dx1D to refill a new matrix RadSlope using f0, derivatives to get calculated powers

!  plot 'LIOC.CAR' using 1:2:3:4 with vectors
   call FILLARRAY(8,LinesOfCurv,POWMIN2,POWMAX2)    ! don't redo bounds consider optional 
!  WriteCenter shows where the spline of slopes is zero, it should be close to zero   
!  use with polar plot, has to come after SplineEval1Dx1D is called, ie. 'set polar' then plot 'Center.dat'
   call WriteCenter(RadSlope,'Center.dat')   ! biggest deviation with nSplineCenter zero slope forced at origin, 
                                             ! then with zero slope forced at average (r(low)+r(high))/2.0
                                             ! smallest deviation without nSplineCenter   
!  write an OFF file
  MV(:)=RadSlope%MV(:) ! store a copy
  RadSlope%MV(:)=N   !full diameters for elevation for Zernicke
  call FILLARRAY(7,LinesOfCurv,POWMIN,POWMAX)
  atmp=Normalize(RadSlope) !allocates atmp, normalizes RadSlope
  call WriteOFF(RadSlope,'elevation.off')
  call WriteOFF(atmp,'elevation1.off')
  RadSlope%MV(:)=MV(:)  ! restore
  x=ZernickeC(atmp,0,4)
  write (*,*) 'c0,4: ',x ! compute Zernicke coefficient
  x=ZernickeC(atmp,1,2)
  write (*,*) 'c12: ',x ! compute Zernicke coefficient
  x=ZernickeC(atmp,2,2)
  write (*,*) 'c22: ',x ! compute Zernicke coefficient
  x=ZernickeC(atmp,2,3)
  write (*,*) 'c23: ',x ! compute Zernicke coefficient
  x=ZernickeC(atmp,2-2)
  write (*,*) 'c2,-2: ',x ! compute Zernicke coefficient

  pause
  write(*,*)  pcafill2(RadSlope) ! these may have an io error also!
  write (*,*) pcafill(RadSlope)
  atmp=0 ! deallocate
  
  call init_augmented_mat(MM,N,M,ARadSlope,ADiaSlope) ! prepare more space
    
! make more than one plot  
  do i=1,2
  if (i==1) then
   write(*,*) 'Plot: ',i  
   call CPU_TIME(time_start)  
   call FILLARRAY(IuseG,LinesOfCurv,POWMIN,POWMAX)     
   call CPU_TIME(time_end)
   t(6)=time_end-time_start
   write(*,*) 'Time to rewrite RadSlope without origin: ',t(6)*1000    
  endif
  if (i==2) then
   write(*,*) 'Next Plot: ',i
   call CPU_TIME(time_start)  
   RadSlope=Atlas
   call refineborders(Atlas,RadSlope)  
   DiaSlope=RadSlope            
!   DiaSlope%Zpd2 = .n. DiaSlope
   DiaSlope%Zpd2 = DiaSplineCenter(DiaSlope)   ! generate the splines diagonally with center node added (slopes only)
   RadSlope%r=make_rings(DiaSlope,.FALSE.)            
   call FILLARRAY(7,LinesOfCurv,POWMIN2,POWMAX2)  ! generate elevation
   DiaSlope=RadSlope             
   DiaSlope%Zpd2 = .n. DiaSlope 
   RadSlope%r=make_rings(DiaSlope,.FALSE.)
   call FILLARRAY(14,LinesOfCurv,POWMIN2,POWMAX2) ! get derivatives from elevation 14 is the same as 4 but should be grainy
   call CPU_TIME(time_end)
   t(6)=time_end-time_start
   write(*,*) 'Time to re-generate a new RadSlope/DiaSlope from Atlas: ',t(6)*1000   
  endif   

! REGENERATE SPLINES ACROSS CENTER (repeating because new values in RadSlope and round rings)
  call CPU_TIME(time_start)  
  DiaSlope=RadSlope            ! move to diagonal; wipes out the original DiaSlope
  DiaSlope%Zpd2 = .n. DiaSlope ! generate the splines diagonally (generate zp2)     
  call CPU_TIME(time_end)
  t(7)=time_end-time_start
  write(*,*) 'Time to re-run splines: ',t(7)*1000

  call CPU_TIME(time_start)
! load bounds
  ADiaSlope%rOutMin=DiaSlope%rOutMin
  ADiaSlope%rInMin=DiaSlope%rInMin
  ADiaSlope%rOutMax=DiaSlope%rOutMax
  ADiaSlope%rInMax=DiaSlope%rInMax
! generate new Rs using rOMIN, rOMAX, but they have to be constant with theta
! get a global value for those two to generate R's INCLUDING ORIGIN and using ADiaSlope  
  ARadSlope%r=make_rings(ADiaSlope,.TRUE.)
! load angles  
  ARadSlope%thta=RadSlope%thta
! load bounds x expansion
  ARadSlope%MV=M*RadSlope%MV  
! use SplineEval1Dx1D and DiaSlope to refill matrix RadSlope with new Zp at all points including origin
  ARadSlope%Zp=RadInterpolate(ARadSlope)  ! same as fillarray with IuseG=7 
  call CPU_TIME(time_end)
  t(8)=time_end-time_start
  write(*,*) 'Time to make new ARadSlope with origin: ',t(8)*1000
  

! GENERATE PRINT FILES
 if (i==1) then
  call WRITEARRAY(ARadSlope,BigPlot)
 endif 
 if (i==2) then
  call WRITEARRAY(ARadSlope,BigGrainyPlot)
 endif  
  
 end do

! put in module with printgraph and put loop in; might be able to read the max/min off each file
! or embed in the file with a comment/header
! can probably make the layout, number of files and file handle generic
! can use iostat to avoid file error on opening
  OPEN (UNIT = 17, FILE = 'plot2.gnu')
   WRITE(17,*) 'reset'
   WRITE(17,*) 'set size square'
   WRITE(17,*) 'set macros'
   WRITE(17,*) 'NOXTICS = "set format x ''''; unset xlabel"' 
   WRITE(17,*) 'NOYTICS = "set format y ''''; unset ylabel"'     
   WRITE(17,*) 'set multiplot layout 1,2 rowsfirst'
   CALL PRINTGRAPH(POWMIN,POWMAX,BigPlot)
   CALL PRINTGRAPH(POWMIN2,POWMAX2,BigGrainyPlot)
   WRITE(17,*) 'unset multiplot'
  CLOSE (17)

! deallocate 
  Atlas=0     
  RadSlope=0
  DiaSlope=0
  ARadSlope=0
  ADiaSlope=0
      
  STOP
  END
