  subroutine Janus(flag,mainfile, elements, vertices, nV, nE)  

! DRIVER PROGRAM FOR SPLINE ROUTINES
  use set_precision, ONLY : wp
  use cornea_arrays
  use io_functions
  use special_fct
  use, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char
  use c_interfaces, ONLY : OpenGL_Show, ConvertPLYtoBIN
  use omp_lib
  IMPLICIT NONE     
  integer :: i, thread
  integer :: MM, N ,M1, N1, ITH
  integer :: TestData                  ! TestData: 0=EyeSys, 1=Atlas, 2=Penta, 3=test 
  integer :: NP                        ! PentaCam=141
  character(c_char), INTENT(IN), DIMENSION(4096) :: mainfile
  integer(c_int), INTENT(INOUT) :: flag ! 0 = called from Jupiter 1=called from juno  
  integer(c_int), INTENT(INOUT) :: nV 
  integer(c_int), INTENT(INOUT) :: nE               
  real(c_float), INTENT(INOUT) :: vertices(*)
  integer(c_int), INTENT(INOUT) :: elements(*) 
  character(len=16) :: AxialPowerDataKnots
  character(len=8) :: BigGrainyPlot
  character(len=7) :: BigPlot
  character(len=8) :: LinesOfCurv
  character(len=4096) :: new_path
  character(:), ALLOCATABLE :: inputfile1,inputfile2
  character(:), ALLOCATABLE :: infile
  character(:), ALLOCATABLE :: outfile 
  integer ::  IuseG, IuseF, j, nblines, file_idx, file_pfx
  integer,allocatable :: MV(:)
  real :: time_start, time_end
  real(wp) :: POWMIN,POWMAX,POWMIN2,POWMAX2
  logical :: donut
  real(wp) ::   X1,X2,Y,YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA,TANC,rBi,rBo

!write(*,*) 'file from Jupiter: ',mainfile  ! this will have a lot of extra random non ASCII stuff after the file name
!! need this because GCC11 isn't F2018 compliant with deferred length character with Bind C
!! ie. can't do CHARACTER(*,c_char), INTENT(IN) :: mainfile with BIND(C) with GCC11
!! Juno's mainfile declaration   character(len=12), dimension(:), allocatable :: args with args(1) works too
!   Converting C char array to Fortran character.
    new_path = " "
    loop_string: do i=1, 4096
        if ( mainfile (i) == c_null_char ) then
            exit loop_string
        else
            new_path (i:i) = mainfile (i)
        end if
    end do loop_string
write(*,*) 'file from Jupiter/Juno: ',trim(new_path)
nblines=len(trim(new_path)) 
allocate(character(nblines) :: inputfile1)
allocate(character(nblines) :: inputfile2)
inputfile1=trim(new_path)
! From either RA?.DAT or XX?.DAT, set inputfile1 to the XX version, inputfile1 to the RA version.
! From either CUR or ELE set inputfile1 to ELE, inputfile2 to CUR
! For CSV set inputfile1 to Atlas file
file_idx=index(inputfile1, ".DAT")
   if( file_idx == 0)then
      print *, 'Not an EyeSys file'
      file_idx=index(inputfile1, ".CSV")
      if( file_idx == 0) then
       print *, 'Not an Atlas file'
       file_idx=index(inputfile1, ".CUR")
       if( file_idx == 0) then
        file_idx=index(inputfile1, ".ELE")
        if( file_idx == 0) then
         print *, 'Not a PentaCam file' 
         print *, 'Unknown file type: make some test data'
         TestData=3; MM=180; N=22 ; NP=141  ! make some test data 
!         TestData=3; MM=360; N=16 ; NP=141  ! make some test data rcnvrt not working 360        
        else
        inputfile2=replacestr(string=inputfile1,search="ELE",substitute="CUR")        
        TestData=2; MM=180; N=22; NP=141 ! PentaCam
       endif
      else
       inputfile2=inputfile1
       inputfile1=replacestr(string=inputfile2,search="CUR",substitute="ELE")
       TestData=2; MM=180; N=22; NP=141 ! PentaCam
      endif
      else
       TestData=1; MM=180; N=22   ! Atlas 
       write(*,*) "Atlas file: ",inputfile1
      endif
   else
      print *, 'suffix is found at index: ',file_idx,"length: ",len(inputfile1)
      print *, 'prefix:',inputfile1(file_idx-2:file_idx-1)
       file_pfx=index(inputfile1(file_idx-2:file_idx-1),"XX")
      if (file_pfx /= 0) then
       inputfile2=replacestr(string=inputfile1,search="XX",substitute="RA")
      else
       file_pfx=index(inputfile1(file_idx-2:file_idx-1),"RA")
       if (file_pfx /= 0) then
        inputfile2=inputfile1
        inputfile1=replacestr(string=inputfile2,search="RA",substitute="XX")
       else
        write(*,*) 'Error parsing EyeSys file name'
        stop
       endif
      endif
    TestData=0 ; MM=360; N=16   ! EyeSys
    write(*,*) "EyeSys files: ",inputfile1," ",inputfile2
   endif

  AxialPowerDataKnots='RCNVRTA.ORIG.CAR'
  BigGrainyPlot='BIGG.CAR'
  BigPlot='BIG.CAR'
  LinesOfCurv='LIOC.CAR'

  call CPU_TIME(time_start)             
  call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  call CPU_TIME(time_end)
  write(*,*) 'Time to allocate memory: ',(time_end-time_start)*1000 
 
  if (TestData .eq. 0) then  
! READ THE EYESYS DATA
! XX????? ARE THE AXIAL DIST. RX???? ARE THE MIRE RADII  
   call CPU_TIME(time_start)
   call init_mat_EyeSys(MM,N,EyeSys) ! allocate the EyeSys matrices
   call RCNVRTE(inputfile2,inputfile1) 
   call CPU_TIME(time_end)
   write(*,*) 'Time to read EyeSys files: ',(time_end-time_start)*1000
!  Generate the slope matrix using ZFCT 
   RadSlope=EyeSys
   EyeSys=0
!  CALL FILLARRAY(0,LinesOfCurv,POWMIN,POWMAX)
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope
!  make round rings and convert 360x16 to 180x22 
   M1=180
   N1=22
   call init_mat_JMatrix(M1,N1,JMatrix)
   ! donut
   rBo=7.0
   rBi=0.05*rBo 
   ! min and max bounds                             
   JMatrix%SAGC0(2)=1E30   ;  JMatrix%SAGC0(3)=-1E30
   JMatrix%Z0(2)=1E30      ;  JMatrix%Z0(3)=-1E30
   JMatrix%INSTC0(2)=1E30  ;  JMatrix%INSTC0(3)=-1E30
   JMatrix%INSTC20(2)=1E30 ;  JMatrix%INSTC20(3)=-1E30
   JMatrix%MEANC0(2)=1E30  ;  JMatrix%MEANC0(3)=-1E30
   JMatrix%MONGEA0(2)=1E30 ;  JMatrix%MONGEA0(3)=-1E30
   JMatrix%R0=0 ; JMatrix%THT0=0
   do i=1,M1
    ITH=2*(i-1)                             ! every 2 degrees
    JMatrix%THT(i)=PI*ITH/180.0_wp
    JMatrix%MV(i)=MIN(RadSlope%MV(2*i),RadSlope%MV(2*i-1))  ! close to real boundary
    do j=1,N1                             ! does not include center point
     if (i > (M1/2) ) then
      JMatrix%R(j,i)=100*((1-j)*(rBo-rBi)/(N1-1)-rBi)
     else
      JMatrix%R(j,i)=100*((j-1)*(rBo-rBi)/(N1-1)+rBi)
     endif
     call SplineEval1Dx1D(1,JMatrix%R(j,i),JMatrix%THT(i),JMatrix%Z(j,i),YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA)  
     call AXIALP(JMatrix%R(j,i),YPR,YP2R2,JMatrix%SAGC(j,i))
     call INSTANTP(JMatrix%R(j,i),YPR,YPTHETA,YP2R2,JMatrix%INSTC(j,i),JMatrix%INSTC2(j,i))
     call MEANP(JMatrix%THT(i),JMatrix%R(j,i),YPR,YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MEANC(j,i))
     call MONGEA(JMatrix%THT(i),JMatrix%R(j,i),YPR,YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MONGEA(j,i))
!    find min and max
     if (JMatrix%Z(j,i) <= JMatrix%Z0(2)) JMatrix%Z0(2)=JMatrix%Z(j,i)
     if (JMatrix%Z(j,i) >= JMatrix%Z0(3)) JMatrix%Z0(3)=JMatrix%Z(j,i)
     if (JMatrix%SAGC(j,i) <= JMatrix%SAGC0(2)) JMatrix%SAGC0(2)=JMatrix%SAGC(j,i)
     if (JMatrix%SAGC(j,i) >= JMatrix%SAGC0(3)) JMatrix%SAGC0(3)=JMatrix%SAGC(j,i)
     if (JMatrix%INSTC(j,i) <= JMatrix%INSTC0(2)) JMatrix%INSTC0(2)=JMatrix%INSTC(j,i)
     if (JMatrix%INSTC(j,i) >= JMatrix%INSTC0(3)) JMatrix%INSTC0(3)=JMatrix%INSTC(j,i)
     if (JMatrix%INSTC2(j,i) <= JMatrix%INSTC20(2)) JMatrix%INSTC20(2)=JMatrix%INSTC2(j,i)
     if (JMatrix%INSTC2(j,i) >= JMatrix%INSTC20(3)) JMatrix%INSTC20(3)=JMatrix%INSTC2(j,i)
     if (JMatrix%MEANC(j,i) <= JMatrix%MEANC0(2)) JMatrix%MEANC0(2)=JMatrix%MEANC(j,i)
     if (JMatrix%MEANC(j,i) >= JMatrix%MEANC0(3)) JMatrix%MEANC0(3)=JMatrix%MEANC(j,i)
     if (JMatrix%MONGEA(j,i) <= JMatrix%MONGEA0(2)) JMatrix%MONGEA0(2)=JMatrix%MONGEA(j,i)
     if (JMatrix%MONGEA(j,i) >= JMatrix%MONGEA0(3)) JMatrix%MONGEA0(3)=JMatrix%MONGEA(j,i)
    end do
   end do
   RadSlope=0
   DiaSlope=0
   deallocate(RadSplineCenter)
   call init_mat(M1,N1,RadSlope,DiaSlope,RadSplineCenter)
   call RadSlope_eq_JMatrix(RadSlope,JMatrix)
   MM=180
   N=22
   endif

   allocate (MV(MM))
           
! OR READ THE ATLAS DATA
! R OR DIST ARE THE MIRE RADII, USING DIST, READS ELEVATION ALSO  
  if (TestData .eq. 1) then 
   call CPU_TIME(time_start)
   call init_mat_Atlas(MM,N,Atlas)
   call init_mat_JMatrix(MM,N,JMatrix)
   call RCNVRTA(inputfile1)
   call CPU_TIME(time_end)
   write(*,*) 'Time to read Atlas CSV file: ',(time_end-time_start)*1000
   call RadSlope_eq_Atlas(JMatrix,RadSlope,Atlas)
  endif

  if (TestData .eq. 2) then
! READ THE PENTACAM DATA (which overwrites Atlas)
! ELE are elevations CUR are "sagittal" curvatures in a 141x141 -7 to 7 mm square -1 is no data 
   call init_mat_Penta(NP,Penta,Skyline)   !allocate the PentaCam matices
   call init_mat_JMatrix(MM,N,JMatrix)
   call init_mat_Atlas(MM,N,Atlas)        !need to excise Atlas
   call RCNVRTP(inputfile1,inputfile2) 
!  arrange the data
   call CPU_TIME(time_start)
   Skyline=Penta
!  convert to polar with splining
   call RadSlope_eq_Skyline(JMatrix, RadSlope, Skyline, Penta)  !needs Penta for border check
   MV(:)=RadSlope%MV(:) ! store a copy
   call CPU_TIME(time_end)
   write(*,*) 'Time to convert Penta: ',(time_end-time_start)*1000
   Penta = 0              ! deallocate
   Skyline = 0
  endif

  if (TestData .eq. 3) then 
! OR GENERATE TEST DATA (EYESYS,ATLAS OR PENTA STYLE)
   call init_mat_JMatrix(MM,N,JMatrix)
   call init_mat_EyeSys(MM,N,EyeSys) ! allocate the EyeSys matrices
   call init_mat_Atlas(MM,N,Atlas)
   call init_mat_Penta(NP,Penta,Skyline)   ! allocate the PentaCam matices   
   call RCNVRTT(MM,N,NP)
!   uncomment next two lines to test fake Penta data
!   Skyline=Penta
!   call RadSlope_eq_Skyline(RadSlope, Skyline, Penta)  !needs Penta & RadSlope for border check
   if (MM == 360) then
    call RadSlope_eq_EyeSys(RadSlope,EyeSys) 
    Atlas=RadSlope   ! total caca
   endif    
   call RadSlope_eq_Atlas(JMatrix,RadSlope,Atlas)
    Penta = 0
    EyeSys = 0
  endif
  
! Here IuseG changes the contents of RadSlope via FillArray
!  IuseG == -1 import slopes, return SAGC (axialp), no splining necessary
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
!  IuseG == 14 has to follow 7, import elevation, return ZNMEX (instantp)  Should be grainy or have other issues

! (IuseG=-1) then IuseG=0,1 or 2), IuseF=0-> intdifM
! (IuseG=0)  then IuseG=(3..7) , IuseF=0-> intdifR
! (IuseG=0)  then IuseG=(3..7),  IuseF=1-> intdifZ    !DON'T DO THIS 
   
  IuseG=0  ! IuseG=-1 or 0 here only, presplining; 0 just finds POWMIN/MAX can skip entirely here

  CALL FILLARRAY(IuseG,LinesOfCurv,POWMIN,POWMAX) 

  IuseG=4  ! if above IuseG=-1, then change to 0, 1 or 2  ! IuseG=0 then change to 3 through 8

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
    write(*,*) 'Time to run fillin: ',(time_end-time_start)*1000
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
    call RadSlope_eq_Atlas(JMatrix,RadSlope,Atlas) ! recalculate RadSlope based on filled-in Atlas, including MV
    call CPU_TIME(time_end)
    write(*,*) 'Time to run radslope: ',(time_end-time_start)*1000
   endif 
  endif
       
! GENERATE RADIAL SPLINES ACROSS CENTER
  call CPU_TIME(time_start)
  DiaSlope=RadSlope              ! move to diagonal format
  if (TestData.ne.2) then
   DiaSlope%Zpd2 = .n. DiaSlope   ! generate the splines diagonally (generate zp2)
  else
   DiaSlope%Zpd2 = DiaSplineCenter(DiaSlope)   ! generate the splines diagonally with center node added (slopes only)
  endif                                        ! should I do this and force the center to be at the origin
  call CPU_TIME(time_end)
  write(*,*) 'Time to run splines: ',(time_end-time_start)*1000

! Rewrite RadSlope with round rings and new values 

! roadmap: now generate round rings, not at previous knots
! generate new Rs using rOMIN, rOMAX, riMIN, riMAX but they have to be constant with theta
! get a global value for those four to generate R's no ORIGIN to avoid singularity
 if (TestData.ne.2) then
!  RadSlope%r=make_rings(DiaSlope,.FALSE.)
 endif 
!  RadSlope%r=make_bad_rings(DiaSlope,.FALSE.)
!  use fillarray to fill DiaSlope Zp with calculated value based on IuseG, optionally generate LIOC
!  using SplineEval1Dx1D to refill a new matrix RadSlope using f0, derivatives to get calculated powers
  
!  Generate LIOC with vector format
   call FILLARRAY(8,LinesOfCurv,POWMIN2,POWMAX2)    ! don't redo bounds consider optional !  plot 'LIOC.CAR' using 1:2:3:4 with vectors
!  WriteCenter shows where the spline of slopes is zero, it should be close to zero for a concave center with a unique maximum  
   call WriteCenter(RadSlope,'Center.dat')   ! biggest deviation with nSplineCenter zero slope forced at origin, 
                                             ! then with zero slope forced at average (r(low)+r(high))/2.0
                                             ! smallest deviation without nSplineCenter; view with set polar; plot 'Center.dat' with lines
  call execute_command_line ("gnuplot -p plotlioc.gnu &", exitstat=i)
!  write OFF and STL files
  MV(:)=RadSlope%MV(:) ! store a copy
!  RadSlope%MV(:)=N   !full diameters for elevation 
  call FILLARRAY(7,LinesOfCurv,POWMIN,POWMAX)
! These are elevation bounds     

!!!$OMP PARALLEL num_threads(2) private(thread)
!      separate iterative parts into subroutines so each thread can work in parallel
!!       thread = omp_get_thread_num()
!!!$OMP threadprivate(RadSlope)
!!       if (thread==0) then

!!!$OMP PARALLEL COPYIN(RadSlope)
  donut = .FALSE.
  
!  if (TestData.eq.2) then
   powmin=JMatrix%SAGC0(2)  
   powmax=JMatrix%SAGC0(3)    
!  endif  
  
  call WriteGeom(JMatrix,donut,powmin,powmax,'elevation.off','elevation.ply')
! from https://w3.impa.br/~diego/software/rply/ c program to convert ASCII PLY to binary PLY MIT licence, included source in tree
!  call execute_command_line ("./ConvertPLYtoBIN -l elevation.ply elevation.bin.ply",exitstat=i)
  infile='elevation.ply'
  outfile='elevation.bin.ply'
  call ConvertPLYtoBIN(infile,outfile)  ! call C (modified) routine directly
! only call if quad .eqv. .FALSE.
  call ConvertOFFtoSTL('elevation.off','elevation.stl','elevation.bin.stl')
!  can view with meshlab e.g.
!  write(*,*) 'Exit meshlab to continue'
!  call execute_command_line ("meshlab elevation.off", exitstat=i)
!  call execute_command_line ("meshlab elevation.stl", exitstat=i)
!  call execute_command_line ("meshlab elevation.bin.stl", exitstat=i)

! eigenvalues show shape of RadSlope without make_rings but with FillArray 7 elevations
!  atmp=pca(2,RadSlope) 
!  atmp=pca(3,RadSlope)
! writes values in openGL friendly format to matrices for passing to C/C++; flag to display with glfw using juno

  call Geom(flag,RadSlope, donut, powmin, powmax, elements, vertices, nV, nE)

!  the cube example
!   nV = 48

!    vertices(1:nV) = (/  -50.0,  50.0, -50.0, 1.0, 0.0, 0.0, 50.0,  50.0, -50.0, 0.0, 1.0, 0.0,  &
!        50.0, -50.0, -50.0, 0.0, 0.0, 1.0, -50.0, -50.0, -50.0, 1.0, 1.0, 1.0,   &
!        -50.0,  50.0, 50.0, 1.0, 1.0, 0.0, 50.0,  50.0, 50.0, 0.0, 1.0, 1.0,  &
!        50.0, -50.0, 50.0, 1.0, 0.0, 1.0, -50.0, -50.0, 50.0, 0.0, 0.0, 0.0 /)

!   nE = 36
!    elements(1:nE) = (/  &
!       0, 1, 2,&
!       2, 3, 0,&
!       4, 5, 6,&
!       6, 7, 4,&
!       0, 4, 5,&
!       5, 1, 0,&
!       3, 7, 6,&
!       6, 2, 3,&
!       0, 4, 7,&
!       7, 3, 0,&
!       1, 5, 6,&
!      & 6, 2, 1      /) 

!return 


!!!$OMP END PARALLEL  
!!else  !OMP thread else
 
! make more than one plot
stop
  
  do i=1,2
  if (i==1) then
   write(*,*) 'Plot: ',i 
   call CPU_TIME(time_start)    
   if (MM == 360) then      ! implies TestData == 0
!   Generate the slope matrix using ZFCT
!   Generate "Atlas" data with AXIALP  
   call RadSlope_eq_EyeSys(RadSlope,EyeSys) 
   else ! MM==180
!   Generate the slope matrix using Atlas data     
   call RadSlope_eq_Atlas(JMatrix,RadSlope,Atlas)
   endif  
   DiaSlope=RadSlope            
   DiaSlope%Zpd2 = .n. DiaSlope
!   DiaSlope%Zpd2 = DiaSplineCenter(DiaSlope)   ! generate the splines diagonally with center node added (slopes only)
!   RadSlope%r=make_rings(DiaSlope,.FALSE.)              
   call FILLARRAY(4,LinesOfCurv,POWMIN,POWMAX)
   call CPU_TIME(time_end)
   write(*,*) 'Time to rewrite RadSlope without origin: ',(time_end-time_start)*1000   
  endif


  if (i==2) then
   write(*,*) 'Next Plot: ',i
   call CPU_TIME(time_start)  
   if (MM == 360) then       ! implies TestData == 0
!   Generate the slope matrix using ZFCT
!   Generate "Atlas" data with AXIALP  
   call RadSlope_eq_EyeSys(RadSlope,EyeSys) 
   else ! MM==180
!   Generate the slope matrix using Atlas data     
   call RadSlope_eq_Atlas(JMatrix,RadSlope,Atlas)
   endif
   DiaSlope=RadSlope            
   DiaSlope%Zpd2 = .n. DiaSlope
!   DiaSlope%Zpd2 = DiaSplineCenter(DiaSlope)   ! generate the splines diagonally with center node added (slopes only)
!   RadSlope%r=make_rings(DiaSlope,.FALSE.)            
   call FILLARRAY(7,LinesOfCurv,POWMIN2,POWMAX2)  ! generate elevation
   DiaSlope=RadSlope             
   DiaSlope%Zpd2 = .n. DiaSlope 
!   RadSlope%r=make_rings(DiaSlope,.FALSE.)
 !  call FILLARRAY(14,LinesOfCurv,POWMIN2,POWMAX2) ! get derivatives from elevation 14 is the same as 4 but might be grainy
   call CPU_TIME(time_end)
   write(*,*) 'Time to re-generate a new RadSlope/DiaSlope from Atlas: ',(time_end-time_start)*1000   
  endif   

! REGENERATE SPLINES ACROSS CENTER (repeating because new values in RadSlope and round rings)
  call CPU_TIME(time_start)  
  DiaSlope=RadSlope            ! move to diagonal; wipes out the original DiaSlope
  DiaSlope%Zpd2 = .n. DiaSlope ! generate the splines diagonally (generate zp2)     
  call CPU_TIME(time_end)
  write(*,*) 'Time to re-run splines: ',(time_end-time_start)*1000

  call CPU_TIME(time_start)

! generate new Rs using rOMIN, rOMAX, but they have to be constant with theta
! get a global value for those two to generate R's INCLUDING ORIGIN and using ADiaSlope  
 ! RadSlope%r=make_rings(ADiaSlope,.TRUE.)
  
 
! use SplineEval1Dx1D and DiaSlope to refill matrix RadSlope with new Zp at all points including origin
  RadSlope%Zp=RadInterpolate(RadSlope)  ! takes DiaSlope/RadSlope data -> interpolates to new ARadslope, no integration
  call CPU_TIME(time_end)
  write(*,*) 'Time to make new ARadSlope with origin: ',(time_end-time_start)*1000
  
! GENERATE PRINT FILES
 if (i==1) then
  call WRITEARRAY(RadSlope,BigPlot)
 endif 
 if (i==2) then
  call WRITEARRAY(RadSlope,BigGrainyPlot)
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
  CLOSE (17)

  call execute_command_line ("gnuplot -p plot2.gnu &", exitstat=i)
!  call execute_command_line ("./view", exitstat=i)

!!endif  ! end OMP
!!!$OMP END PARALLEL

! deallocate
  deallocate(RadSplineCenter)
  deallocate (MV)
  if (TestData == 0) then
   EyeSys=0 
  endif
  Atlas=0     
  RadSlope=0
  DiaSlope=0

  return        

  END subroutine janus
