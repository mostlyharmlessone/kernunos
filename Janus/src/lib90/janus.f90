  subroutine Janus(flag,mainfile, elements, vertices, nV, nE)  

! DRIVER PROGRAM FOR SPLINE ROUTINES
  use set_precision, ONLY : wp
  use cornea_arrays
  use special_fct
  use io_functions
  use, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char
  use c_interfaces, ONLY : OpenGL_Show
  use omp_lib
  IMPLICIT NONE     
  integer :: i, thread, unitno1, ierr
  integer :: MM, N ,M1, N1, ITH
  integer :: TestData                  ! TestData: 0=EyeSys, 1=Atlas, 2=Penta, 3=test 
  integer :: NP                        ! PentaCam=141
  character(c_char), INTENT(IN), DIMENSION(4096) :: mainfile
  integer(c_int), INTENT(INOUT) :: flag ! 0 = called from Jupiter 1=called from juno  
  integer(c_int), INTENT(INOUT) :: nV 
  integer(c_int), INTENT(INOUT) :: nE               
  real(c_float), INTENT(INOUT) :: vertices(*)
  integer(c_int), INTENT(INOUT) :: elements(*) 
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
  real(wp) :: POWMIN,POWMAX,POWMIN2,POWMAX2,POWCTR,POW
  logical :: donut
  real(wp) :: Y,YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA,rBi,rBo

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

  BigGrainyPlot='BIGG.CAR'
  BigPlot='BIG.CAR'
  LinesOfCurv='LIOC.CAR'

  call CPU_TIME(time_start)             
  call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  call CPU_TIME(time_end)
  write(*,*) 'Time to allocate memory: ',(time_end-time_start)*1000 

   M1=180
   N1=22
   if (allocated(JMatrix1%R)) then
    write(*,*) 'JMatrix1 allocated'
   else
    if (allocated(JMatrix%R)) then
     write(*,*) 'allocating JMatrix1'
     call init_mat_JMatrix(M1,N1,JMatrix1)
    else
     write(*,*) 'allocating JMatrix'
     call init_mat_JMatrix(M1,N1,JMatrix)
    endif
   endif
 
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
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope
  endif

! READ THE ATLAS DATA
  if (TestData .eq. 1) then 
   call CPU_TIME(time_start)
   call init_mat_Atlas(MM,N,Atlas)
   call RCNVRTA(inputfile1)
   call CPU_TIME(time_end)
   write(*,*) 'Time to read Atlas CSV file: ',(time_end-time_start)*1000
   call RadSlope_eq_Atlas(JMatrix,RadSlope,Atlas)
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope
  endif

  if ((TestData .eq. 1) .or. (TestData .eq. 0)) then
!  make round rings and if needed convert 360x16 to 180x22 
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
    if (TestData .eq. 0) then 
     JMatrix%MV(i)=MIN(RadSlope%MV(2*i),RadSlope%MV(2*i-1))  ! close to real boundary
    else  ! TestData == 1 and MM==180
     JMatrix%MV(i)=RadSlope%MV(i)
    endif
    do j=1,N1                             ! does not include center point
     if (i > (M1/2) ) then
      JMatrix%R(j,i)=100*((1-j)*(rBo-rBi)/(N1-1)-rBi)
     else
      JMatrix%R(j,i)=100*((j-1)*(rBo-rBi)/(N1-1)+rBi)
     endif     

     if ( Testdata .eq. 1 ) then  ! check on AD,Z and POW consistency before overwriting JMatrix/Atlas values
      call SplineEval1Dx1D(1,Atlas%AD(i,j),JMatrix%THT(i),Y,YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA)
      call AXIALP(Atlas%AD(i,j),YPR,YP2R2,POW)
!      JMatrix%SAGC(j,i)-POW
!      JMatrix%Z(j,i)-Y
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
   if (Testdata .eq. 1) then
    Atlas=0
   endif
   RadSlope=0
   DiaSlope=0
   deallocate(RadSplineCenter)
   call init_mat(M1,N1,RadSlope,DiaSlope,RadSplineCenter)
   call RadSlope_eq_JMatrix(RadSlope,JMatrix)
   MM=180
   N=22
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope
!  These are the spline centers of the elevations
   call MakeRadSplineCenter
!  WriteCenter shows where the spline of slopes is zero, it should be close to zero for a concave center with a unique maximum
   call WriteCenter(RadSlope,'Center.dat')! biggest deviation with nSplineCenter zero slope forced at origin, 
                                             ! then with zero slope forced at average (r(low)+r(high))/2.0
                                             ! smallest deviation without nSplineCenter; view with set polar; plot 'Center.dat' with lines
   call execute_command_line ("gnuplot -p plotcenter.gnu &", exitstat=i)
   call SplineEval1Dx1D(1,JMatrix%R0,JMatrix%THT0,JMatrix%Z0(1))  !center value of elevation; needs integration from slopes
   !could also do all the deviations' elevations or powers eg
!   do j=1,MM
!    call SplineEval1Dx1D(1,RadSplineCenter(j),JMatrix%THT(j),JMatrix%RC(j))
!   end do
!  Reload RadSlope & respline
   do i=1,MM
    do j=1,RadSlope%MV(i)
     RadSlope%Zp(j,i)=JMatrix%SAGC(j,i)
    end do
   end do
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope
   call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT0,JMatrix%SAGC0(1))  ! center value
   call RadSlope_eq_JMatrix(RadSlope,JMatrix)                        ! restore RadSlope
   endif

  if (TestData .eq. 2) then
! READ THE PENTACAM DATA (which overwrites Atlas)
! ELE are elevations CUR are "sagittal" curvatures in a 141x141 -7 to 7 mm square -1 is no data 
   call init_mat_Penta(NP,Penta,Skyline)   !allocate the PentaCam matices
   call init_mat_Atlas(MM,N,Atlas)        !need to excise Atlas
   call RCNVRTP(inputfile1,inputfile2) 
!  arrange the data
   call CPU_TIME(time_start)
   Skyline=Penta
!  convert to polar with splining; makes round rings as above with 180x22 - also already has center values Z0(1) and SAGC0(1)
   call RadSlope_eq_Skyline(JMatrix, RadSlope, Skyline, Penta)  !needs Penta for border check
   call CPU_TIME(time_end)
   write(*,*) 'Time to convert Penta: ',(time_end-time_start)*1000
   Penta = 0              ! deallocate
   Skyline = 0
   Atlas=0
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope
   call MakeRadSplineCenter
   call WriteCenter(RadSlope,'Center.dat')
   call execute_command_line ("gnuplot -p plotcenter.gnu &", exitstat=i)
   JMatrix%INSTC0(2)=1E30  ;  JMatrix%INSTC0(3)=-1E30
   JMatrix%INSTC20(2)=1E30 ;  JMatrix%INSTC20(3)=-1E30
   JMatrix%MEANC0(2)=1E30  ;  JMatrix%MEANC0(3)=-1E30
   JMatrix%MONGEA0(2)=1E30 ;  JMatrix%MONGEA0(3)=-1E30
   do i=1,MM
    do j=1,RadSlope%MV(i)                           
     call SplineEval1Dx1D(1,JMatrix%R(j,i),JMatrix%THT(i),JMatrix%Z(j,i),YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA)  
     call INSTANTP(JMatrix%R(j,i),YPR,YPTHETA,YP2R2,JMatrix%INSTC(j,i),JMatrix%INSTC2(j,i))
     call MEANP(JMatrix%THT(i),JMatrix%R(j,i),YPR,YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MEANC(j,i))
     call MONGEA(JMatrix%THT(i),JMatrix%R(j,i),YPR,YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MONGEA(j,i))
!    find min and max
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

!  Calculate center values for everything but Z0,SAGC0 (already done)
!  Reload RadSlope & respline
   do i=1,MM
    do j=1,RadSlope%MV(i)
     RadSlope%Zp(j,i)=JMatrix%INSTC(j,i)
    end do
   end do
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope   ! generate the splines diagonally (generate zp2)
   call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT0,JMatrix%INSTC0(1))  ! center value
   call RadSlope_eq_JMatrix(RadSlope,JMatrix)                        ! restore RadSlope
!  Reload RadSlope & respline
   do i=1,MM
    do j=1,RadSlope%MV(i)
     RadSlope%Zp(j,i)=JMatrix%INSTC2(j,i)
    end do
   end do
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope   ! generate the splines diagonally (generate zp2)
   call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT0,JMatrix%INSTC20(1))  ! center value
   call RadSlope_eq_JMatrix(RadSlope,JMatrix)                        ! restore RadSlope
!  Reload RadSlope & respline
   do i=1,MM
    do j=1,RadSlope%MV(i)
     RadSlope%Zp(j,i)=JMatrix%MEANC(j,i)
    end do
   end do
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope   ! generate the splines diagonally (generate zp2)
   call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT0,JMatrix%MEANC0(1))  ! center value
   call RadSlope_eq_JMatrix(RadSlope,JMatrix)                        ! restore RadSlope
!  Reload RadSlope & respline
   do i=1,MM
    do j=1,RadSlope%MV(i)
     RadSlope%Zp(j,i)=JMatrix%MONGEA(j,i)
    end do
   end do
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope   ! generate the splines diagonally (generate zp2)
   call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT0,JMatrix%MONGEA0(1))  ! center value
   call RadSlope_eq_JMatrix(RadSlope,JMatrix)                        ! restore RadSlope
  
  allocate (MV(MM))
  MV(:)=RadSlope%MV(:) ! store a copy

!  use fillarray to fill DiaSlope Zp with calculated value based on IuseG, optionally generate LIOC
!  using SplineEval1Dx1D to refill a new matrix RadSlope using f0, derivatives to get calculated powers
  
!  Generate LIOC with vector format
   call FILLARRAY(8,LinesOfCurv,POWMIN2,POWMAX2)    ! don't redo bounds consider optional !  plot 'LIOC.CAR' using 1:2:3:4 with vectors
!  call execute_command_line ("gnuplot -p plotlioc.gnu &", exitstat=i)

!  write OFF and STL files

   donut = .FALSE.

   powctr=JMatrix%SAGC0(1)  
   powmin=JMatrix%SAGC0(2)  
   powmax=JMatrix%SAGC0(3)
   write(*,*) 'powctr,POWMIN,POWMAX',powctr,POWMIN,POWMAX
   powmin=10.0 
   
! Writes OFF and ASCII PLY files
  call WriteGeom(JMatrix,donut,powmin,powmax,'elevation.off','elevation.ply')
! from https://w3.impa.br/~diego/software/rply/ c program to convert ASCII PLY to binary PLY MIT licence, included source in tree
  call execute_command_line ("./ConvertPLYtoBIN -l elevation.ply elevation.bin.ply",exitstat=i)
! only call if quad .eqv. .FALSE.
! Writes STL from OFF
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

  call Geom(flag, JMatrix, donut, powmin, powmax, elements, vertices, nV, nE)

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


! make more than one plot
  deallocate(MV)
  deallocate(RadSplineCenter)

  do i=1,2
  if (i==1) then
   write(*,*) 'Plot: ',i 
   call CPU_TIME(time_start)   
   call RadSlope_eq_JMatrix(RadSlope,JMatrix)  
   DiaSlope=RadSlope            
   DiaSlope%Zpd2 = .n. DiaSlope
!   DiaSlope%Zpd2 = DiaSplineCenter(DiaSlope)   ! generate the splines diagonally with center node added (slopes only)
!   RadSlope%r=make_rings(DiaSlope,.FALSE.)              
   call FILLARRAY(4,LinesOfCurv,POWMIN,POWMAX)
   write(*,*) 'POWMIN,POWMAX',POWMIN,POWMAX
   call CPU_TIME(time_end)
   write(*,*) 'Time to make plot',i,(time_end-time_start)*1000   
  endif

  if (i==2) then
   write(*,*) 'Next Plot: ',i
   call CPU_TIME(time_start)  
   call RadSlope_eq_JMatrix(RadSlope,JMatrix)
   DiaSlope=RadSlope            
   DiaSlope%Zpd2 = .n. DiaSlope
!   DiaSlope%Zpd2 = DiaSplineCenter(DiaSlope)   ! generate the splines diagonally with center node added (slopes only)
   call FILLARRAY(7,LinesOfCurv,POWMIN2,POWMAX2)  ! generate elevation
   DiaSlope=RadSlope             
   DiaSlope%Zpd2 = .n. DiaSlope 
   call FILLARRAY(14,LinesOfCurv,POWMIN2,POWMAX2) ! get derivatives from elevation 14 is the same as 4 but might be grainy
   write(*,*) 'POWMIN,POWMAX',POWMIN2,POWMAX2
   call CPU_TIME(time_end)
   write(*,*) 'Time to make plot',i,(time_end-time_start)*1000   
  endif   

  
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

   unitno1 = get_new_fileunit()
   open(unitno1, file = 'plot2.gnu', action="write", iostat=ierr)
   WRITE(unitno1,*) 'reset'
   WRITE(unitno1,*) 'set size square'
   WRITE(unitno1,*) 'set macros'
   WRITE(unitno1,*) 'NOXTICS = "set format x ''''; unset xlabel"' 
   WRITE(unitno1,*) 'NOYTICS = "set format y ''''; unset ylabel"'     
   WRITE(unitno1,*) 'set multiplot layout 1,2 rowsfirst'
   CALL PRINTGRAPH(unitno1,POWMIN,POWMAX,BigPlot)
   CALL PRINTGRAPH(unitno1,POWMIN2,POWMAX2,BigGrainyPlot)
   CLOSE (unitno1)

  call execute_command_line ("gnuplot -p plot2.gnu &", exitstat=i)

  RadSlope=0
  DiaSlope=0
  return        

  END subroutine janus
