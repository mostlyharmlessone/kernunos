  subroutine Janus(flag, file_from_C, elements, vertices, nV, nE) bind(C,name='janus_')
! DRIVER PROGRAM FOR SPLINE ROUTINES
  use set_precision, ONLY : wp
  use lapackinterface
  use cornea_arrays
  use special_fct
  use io_functions
  use, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char
  !use c_interfaces, ONLY : ConvertPLYtoBIN
  use omp_lib
  IMPLICIT NONE     
  integer :: i, j, k, ii, kk, m, i1, j1, thread, ierr, info, nrhs
  integer :: MM, N ,M1, N1, NN, ITH
  integer :: TestData                  ! TestData: -1=test, 0=EyeSys, 1=Atlas, (2-5)=Penta 
  integer :: NP                        ! PentaCam=141
  integer :: unitno1                  
  character(c_char), INTENT(IN), DIMENSION(4096) :: file_from_C
  integer(c_int), INTENT(INOUT) :: flag
  integer(c_int), INTENT(INOUT) :: nV 
  integer(c_int), INTENT(INOUT) :: nE               
  real(c_float), INTENT(INOUT) :: vertices(*)
  integer(c_int), INTENT(INOUT) :: elements(*)
  character(len=7) :: BigPlot
  character(len=8) :: LinesOfCurv
  character(len=4096) :: new_path
  character(:), ALLOCATABLE :: inputfile1,inputfile2
  character(:), ALLOCATABLE :: logfile
  integer ::  nblines, file_idx, file_pfx
  integer,allocatable :: MV(:)
  real :: time_start, time_end
  real(wp) :: POWMIN,POWMAX,POWMIN2,POWMAX2,POWCTR,POW
  logical :: donut, exists
  real(wp) :: Y,YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA,rBi,rBo
  integer :: LWORK, k_max, kk_max
  real(wp), allocatable :: WORK(:), ZernC(:,:), B_Matrix(:,:), rlocal(:), thtlocal(:)
  real(wp), allocatable :: XTX(:,:),EE(:)
  integer, allocatable :: IPIV(:)
  real(wp) :: ctr_circle_x, ctr_circle_y, R_Talus, Theta_Talus, X_global, Y_global

! flag == 99 Deallocate
if (flag == 99) then
    if (allocated(JMatrix%R)) then
     JMatrix=0
    endif
    if (allocated(JMatrix1%R)) then
     JMatrix1=0
    endif
    if (allocated(DiaSlope%rd)) then
     DiaSlope=0
    endif
    if (allocated(RadSlope%r)) then
     RadSlope=0
    endif
    if (allocated(inputfile1)) then
     deallocate(inputfile1)
     deallocate(inputfile2)
     deallocate(logfile)
    endif
    if (allocated(MV)) then
     deallocate(MV)
    endif
    if (allocated(RadSplineCenter)) then
     deallocate(RadSplineCenter)
    endif
    return
endif

!write(*,*) 'file from kernunos: ',file_from_C  ! this will have a lot of extra random non ASCII stuff after the file name
!! need this because GCC11 isn't F2018 compliant with deferred length character with Bind C
!! ie. can't do CHARACTER(*,c_char), INTENT(IN) :: file_from_C_1 with BIND(C) with GCC11
!! declaring character(len=12), dimension(:), allocatable :: args with args(1) works too, but limited in length
!   Converting C char array to Fortran character.
    new_path = " "
    do i=1, 4096
        if ( file_from_C (i) == c_null_char ) then
            exit
        else
            new_path (i:i) = file_from_C (i)
        end if
    end do

write(*,*) 'flag to Fortran:',flag
write(*,*) 'file from kernunos: ',trim(new_path)
nblines=len(trim(new_path)) 
allocate(character(nblines) :: inputfile1)
allocate(character(nblines) :: logfile)
inputfile1=trim(new_path)
allocate(character(nblines) :: inputfile2)

! Writes ASCII PLY file
if (flag == 3) then
if (allocated(JMatrix%R)) then
file_idx=index(inputfile1, ".ply")
 if( file_idx == 0) then
   write(*,*) 'not a ply file'
  else

  donut = .FALSE.

  powctr=JMatrix%SAGC0(1)
  powmin=JMatrix%SAGC0(2)
  powmax=JMatrix%SAGC0(3)
  write(*,*) 'powctr,POWMIN,POWMAX',powctr,POWMIN,POWMAX
  ! powmin=35.5
  ! powmax=55.5

  call WriteGeomPLY(flag,JMatrix,donut,powmin,powmax,inputfile1)
  write(*,*) 'Wrote ply file...',inputfile1
  return
 endif
else
  write(*,*) 'Have to allocate data prior to writing a ply file'
 return ! if flag==3 and not allocated do nothing
endif
endif

! Writes OFF file
if (flag == 2) then
if (allocated(JMatrix%R)) then
file_idx=index(inputfile1, ".off")
 if( file_idx == 0) then
   write(*,*) 'not an off file'
  else

  donut = .FALSE.

  powctr=JMatrix%SAGC0(1)
  powmin=JMatrix%SAGC0(2)
  powmax=JMatrix%SAGC0(3)
  write(*,*) 'powctr,POWMIN,POWMAX',powctr,POWMIN,POWMAX

  call WriteGeomOFF(flag,JMatrix,donut,powmin,powmax,inputfile1)
  write(*,*) 'Wrote off file...',inputfile1
  return
 endif
else
  write(*,*) 'Have to allocate data prior to writing an off file'
 return ! if flag==2 and not allocated do nothing
endif
endif

! flag == 0 Import file and compute JMatrix, RAdSlope, etc.
if (flag == 0) then
! From either RA?.DAT or XX?.DAT, set inputfile1 to the XX version, inputfile1 to the RA version.
! For either .CUR or .ELE or .CUR.CSV or .ELE.CSV set inputfile1 to Penta file of appropriate type with TestData
! For CSV but not .ELE.CSV or .CUR.CSV set inputfile1 to Atlas file
file_idx=index(inputfile1, ".DAT")
   if( file_idx == 0)then
      write(*,*) 'Not an EyeSys file'
      file_idx=index(inputfile1, ".CSV")
      if( file_idx == 0) then
       write(*,*) 'Not an Atlas file'
       file_idx=index(inputfile1, ".CUR")
       if( file_idx == 0) then
        file_idx=index(inputfile1, ".ELE")
        if( file_idx == 0) then
         write(*,*) 'Not a PentaCam file' 
         write(*,*) 'Unknown file type: make some test data, flag = ',flag
         TestData=-1; MM=180; N=22 ; NP=141  ! make some test data not working
!         TestData=-1; MM=360; N=16 ; NP=141  ! make some test data rcnvrt not working 360        
        else
!        inputfile2=replacestr(string=inputfile1,search="ELE",substitute="CUR")        
        TestData=2; MM=180; N=22; NP=141 ! PentaCam ELE
       endif
      else
!       inputfile2=inputfile1
!       inputfile1=replacestr(string=inputfile2,search="CUR",substitute="ELE")
       TestData=3; MM=180; N=22; NP=141 ! PentaCam CUR
      endif
      else
       file_idx=index(inputfile1, ".CUR")
       if( file_idx == 0) then
        file_idx=index(inputfile1, ".ELE") 
        if( file_idx == 0) then
         TestData=1; MM=180; N=22   ! Atlas 
         write(*,*) "Atlas file: ",inputfile1
        else
        write(*,*) 'Not an Atlas file'
 !       inputfile2=replacestr(string=inputfile1,search="ELE",substitute="CUR")        
        TestData=4; MM=180; N=22; NP=141 ! PentaCam ELE.CSV
        endif
        else
        write(*,*) 'Not an Atlas file'
!       inputfile2=inputfile1
!       inputfile1=replacestr(string=inputfile2,search="CUR",substitute="ELE")
       TestData=5; MM=180; N=22; NP=141 ! PentaCam CUR.CSV
       endif
      endif
   else
      write(*,*) 'suffix is found at index: ',file_idx,"length: ",len(inputfile1)
      write(*,*) 'prefix:',inputfile1(file_idx-2:file_idx-1)
       file_pfx=index(inputfile1(file_idx-2:file_idx-1),"XX")
      if (file_pfx /= 0) then
       inputfile2=replacestr(string=inputfile1,search="XX",substitute="RA")
       inquire(file=trim(inputfile2), exist=exists)
       if(.NOT.exists) then
        write(*,*) 'Error: EyeSys files have to be in pairs'
        write(*,*) 'No corresponding',inputfile2,'for',inputfile1
        return
       endif
      else
       file_pfx=index(inputfile1(file_idx-2:file_idx-1),"RA")
       if (file_pfx /= 0) then
        inputfile2=inputfile1
        inputfile1=replacestr(string=inputfile2,search="RA",substitute="XX")
        inquire(file=trim(inputfile1), exist=exists)
        if(.NOT.exists) then
         write(*,*) 'Error: EyeSys files have to be in pairs'
         write(*,*) 'No corresponding',inputfile1,'for',inputfile2
         return
        endif
       else
        write(*,*) 'Error parsing EyeSys file name'
        return
       endif
      endif
    TestData=0 ; MM=360; N=16   ! EyeSys
    write(*,*) "EyeSys files: ",inputfile1," ",inputfile2
   endif

call CPU_TIME(time_start)
  if (allocated(RadSlope%r)) then
   write(*,*) 'Radslope,DiaSlope already allocated'
  else
   call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  endif
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
     JMatrix1%R(:,:)=JMatrix%R(:,:)
     JMatrix1%Z(:,:)=JMatrix%Z(:,:)    
     JMatrix1%YPR(:,:)=JMatrix%YPR(:,:)
     JMatrix1%YPTHETA(:,:)=JMatrix%YPTHETA(:,:)  
     JMatrix1%THT(:)=JMatrix%THT(:)
     JMatrix1%SAGC(:,:)=JMatrix%SAGC(:,:)
     JMatrix1%INSTC(:,:)=JMatrix%INSTC(:,:)
     JMatrix1%INSTC2(:,:)=JMatrix%INSTC2(:,:)
     JMatrix1%MEANC(:,:)=JMatrix%MEANC(:,:)
     JMatrix1%MONGEA(:,:)=JMatrix%MONGEA(:,:)
     JMatrix1%RC(:)=JMatrix%RC(:)
     JMatrix1%LIOC(:,:)=JMatrix%LIOC(:,:)
     JMatrix1%MV(:)=JMatrix%MV(:)  
     JMatrix1%R0=JMatrix%R0
     JMatrix1%Z0(:)=JMatrix%Z0(:)
     JMatrix1%THT0=JMatrix%THT0
     JMatrix1%SAGC0(:)=JMatrix%SAGC0(:)
     JMatrix1%INSTC0(:)=JMatrix%INSTC0(:)
     JMatrix1%INSTC20(:)=JMatrix%INSTC20(:)
     JMatrix1%MEANC0(:)=JMatrix%MEANC0(:)
     JMatrix1%MONGEA0(:)=JMatrix%MONGEA0(:)
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
!    save for vertex normals     
     JMatrix%YPR(j,i)=YPR
     JMatrix%YPTHETA(j,i)=YPTHETA
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
!   call execute_command_line ("gnuplot -p plotcenter.gnu &", exitstat=i)
   endif

  if (TestData .ge. 2 .AND. TestData .le. 5) then
! READ THE PENTACAM DATA
! ELE are elevations CUR are "sagittal" curvatures in a 141x141 -7 to 7 mm square -1 is no data
! .ELE.CSV or .CUR.CSV versions have less text but use semicolons (;) instead of -1
   call init_mat_Penta(NP,Penta,Skyline)   !allocate the PentaCam matices
   call RCNVRTP(TestData,inputfile1)
!  arrange the data
   call CPU_TIME(time_start)
   Skyline=Penta
!  convert to polar with splining; makes round rings as above with 180x22 - also already has either center value Z0(1) or SAGC0(1)
   call RadSlope_eq_Skyline(JMatrix, RadSlope, Skyline, Penta)  !needs Penta for border check populates RadSlope with ZFCT
   call CPU_TIME(time_end)
   write(*,*) 'Time to convert Penta: ',(time_end-time_start)*1000
   Penta = 0              ! deallocate
   Skyline = 0
    do i=1,MM              
     do j=1,RadSlope%MV(i)
      if (TestData.eq.2 .or. TestData.eq.4) then ! put elevation into Zp for splining
       RadSlope%Zp(j,i)=JMatrix%Z(j,i)
      endif
     end do
    end do
   DiaSlope=RadSlope              ! move to diagonal format   
   DiaSlope%Zpd2 = .n. DiaSlope                               
   call MakeRadSplineCenter                                   
   call WriteCenter(RadSlope,'Center.dat')
!   call execute_command_line ("gnuplot -p plotcenter.gnu &", exitstat=i)
   if (TestData.eq.2 .or. TestData.eq.4) then  ! no valid data from Skyline=Penta
    JMatrix%SAGC0(2)=1E30  ;  JMatrix%SAGC0(3)=-1E30
   endif
   JMatrix%INSTC0(2)=1E30  ;  JMatrix%INSTC0(3)=-1E30
   JMatrix%INSTC20(2)=1E30 ;  JMatrix%INSTC20(3)=-1E30
   JMatrix%MEANC0(2)=1E30  ;  JMatrix%MEANC0(3)=-1E30
   JMatrix%MONGEA0(2)=1E30 ;  JMatrix%MONGEA0(3)=-1E30
   do i=1,MM
    do j=1,RadSlope%MV(i)
!    SplineEval1Dx1D works on values in DiaSlope
     if (TestData.eq.3 .or. TestData.eq.5) then  ! already have SAGC from .CUR and .CUR.CSV                       
      call SplineEval1Dx1D(1,JMatrix%R(j,i),JMatrix%THT(i),JMatrix%Z(j,i),YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA)  !integrate for CUR version with slope
     else  !TestData.eq.2 .or. TestData.eq.4
      call SplineEval1Dx1D(0,JMatrix%R(j,i),JMatrix%THT(i),JMatrix%Z(j,i),YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA)  !do not integrate for ELE version with slope
      call AXIALP(JMatrix%R(j,i),YPR,YP2R2,JMatrix%SAGC(j,i))  !generate SAGC
     endif
!    save for vertex normals     
     JMatrix%YPR(j,i)=YPR
     JMatrix%YPTHETA(j,i)=YPTHETA     
     call INSTANTP(JMatrix%R(j,i),YPR,YPTHETA,YP2R2,JMatrix%INSTC(j,i),JMatrix%INSTC2(j,i))
     call MEANP(JMatrix%THT(i),JMatrix%R(j,i),YPR,YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MEANC(j,i))
     call MONGEA(JMatrix%THT(i),JMatrix%R(j,i),YPR,YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MONGEA(j,i))
!    find min and max
     if (TestData.eq.2 .or. TestData.eq.4) then  ! no valid data from Skyline=Penta
      if (JMatrix%SAGC(j,i) <= JMatrix%SAGC0(2)) JMatrix%SAGC0(2)=JMatrix%SAGC(j,i)
      if (JMatrix%SAGC(j,i) >= JMatrix%SAGC0(3)) JMatrix%SAGC0(3)=JMatrix%SAGC(j,i)
     endif
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

  if (TestData .ge. 0) then  ! all data files (not test) needs central values computed unless they already exist
   if (TestData.eq.3 .or. TestData.eq.5 .or. TestData.eq.0 .or. TestData.eq.1) then 
    call SplineEval1Dx1D(1,JMatrix%R0,JMatrix%THT0,JMatrix%Z0(1))  !center value of elevation; needs integration from slopes
   endif  !TestData.eq.2 .or. TestData.eq.4  already has valid Z0 from cornea_arrays & ELE file
   if (TestData.ne.3 .and. TestData.ne.5) then  !TestData.eq.3 .or. TestData.eq.5  already has valid SAGC0 from cornea_arrays & CUR file
!  Reload RadSlope & re-spline
    do i=1,MM
     do j=1,RadSlope%MV(i)
      RadSlope%Zp(j,i)=JMatrix%SAGC(j,i)
     end do
    end do
    DiaSlope=RadSlope              ! move to diagonal format
    DiaSlope%Zpd2 = .n. DiaSlope   ! spline
    call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT0,JMatrix%SAGC0(1))  ! center value
    call RadSlope_eq_JMatrix(RadSlope,JMatrix)                        ! restore RadSlope
   endif

  else  
! OR GENERATE Fake data (EYESYS,ATLAS OR PENTA STYLE)

   call init_mat_EyeSys(MM,N,EyeSys) ! allocate the EyeSys matrices
   call init_mat_Atlas(MM,N,Atlas)
   call init_mat_Penta(NP,Penta,Skyline)   ! allocate the PentaCam matices   
   call RCNVRTT(MM,N,NP)
!   uncomment next two lines to test fake Penta data
   Skyline=Penta
   call RadSlope_eq_Skyline(JMatrix, RadSlope, Skyline, Penta)  !needs Penta for border check populates RadSlope with ZFCT
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

! simple subtraction the second time through
  if (allocated(JMatrix1%R)) then
     JMatrix%SAGC(:,:)=JMatrix1%SAGC(:,:)-JMatrix%SAGC(:,:)
     JMatrix%INSTC(:,:)=JMatrix1%INSTC(:,:)-JMatrix%INSTC(:,:)
     JMatrix%INSTC2(:,:)=JMatrix1%INSTC2(:,:)-JMatrix%INSTC2(:,:)
     JMatrix%MEANC(:,:)=JMatrix1%MEANC(:,:)-JMatrix%MEANC(:,:)
     JMatrix%MONGEA(:,:)=JMatrix1%MONGEA(:,:)-JMatrix%MONGEA(:,:)
     JMatrix%Z0(:)=JMatrix1%Z0(:)-JMatrix%Z0(:)
     JMatrix%SAGC0(:)=JMatrix1%SAGC0(:)-JMatrix%SAGC0(:)
     JMatrix%INSTC0(:)=JMatrix1%INSTC0(:)-JMatrix%INSTC0(:)
     JMatrix%INSTC20(:)=JMatrix1%INSTC20(:)-JMatrix%INSTC20(:)
     JMatrix%MEANC0(:)=JMatrix1%MEANC0(:)-JMatrix%MEANC0(:)
     JMatrix%MONGEA0(:)=JMatrix1%MONGEA0(:)-JMatrix%MONGEA0(:)   
  endif

endif !(flag == 0)

!Zernike coefficents
if (flag == 1) then
  if (allocated(JMatrix%R)) then
! Try to generate Zernike coefficients based on central elevations & lsq to Zernike polynomials
!  call CPU_TIME(time_start)
  time_start=omp_get_wtime()
  MM=180; N=22; NP=141
!  Reload RadSlope & respline
   do i=1,MM
    do j=1,RadSlope%MV(i)
     RadSlope%Zp(j,i)=JMatrix%Z(j,i)
    end do
   end do
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope   ! generate the splines diagonally (generate zp2)

! allocate working matrices
   nrhs=(MM*N+1)
   kk_max=5*12
   k=0
   do m=-4,4
    do nn=ABS(m),4
     if (mod(nn-m,2) == 0) then
      k=k+1
!     write(*,*) 'k,n,m: ',k,nn,m
     endif
    end do
   end do
   k_max=k   
   allocate (B_Matrix(k_max,kk_max),ZernC(kk_max,nrhs),rlocal(kk_max),thtlocal(kk_max),stat=ierr) ! ZernC(kk_max) to hold data though only k_max Zernike coeficients
   if (ierr /= 0) then
    write(*,*) 'unable to allocate memory in Zernike'
    return
   endif
   ZernC=0
   if (allocated(ZernJ%ZC)) then
   ! nothing
   else
    call init_mat_ZernJ(MM,N+1,ZernJ)
   endif

!!!$OMP PARALLEL DO PRIVATE(ii,i1,j1,i,j,kk,ctr_circle_x,ctr_circle_y,Y_global,X_global,R_Talus,Theta_Talus,rlocal,thtlocal)
do ii=1,nrhs
!  cycle through i1 1 to MM and j1 1 to N with one point for origin at N+1
   i1=mod(ii,MM)
   j1=int(ii/MM)+1
   if (i1 .eq. 0) then
    i1=MM
    j1=j1-1
   endif

!  center of local geometry
   if (ii .LT. nrhs) then
    ctr_circle_x=JMatrix%R(j1,i1)*cos(JMatrix%THT(i1))
    ctr_circle_y=JMatrix%R(j1,i1)*sin(JMatrix%THT(i1))
   else ! last one is origin
    ctr_circle_x=0.0
    ctr_circle_y=0.0
   endif
   kk=0   
   do i=1,5
    do j=1,12  !kk_max=5*12
    kk=kk+1
!   local cylindrical coordinates
    rlocal(kk)=(i-1)/4.0  ! r goes from 0 to 1
    thtlocal(kk)=2*PI*(j-1)/12  ! tht from 0 to 2*Pi without overlap
!   global cylindrical coordinates
    Y_global=(rlocal(kk)*sin(thtlocal(kk))-ctr_circle_y)
    X_global=(rlocal(kk)*cos(thtlocal(kk))-ctr_circle_x)
    R_Talus=sqrt(X_global*X_global+Y_global*Y_global)
    if (ABS(X_global) > EPS .AND. ABS(Y_global) > EPS) then
     if (X_global > 0 .AND. Y_global > 0 ) then
      Theta_Talus=ATan(Y_global/X_global)
     endif
     if (X_global < 0 .AND. Y_global > 0 ) then
      Theta_Talus=ATan(Y_global/X_global)+PI
     endif
     if (X_global < 0 .AND. Y_global < 0 ) then
      Theta_Talus=ATan(Y_global/X_global)+PI
     endif
     if (X_global > 0 .AND. Y_global < 0 ) then
      Theta_Talus=ATan(Y_global/X_global)+2*PI
     endif
     call SplineEval1Dx1D(1,R_Talus,Theta_Talus,ZernC(kk,ii))  ! elevation for Zernike; use coefficient vector as temporary storage
    else
     Theta_Talus=0
     call SplineEval1Dx1D(1,R_Talus,Theta_Talus,ZernC(kk,ii))  ! elevation for Zernike; use coefficient vector as temporary storage   
    endif 
    end do
   end do
   end do  ! end ii to nrhs
!!!$OMP END PARALLEL DO

   call RadSlope_eq_JMatrix(RadSlope,JMatrix)                      ! restore RadSlope

! generate the Zpolynomial degree_polynomial values for each point, makes a matrix degree_polynomials x length_data
! if n >= 0 ABS(m) <= n  & mod(n-m,2) = 0
  do kk=1,kk_max   
   k=0
   do m=-4,4
    do nn=ABS(m),4
     if (mod(nn-m,2) == 0) then
      k=k+1
      B_Matrix(k,kk)=zern(nn,m,rlocal(kk),thtlocal(kk))  ! local cylindrical coordinates
     else
      cycle
     endif
    end do
   end do
  end do

! solve the LSQ equations for ZernC(k): solution is degree_polynomials number of coefficients;  B_Matrix(k,kk)*ZernC(k)=z(kk) 
! Use normal equation XTX.c=X.z ie. B_Matrix(k,kk)*ZernC(k)=z(kk) or use LAPACKs dgels()
! only have to call this once; NRHS can be for the whole talus plot since B_Matrix is invariant.
! have to allocate WORK
!   allocate(XTX(k_max,k_max),EE(k_max),IPIV(k_max))
!   XTX=matmul(B_matrix,Transpose(B_matrix))
!   EE=matmul(B_matrix,ZernC(:,1))
!   call DGESV(k_max,1,XTX,k_max,IPIV,EE,k_max,INFO) ! overwrites EE into solution 
!   call GaussJordan(k_max, NRHS ,XTX ,k_max , EE, k_max, INFO )   ! overwrites EE into solution
  LWORK = min(k_max,kk_max) + max( min(k_max,kk_max), nrhs )
  allocate (WORK(LWORK))! WORK is dimension LWORK
  call DGELS( 'T', k_max, kk_max, nrhs, B_Matrix, k_max, ZernC , kk_max, WORK, LWORK, INFO ) ! overwrites ZernC (only to k_max)
 
! to plot "talus" instead of center, pick a point with circle around it; same thing as above, plot the vertical coma vs position; will be compute more intensive
write(*,*) 'k_max,kk_max, info: ',k_max,kk_max,info

!$OMP PARALLEL DO PRIVATE(i1,j1,i,j,kk)
do kk=1,nrhs
!  cycle through i1 1 to MM and j1 1 to N with one point for origin at N+1
i1=mod(kk,MM)
j1=int(kk/MM)+1
if (i1 .eq. 0) then
 i1=MM
 j1=j1-1
endif
  ZernJ%ZC(j1,i1,1:k_max)=ZernC(1:k_max,kk)
end do
!$OMP END PARALLEL DO

! center values
do k=1,15
 ZernJ%ZC0(1,:)=ZernC(1:k_max,nrhs)
end do
! find min and max
ZernJ%ZC0(2,:)=1E30
ZernJ%ZC0(3,:)=-1E30
do i =1,MM
 do j = 1,RadSlope%MV(i)
  do k = 1,15
   if (ZernJ%ZC(j,i,k) <= ZernJ%ZC0(2,k)) ZernJ%ZC0(2,k)=ZernJ%ZC(j,i,k)
   if (ZernJ%ZC(j,i,k) >= ZernJ%ZC0(3,k)) ZernJ%ZC0(3,k)=ZernJ%ZC(j,i,k)
  end do
 end do
end do

write(*,*) 'center Zernike values: ',ZernC(1:k_max,nrhs)
write(*,*) ' '
! Done with Zernike
  !deallocate(XTX,EE,IPIV)  !if used above
  deallocate(WORK,B_Matrix,ZernC,rlocal,thtlocal)
!  call CPU_TIME(time_end)
  time_end=omp_get_wtime()
  write(*,*) 'Time to compute Zernike: ',(time_end-time_start)
  return
 else
  return ! if flag==1 and not allocated do nothing
 endif
endif

!  use fillarray to fill DiaSlope Zp with calculated value based on IuseG, optionally generate LIOC
!  using SplineEval1Dx1D to refill a new matrix RadSlope using f0, derivatives to get calculated powers

   call RadSlope_eq_JMatrix(RadSlope,JMatrix)
   DiaSlope=RadSlope
   DiaSlope%Zpd2 = .n. DiaSlope
   LinesOfCurv='LIOC.CAR'

!  Generate LIOC with vector format
   call FILLARRAY(8,LinesOfCurv,POWMIN2,POWMAX2)    ! don't redo bounds consider optional !  plot 'LIOC.CAR' using 1:2:3:4 with vectors
!   call execute_command_line ("gnuplot -p plotlioc.gnu &", exitstat=i)

!  write OFF files

   donut = .FALSE.

   powctr=JMatrix%SAGC0(1)  
   powmin=JMatrix%SAGC0(2)  
   powmax=JMatrix%SAGC0(3)
   write(*,*) 'powctr,POWMIN,POWMAX',powctr,POWMIN,POWMAX


! needs a flag to select function as well as OFF/PLY etc, or call directly from kernunos?

! writes values in openGL friendly format to matrices for passing to C/C++
! flag determines what to write for elevation and color
  call Geom(flag, JMatrix, donut, powmin, powmax, elements, vertices, nV, nE)


! eigenvalues show shape of RadSlope without make_rings but with FillArray 7 elevations
!  atmp=pca(2,RadSlope) 
!  atmp=pca(3,RadSlope)


!gnuplot output

BigPlot='BIG.CAR'

   call CPU_TIME(time_start)   
   call RadSlope_eq_JMatrix(RadSlope,JMatrix)  
   DiaSlope=RadSlope            
   DiaSlope%Zpd2 = .n. DiaSlope             
   call FILLARRAY(4,LinesOfCurv,POWMIN,POWMAX)
   write(*,*) 'POWMIN,POWMAX',POWMIN,POWMAX
   call CPU_TIME(time_end)
   write(*,*) 'Time to make plot',i,(time_end-time_start)*1000   

! GENERATE PRINT FILES
  call WRITEARRAY(RadSlope,BigPlot)

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
   CALL PRINTGRAPH(unitno1,POWMIN,POWMAX,BigPlot)
   WRITE(unitno1,*) 'pause mouse close'  !this allows the file to be opened by gnuplot by clicking on it without closing the window
   CLOSE (unitno1)

!   call execute_command_line ("gnuplot -p plot2.gnu &", exitstat=i)

  write(*,*) 'Done: janus'

  return        

  END subroutine janus
