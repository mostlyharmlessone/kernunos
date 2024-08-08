  subroutine Janus(flag,file_from_C,elements,vertices,legend,zern,nV,nE,nL,nZ) bind(C,name='janus_')
! DRIVER PROGRAM FOR SPLINE ROUTINES
  use set_precision, ONLY : wp
  use lapackinterface
  use cornea_arrays
  use special_fct
  use io_functions
  use spline_interfaces
  use, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char
  use c_interfaces, ONLY : LogC, Ccounter, ConvertPLYtoBIN
  use omp_lib
  IMPLICIT NONE
  integer :: i, j, k, ii, kk, m, nn, i1, j1, ierr, info, nrhs
  integer,save :: MM, N ,M1, N1, Power_Rings_Count
  integer,save :: TestData             ! TestData: -1=test, 0=EyeSys, 1=Atlas, (2-5)=Penta
  integer,save :: NP                        ! PentaCam=141
  integer :: unitno1
  character(c_char), INTENT(IN), DIMENSION(4096) :: file_from_C
  integer(c_int), INTENT(INOUT) :: flag
  integer(c_int), INTENT(INOUT) :: nV 
  integer(c_int), INTENT(INOUT) :: nE               
  real(c_float), INTENT(INOUT) :: vertices(*)
  integer(c_int), INTENT(INOUT) :: elements(*)
  integer(c_int), INTENT(INOUT) :: nL
  real(c_float), INTENT(INOUT) :: legend(*)
  integer(c_int), INTENT(INOUT) :: nZ
  real(c_float), INTENT(INOUT) :: zern(*)
  character(len=8) :: LinesOfCurv
  character(len=4096) :: new_path
  character(:),save, ALLOCATABLE :: inputfile1,inputfile2,BigPlot
  character(:),save, ALLOCATABLE :: logfile
  integer ::  nblines, file_idx, file_pfx,read_error,io
  integer,allocatable :: MV(:)
  real(8) :: time_start, time_end
  real(wp) :: POWMIN,POWMAX,POWMAX2,POWCTR,POW
  logical :: donut, exists
  real(wp) :: Y,YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA,rBi,rBo
  integer :: k_max, kk_max, iflag !,LWORK
  integer(c_int) :: dat, fct, map
  real(wp), allocatable :: zernC(:,:), B_Matrix(:,:), rlocal(:), thtlocal(:) !,WORK(:)
  real(wp), allocatable :: XTX(:,:),EE(:,:)
  integer, allocatable :: IPIV(:)
  real(wp) :: ctr_circle_x, ctr_circle_y, R_Talus, Theta_Talus, X_global, Y_global

write(*,*) 'flag to Fortran:',flag
write(*,*) 'flag(action) last digits to Fortran:',mod(flag,100)
if (mod(flag,100) /= 0) then ! changed by new file, if there are previous values from last call, these are the existing values
 !these used to be in cornea_arrays and were static==implicitly saved, now they are locally saved
 write(*,*) 'previous MM,N,TestData: ',MM,N,TestData
endif
dat=(flag-mod(flag,1000000))/1000000 ! first two digits
write(*,*) 'dat to Fortran:',dat
write(*,*) 'tweaks(dat) to Fortran:',btest(dat, 0),btest(dat, 1),btest(dat, 2),btest(dat, 3),btest(dat, 4)
fct=mod(((flag-mod(flag,10000))/10000),100)
write(*,*) 'fct to Fortran:',fct
map=mod((flag-mod(flag,100))/100,100)
write(*,*) 'color(map) to Fortran:',map
! dat = first binary bit 0/1 centernode tweak ie btest(dat,0) = .true.
! dat = second binary bit 0/1 shift r-values tweak ie btest(dat,1) = .true.
! integration of slopes for elevation:
! dat = third binary bit 0/1 cubic spline integration (=1)(ie btest(dat,2) = .true.) vs trapezoidal rule (default = 0)
! dat =fourth binary bit 0/1 fillin2 cannot be combined with splinefillin ie btest(dat,3) = .true.
! dat =fifth binary bit 0/1 splinefillin cannot be combined with lsqfillin ie btest(dat,4) = .true.

! iflag passing of dat to SplineEval1Dx1D centernode splines and integration of splines
! first digit iflag-mod(iflag,10))/10
! second digit mod(iflag,10)
!btest(dat, 2)     T   F
!
!              T  12  11
!btest(dat,0)
!              F  02  01

if (btest(dat, 2)) then
 if (btest(dat,0)) then
  iflag=12
 else
  iflag=2
 endif
else
 if (btest(dat,0)) then
  iflag=11
 else
  iflag=1
 endif
endif
write(*,*) 'iflag for SplineEval1Dx1D:',iflag

! mod(flag,100) == 99 Deallocate
if (mod(flag,100) == 99) then
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
    if (allocated(EyeSys%RA)) then
     EyeSys=0
    endif
    if (allocated(Atlas%AR)) then
     Atlas=0
    endif
    if (allocated(Penta%DAT)) then
     Penta=0
    endif
    if (allocated(Skyline%DAT)) then
     Skyline=0
    endif
    return
endif

if (mod(flag,100) == 0 .or. mod(flag,100) == 2 .or. mod(flag,100) == 3) then  !only need new file name if opening a file or printing, local save of inputfile1,inputfile2,logfile
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

 write(*,*) 'file from kernunos: ',trim(new_path)
 nblines=len(trim(new_path))
 if (allocated(inputfile1)) then
  deallocate(inputfile1)
  deallocate(inputfile2)
  deallocate(logfile)
  deallocate(BigPlot)
 endif
 allocate(character(nblines) :: inputfile1)
 allocate(character(nblines) :: logfile)
 inputfile1=trim(new_path)
 allocate(character(nblines) :: inputfile2)
 allocate(character(nblines) :: BigPlot)
endif  ! mod(flag,100) == 0

! Writes ASCII PLY file
if (mod(flag,100) == 3) then
if (allocated(JMatrix%R)) then
file_idx=index(inputfile1, ".ply")
 if( file_idx == 0) then
   write(*,*) inputfile1, 'is not a ply file'
   return
  else
  donut = .FALSE.
  if (fct .lt. 16 .and. fct .gt. 0) then
    powctr=JMatrix%ZC0(1,fct)
    powmin=JMatrix%ZC0(2,fct)
    powmax=JMatrix%ZC0(3,fct)
  else
  SELECT CASE (fct)
    CASE (0)
    powctr=JMatrix%SAGC0(1)
    powmin=JMatrix%SAGC0(2)
    powmax=JMatrix%SAGC0(3)
    CASE (16)
    powctr=JMatrix%INSTC0(1)
    powmin=JMatrix%INSTC0(2)
    powmax=JMatrix%INSTC0(3)
    CASE (17)
    powctr=JMatrix%INSTC20(1)
    powmin=JMatrix%INSTC20(2)
    powmax=JMatrix%INSTC20(3)
    CASE (18)
    powctr=JMatrix%MEANC0(1)
    powmin=JMatrix%MEANC0(2)
    powmax=JMatrix%MEANC0(3)
    CASE (19)
    powctr=JMatrix%MONGEA0(1)
    powmin=JMatrix%MONGEA0(2)
    powmax=JMatrix%MONGEA0(3)
    CASE (20)
    powctr=JMatrix%Z0(1)
    powmin=JMatrix%Z0(2)
    powmax=JMatrix%Z0(3)
    CASE DEFAULT
    powctr=JMatrix%SAGC0(1)
    powmin=JMatrix%SAGC0(2)
    powmax=JMatrix%SAGC0(3)
 END SELECT
 endif
  write(*,*) 'powctr,POWMIN,POWMAX',powctr,POWMIN,POWMAX
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
if (mod(flag,100) == 2) then
if (allocated(JMatrix%R)) then
file_idx=index(inputfile1, ".off")
 if( file_idx == 0) then
   write(*,*) inputfile1,'is not an off file'
   return
  else
  donut = .FALSE.
  if (fct .lt. 16 .and. fct .gt. 0) then
    powctr=JMatrix%ZC0(1,fct)
    powmin=JMatrix%ZC0(2,fct)
    powmax=JMatrix%ZC0(3,fct)
  else
  SELECT CASE (fct)
    CASE (0)
    powctr=JMatrix%SAGC0(1)
    powmin=JMatrix%SAGC0(2)
    powmax=JMatrix%SAGC0(3)
    CASE (16)
    powctr=JMatrix%INSTC0(1)
    powmin=JMatrix%INSTC0(2)
    powmax=JMatrix%INSTC0(3)
    CASE (17)
    powctr=JMatrix%INSTC20(1)
    powmin=JMatrix%INSTC20(2)
    powmax=JMatrix%INSTC20(3)
    CASE (18)
    powctr=JMatrix%MEANC0(1)
    powmin=JMatrix%MEANC0(2)
    powmax=JMatrix%MEANC0(3)
    CASE (19)
    powctr=JMatrix%MONGEA0(1)
    powmin=JMatrix%MONGEA0(2)
    powmax=JMatrix%MONGEA0(3)
    CASE (20)
    powctr=JMatrix%Z0(1)
    powmin=JMatrix%Z0(2)
    powmax=JMatrix%Z0(3)
    CASE DEFAULT
    powctr=JMatrix%SAGC0(1)
    powmin=JMatrix%SAGC0(2)
    powmax=JMatrix%SAGC0(3)
 END SELECT
 endif
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

! last two digits of flag == 0 parse file name, assign TestData type and MM,N
if (mod(flag,100) == 0) then
 call CCounter(0)
! From either RA?.? or XX?.?, set inputfile1 to the XX version, inputfile1 to the RA version.
! For either .CUR or .ELE or _CUR.CSV or _ELE.CSV set inputfile1 to Penta file of appropriate type with TestData
! For CSV but not _ELE.CSV or _CUR.CSV set inputfile1 to Atlas file
 file_idx=index(inputfile1, "RA")+index(inputfile1, "XX")
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
         BigPlot=trim("test.PLT")
         TestData=-1; MM=360; N=16 ; NP=141
        else
!        inputfile2=replacestr(string=inputfile1,search="ELE",substitute="CUR")
        BigPlot=replacestr(string=inputfile1,search="ELE",substitute="PLT")
        TestData=2; MM=180; N=22; NP=141 ! PentaCam ELE
       endif
      else
!       inputfile2=inputfile1
!       inputfile1=replacestr(string=inputfile2,search="CUR",substitute="ELE")
        BigPlot=replacestr(string=inputfile1,search="CUR",substitute="PLT")
       TestData=3; MM=180; N=22; NP=141 ! PentaCam CUR
      endif
      else
       file_idx=index(inputfile1, "_CUR")
       if( file_idx == 0) then
        file_idx=index(inputfile1, "_ELE")
        if( file_idx == 0) then
         TestData=1; MM=180; N=25   ! Atlas 900 can be 25, 9000 seems to be 22
         BigPlot=replacestr(string=inputfile1,search="CSV",substitute="PLT")
         write(*,*) "Atlas file: ",inputfile1
        else
        write(*,*) 'Not an Atlas file'
 !       inputfile2=replacestr(string=inputfile1,search="ELE",substitute="CUR")
        BigPlot=replacestr(string=inputfile1,search="CSV",substitute="PLT")
        TestData=4; MM=180; N=22; NP=141 ! PentaCam ELE.CSV
        endif
        else
        write(*,*) 'Not an Atlas file'
!       inputfile2=inputfile1
!       inputfile1=replacestr(string=inputfile2,search="CUR",substitute="ELE")
        BigPlot=replacestr(string=inputfile1,search="CSV",substitute="PLT")
        TestData=5; MM=180; N=22; NP=141 ! PentaCam CUR.CSV
       endif
      endif
   else
!  EyeSys
      write(*,*) 'prefix is found at index: ',file_idx,"length: ",len(inputfile1)
      write(*,*) 'prefix:',inputfile1(file_idx:file_idx+1)
      write(*,*) 'inputfile1: ',inputfile1
       file_idx=index(inputfile1, "XX")  !index(inputfile1, "XX", back)
    !   file_pfx=index(inputfile1(file_idx:file_idx+1),"XX")
      if (file_idx /= 0) then
       inputfile2=replacestr(string=inputfile1,search="XX",substitute="RA")
       BigPlot=replacestr(string=inputfile1,search="RA",substitute="PL")
       inquire(file=trim(inputfile2), exist=exists)
       if(.NOT.exists) then
        write(*,*) 'Error: EyeSys files have to be in pairs, or file name has XX other than prefix'
        write(*,*) 'No corresponding',inputfile2,'for',inputfile1
        return
       endif
      else
       file_idx=index(inputfile1, "RA") !index(inputfile1, "RA", back)
!       file_pfx=index(inputfile1(file_idx:file_idx+1),"RA")
       if (file_idx /= 0) then
        inputfile2=inputfile1
        inputfile1=replacestr(string=inputfile2,search="RA",substitute="XX")
        BigPlot=replacestr(string=inputfile1,search="XX",substitute="PL")
        inquire(file=trim(inputfile1), exist=exists)
        if(.NOT.exists) then
         write(*,*) 'Error: EyeSys files have to be in pairs, or file name has RA other than prefix'
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
 endif ! (mod(flag,100) == 0) parsing the file name,assigning TestData type and MM,N


! allocate JMatrix needed for file import
!  JMatrix is 180x22 to make importing from Atlas easier.
!  M1,N1 ot avoid overwriting MM,N at this point
   M1=180
   N1=22
   if (allocated(JMatrix%R)) then
    write(*,*) 'JMatrix allocated'
    if (allocated(JMatrix1%R)) then
     write(*,*) 'JMatrix1 allocated'
    else
     write(*,*) 'allocating JMatrix1'
     call init_mat_JMatrix(M1,N1,JMatrix1)
    endif
    ! always store the last JMatrix in JMatrix1
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
    JMatrix1%RC(:,:)=JMatrix%RC(:,:)
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

if (TestData .eq. 0) then
 MM=360 ; N=16 ! EyeSys if file not read; should not be necessary as should agree with previous value.
 ! wipe RadSlope/DiaSlope clean to ensure the correct MM,N based on previous assignment
 if (allocated(RadSlope%r)) then
  write(*,*) 'Radslope,DiaSlope need to be reallocated'
  RadSlope = 0 ; DiaSlope = 0 ; deallocate(RadSplineCenter)
  call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
 else
  call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
 endif
 if(.not.allocated(EyeSys%RA)) then
  call init_mat_EyeSys(MM,N,EyeSys) ! allocate the EyeSys matrices
 endif
 if (mod(flag,100) == 0) then !read the files
! READ THE EYESYS DATA
! XX????? ARE THE AXIAL DIST. RX???? ARE THE MIRE RADII  
   call CPU_TIME(time_start)
   read_error=0
   call RCNVRTE(inputfile2,inputfile1,read_error)
   call CPU_TIME(time_end)
   write(*,*) 'Time to read EyeSys files: ',(time_end-time_start)*1000
   if (read_error > 0) return
  endif  !(mod(flag,100) /= 0,99,2,3 must be 1 or 4, reload the original data
! Generate the slope matrix using ZFCT
  RadSlope=EyeSys
endif

! READ THE ATLAS DATA
if (TestData .eq. 1) then
 MM=180
 if (mod(flag,100) == 0) then !read the files
  read_error=0
  call CPU_TIME(time_start)
 ! determine the type, prior to allocating Atlas
  call RCNVRTA_type(inputfile1,Power_Rings_Count,read_error)
  if (read_error .eq. 1) then
   write(*,*) 'Possible semicolon delimited Atlas file, try sed'
   inputfile2=replacestr(string=inputfile1,search=".CSV",substitute=".TMP")
   write(*,*) 'sed "s/;/,/g" ' // inputfile1 // ' > ' // inputfile2
   call system('sed "s/;/,/g" ' // inputfile1 // ' > ' // inputfile2, io)
   if (io > 0) then
    write (*,*) 'system command to sed failed'
    write (*,*) 'Consider using your text editor to search/replace all semicolons with commas in',inputfile1
   else
    call RCNVRTA_type(inputfile2, Power_Rings_Count, read_error)
    if (read_error > 0) write (*,*) 'temp Atlas file read error, probably not because semicolon delimited'
    call system('rm ' // inputfile2, io)
    if (io > 0) write (*,*) 'system command to remove tmp file failed'
   endif
  endif
  N=Power_Rings_Count
  if(.not.allocated(Atlas%AR)) then
   call init_mat_Atlas(MM,N,Atlas)
  else
   Atlas=0
   call init_mat_Atlas(MM,N,Atlas)
  endif
  read_error=0
  call RCNVRTA(inputfile1,N,read_error)
  if (read_error .eq. 1) then
   write(*,*) 'Possible semicolon delimited Atlas file, try sed'
   inputfile2=replacestr(string=inputfile1,search=".CSV",substitute=".TMP")
   write(*,*) 'sed "s/;/,/g" ' // inputfile1 // ' > ' // inputfile2
   call system('sed "s/;/,/g" ' // inputfile1 // ' > ' // inputfile2, io)
   if (io > 0) then
    write (*,*) 'system command to sed failed'
    write (*,*) 'Consider using your text editor to search/replace all semicolons with commas in',inputfile1
   else
    call RCNVRTA(inputfile2, N, read_error)
    if (read_error > 0) write (*,*) 'temp Atlas file read error, probably not because semicolon delimited'
    call system('rm ' // inputfile2, io)
    if (io > 0) write (*,*) 'system command to remove tmp file failed'
   endif
  endif
  call CPU_TIME(time_end)
  write(*,*) 'Time to read Atlas CSV file: ',(time_end-time_start)*1000
  if (read_error > 0) return
 endif  !(mod(flag,100) /= 0,99,2,3 must be 1 or 4, reload the original data
 N=Power_Rings_Count
 ! wipe RadSlope/DiaSlope clean to ensure the correct MM,N based on previous assignment
 if (allocated(RadSlope%r)) then
  write(*,*) 'Radslope,DiaSlope need to be reallocated'
  RadSlope = 0 ; DiaSlope = 0 ; deallocate(RadSplineCenter)
  call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
 else
  call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
 endif
 RadSlope=Atlas
endif

! READ THE PENTACAM DATA
 if (TestData .ge. 2 .AND. TestData .le. 5) then
  call CPU_TIME(time_start)
  MM=180; N=22; NP=141   ! PentaCam
! wipe RadSlope/DiaSlope clean to ensure the correct MM,N based on previous assignment
  if (allocated(RadSlope%r)) then
   write(*,*) 'Radslope,DiaSlope need to be reallocated'
   RadSlope = 0 ; DiaSlope = 0 ; deallocate(RadSplineCenter)
   call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  else
   call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  endif
  if(.not.allocated(Penta%DAT)) then
   call init_mat_Penta(NP,Penta,Skyline)   !allocate the PentaCam matices
  endif
! ELE are elevations CUR are "sagittal" curvatures in a 141x141 -7 to 7 mm square -1 is no data
! _ELE.CSV or _CUR.CSV versions have less text but use semicolons (;) instead of -1
  if (mod(flag,100) == 0) then ! read the files
   read_error=0
   call RCNVRTP(TestData,inputfile1,read_error)
   if (read_error > 0) return
  endif  !(mod(flag,100) /= 0,99,2,3 must be 1 or 4, reload the original data
! arrange the data
  Skyline=Penta
! convert to polar with splining; makes round rings as above with 180x22 - also already has either center value Z0(1) or SAGC0(1)
  ! RadSlope_eq_Skyline puts elevation into JMatrix%Z(j,i) and possibly populates JMatrix%Z(j,i) with crap
  call RadSlope_eq_Skyline(JMatrix, RadSlope, Skyline, Penta)  !needs Penta for border check populates RadSlope with ZFCT
  call CPU_TIME(time_end)
  write(*,*) 'Time to convert Penta: ',(time_end-time_start)*1000
  if (TestData.eq.2 .or. TestData.eq.4) then ! ELE or ELE.CSV PentaCam files, put elevation into Zp for splining without integration
   RadSlope%Zp(:,:)=0 ; JMatrix%SAGC(:,:) = 0 ; JMatrix%SAGC0(:) = 0 ! ELE should not have anything in Zp or SAGC yet
   do i=1,MM
    do j=1,RadSlope%MV(i)
      RadSlope%Zp(j,i)=JMatrix%Z(j,i) !=RadSlope%Z(j,i) ! at this point
    end do
   end do
!   write(*,*) 'Central Elevation, min, max: ',JMatrix%Z0(1),JMatrix%Z0(2),JMatrix%Z0(3)
  else ! CUR version shouldn't have elevations yet
   JMatrix%Z(:,:) = 0 ; JMatrix%Z0(:) = 0
!   write(*,*) 'Central Sagittal power, min, max: ',JMatrix%SAGC0(1),JMatrix%SAGC0(2),JMatrix%SAGC0(3)
  endif  
 endif

! OR GENERATE Fake EyeSys data
if (TestData .lt. 0) then
 MM=360; N=16   ! fake EyeSys
! wipe RadSlope/DiaSlope clean to ensure the correct MM,N based on previous assignment
 if (allocated(RadSlope%r)) then
  write(*,*) 'Radslope,DiaSlope need to be reallocated'
  RadSlope = 0 ; DiaSlope = 0 ; deallocate(RadSplineCenter)
  call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
 else
  call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
 endif
 if(.not.allocated(EyeSys%RA)) then
  call init_mat_EyeSys(MM,N,EyeSys) ! allocate the EyeSys matrices
 endif
  if (mod(flag,100) == 0) then !read the files
   call RCNVRTT(MM,N)
  endif  !(mod(flag,100) /= 0,99,2,3 must be 1 or 4, reload the original data  endif
! Generate the slope matrix using ZFCT
  RadSlope=EyeSys
 endif

! fillin tweaks
!  FILL IN MISSING ATLAS RING DATA USING CIRCUMFERENTIAL SPLINES
!  R is not constant; they're not circles, so splining along the curve gives curvatures that
!  are not orthogonal to R, nor z2(deriv of theta)  probably best not to do this
if (TestData .eq. 1) then
 if (btest(dat, 4) .or. btest(dat, 3)) then
! make sure I have a backup of Atlas the same size as Atlas before fillin
  if(.not.allocated(AtlasSave%AR)) then
   N=size(Atlas%AP,2)
   MM=size(Atlas%AP,1)
   call init_mat_Atlas(MM,N,AtlasSave)
  else
   AtlasSave=0
   N=size(Atlas%AP,2)
   MM=size(Atlas%AP,1)
   call init_mat_Atlas(MM,N,AtlasSave)
  endif
 endif
 if (btest(dat, 4)) then
  AtlasSave=Atlas
  Atlas%AP=splinefillin(Atlas%AP)
  Atlas%AD=splinefillin(Atlas%AD)
  Atlas%AY=splinefillin(Atlas%AY)
 endif
!  FILL IN MISSING ATLAS RING DATA USING LSQ cosine series
 if (btest(dat, 3)) then
  AtlasSave=Atlas
  Atlas%AP=lsqfillin(Atlas%AP)
  Atlas%AD=lsqfillin(Atlas%AD)
  Atlas%AY=lsqfillin(Atlas%AY)
 endif
 if (btest(dat, 4) .or. btest(dat, 3)) then
  RadSlope=Atlas
  Atlas=AtlasSave  ! restore Atlas after using it to define RadSlope
!  Look at these intersecting rings using
!  gnuplot 'plot 'datafile dumped with' u 1:2'  (don't set polar) first option, or splot second option
!  do j=1,N
!    do i=1,MM
!     write(*,*) Atlas%DEG(i),Atlas%AP(i,j)
 !    write(*,*) Atlas%AD(i,j)*COS(PI*Atlas%DEG(i)/180.0),Atlas%AD(i,j)*SIN(PI*Atlas%DEG(i)/180.0),0
!    end do
!    write(*,*) ' '
!   end do
 endif
endif

! Spline RadSlope
  DiaSlope=RadSlope              ! move to diagonal format
  DiaSlope%Zpd2 = .n. DiaSlope   ! spline across center without tweaks

 if ( Testdata .eq. 1 ) then
  call MakeRadSplineCenter(0)    ! capture the spline center deviations from unmodified RadSlope
! Have to do AdjustSlope tweak before centernode, since centernode essentially reduces RadSplineCenter(1,:) to 0
  if (btest(dat, 1)) then         ! moving each meridian to align curves
   call AdjustRadSplineCenter     ! changes r only in DiaSlope
   DiaSlope%Zpd2 = .n. DiaSlope   ! re-spline, standard
  endif
  if (btest(dat, 0) ) then        ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
   call DiaSplineCenter(DiaSlope) ! re-spline, with center node
  endif
  call MakeRadSplineCenter(dat)        ! generates spline centers with tweaks
  endif

 ! Atlas spline consistency check and computation of elevation by power vs elevation in file
 if ( Testdata .eq. 1 ) then
  k=0 ; powmax2 = 0 ; powmax =0  ! Use these temporarily
 ! find max elevation from Atlas file
  do i=1,MM
   do j=1,RadSlope%MV(i)
    if (100*Atlas%AY(i,j) > powmax) powmax=100*Atlas%AY(i,j)
    end do
   end do
 ! check spline power & elevation at knots
   do i=1,MM
    do j=1,RadSlope%MV(i)
     if (i > 90) then
      call SplineEval1Dx1D(iflag,-100*Atlas%AD(i,j),PI*(i-1)/90.0_wp,Y,YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA)
      call AXIALP(-100*Atlas%AD(i,j),YPR,YP2R2,pow)
     else
      call SplineEval1Dx1D(iflag,100*Atlas%AD(i,j),PI*(i-1)/90.0_wp,Y,YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA)
      call AXIALP(100*Atlas%AD(i,j),YPR,YP2R2,pow)
     endif
!   skip missing elevation points to compute (cumulative) average error
    if (Atlas%AY(i,j) > 0) then
      k=k+1
      powmax2=powmax2+ABS(Y-powmax+100*Atlas%AY(i,j))
     endif
!   checks that power at knots is correct at knts
     if (ABS(Atlas%AP(i,j)-pow) > EPS .and. (Atlas%AP(i,j) .gt. 0) .and. (Atlas%AD(i,j) .gt. 0) .and. (Atlas%AY(i,j) .gt. 0) .AND. (Atlas%AR(i,j) > 0)) then
      write(*,*) 'Atlas power spline error in janus: ',j,i,Atlas%AP(i,j),pow
     endif
    end do
   end do
   write(*,*) 'Atlas avg abs elevation percent error : ',(100*powmax2/k)/powmax
 endif

! Make JMatrix
!  make round rings and if needed convert 360x16 or 180x25 to 180x22
! donut
! Find maximum radius from data in RadSlope
  rBo = 0
  do i=1,MM
   do j=1,N
    if (ABS(RadSlope%r(j,i)) >= rBo) rBo=ABS(RadSlope%r(j,i))
   end do
  end do
  rBo=rBo/100.   ! scaling
  rBi=0.05*rBo
!  min and max bounds
  JMatrix%SAGC0(2)=1E30   ;  JMatrix%SAGC0(3)=-1E30 ; JMatrix%SAGC0(1)=0
  JMatrix%Z0(2)=1E30      ;  JMatrix%Z0(3)=-1E30 ;    JMatrix%Z0(3)=0
  JMatrix%INSTC0(2)=1E30  ;  JMatrix%INSTC0(3)=-1E30 ; JMatrix%INSTC0(1)=0
  JMatrix%INSTC20(2)=1E30 ;  JMatrix%INSTC20(3)=-1E30 ; JMatrix%INSTC20(1)=0
  JMatrix%MEANC0(2)=1E30  ;  JMatrix%MEANC0(3)=-1E30 ; JMatrix%MEANC0(1)=0
  JMatrix%MONGEA0(2)=1E30 ;  JMatrix%MONGEA0(3)=-1E30 ; JMatrix%MONGEA0(1)=0
  JMatrix%R0=0 ; JMatrix%THT0=0
! Generate the rings

write(*,*) 'JMatrix'
  do i=1,M1                             ! every 2 degrees
   JMatrix%THT(i)=PI*(i-1)/90.0_wp
   if (MM == 360 .and. N == 16) then  ! original EyeSys RadSlope or fake data
    JMatrix%MV(i)=MIN(RadSlope%MV(2*i),RadSlope%MV(2*i-1))  ! close to real boundary
   else  ! MM==180
    JMatrix%MV(i)=min(RadSlope%MV(i),N1)  ! if N=25
   endif
   do j=1,JMatrix%MV(i)                             ! does not include center point
    if (i > (M1/2) ) then
     JMatrix%R(j,i)=N1*100*((1-j)*(rBo-rBi)/(N1-1)-rBi)/(1.*N)  !scaled to compensate for 16 vs 22 or 25 rings
    else
     JMatrix%R(j,i)=N1*100*((j-1)*(rBo-rBi)/(N1-1)+rBi)/(1.*N)
    endif
! populate JMatrix rings, not the centers
! elevations
    if (TestData.ne.2 .and. TestData.ne.4) then  ! slope based data, integrate based on iflag with or without cubic/trapez or center point or not for values
     call SplineEval1Dx1D(iflag,JMatrix%R(j,i),JMatrix%THT(i),JMatrix%Z(j,i),YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA)
    else  !TestData.eq.2 .or. TestData.eq.4  ! ELE and ELE.CSV files use elevation, no integration, center point or not
     if (btest(dat,0)) then
      call SplineEval1Dx1D(10,JMatrix%R(j,i),JMatrix%THT(i),JMatrix%Z(j,i),YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA)
     else
      call SplineEval1Dx1D(0,JMatrix%R(j,i),JMatrix%THT(i),JMatrix%Z(j,i),YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA)
     endif
    endif
!  save for vertex normals
    JMatrix%YPR(j,i)=YPR
    JMatrix%YPTHETA(j,i)=YPTHETA
!  powers
    call AXIALP(JMatrix%R(j,i),YPR,YP2R2,JMatrix%SAGC(j,i))
    call INSTANTP(JMatrix%R(j,i),YPR,YPTHETA,YP2R2,JMatrix%INSTC(j,i),JMatrix%INSTC2(j,i))
    call MEANP(JMatrix%THT(i),JMatrix%R(j,i),YPR,YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MEANC(j,i))
    call MONGEA(JMatrix%THT(i),JMatrix%R(j,i),YPR,YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MONGEA(j,i))
!   find min and max
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
  end do !end JMatrix ring generation

!  Calculate center values for everything
!  These have MM different values of the center!
!  Reset these has no more need for EyeSys/ATLAS/PentaCam RadSlope
   RadSlope=0
   DiaSlope=0
   deallocate(RadSplineCenter)
!  reinitialize with M1 and N1
!   MM=M1
!   N=N1
   call init_mat(M1,N1,RadSlope,DiaSlope,RadSplineCenter)
   RadSlope=JMatrix
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope

 if ( Testdata .eq. 1 ) then
   call MakeRadSplineCenter(0)     ! remakes RadSplineCenter(1,:)
   if (btest(dat, 0) ) then        ! use nsplineCenter to force zero slope at origin,
    call DiaSplineCenter(DiaSlope) ! re-spline, with center node
                                   ! changes spline but requires SplineEvalCenter
                                   ! remakes RadSplineCenter(2,:) and RadSplineCenter(3,:)
   endif
   call MakeRadSplineCenter(dat)        ! this relies on JMatrix, not the original data in RadSlope from the file
!  Should I do this again? and for each one?
   if (btest(dat, 1)) then         ! moving each meridian to align curves
    call AdjustRadSplineCenter     ! changes r only
    DiaSlope%Zpd2 = .n. DiaSlope   ! re-spline, standard
   endif
 endif


   write(*,*) 'Z'
!  Z
!   if (TestData.ne.2 .and. TestData.ne.4) then   ! already has valid Z0 from cornea_arrays & ELE file NOT YET IT DOES NOT
    do i=1,M1
     call SplineEval1Dx1D(iflag,JMatrix%R0,JMatrix%THT(i),JMatrix%Z(N1+1,i))  !center value of elevation; needs integration from slopes
     JMatrix%Z0(1)=(i*JMatrix%Z0(1)+JMatrix%Z(N1+1,i))/(i+1)      ! cumulative average
    end do
    if (JMatrix%Z0(1) <= JMatrix%Z0(2)) JMatrix%Z0(2)=JMatrix%Z0(1)
    if (JMatrix%Z0(1) >= JMatrix%Z0(3)) JMatrix%Z0(3)=JMatrix%Z0(1)
!   endif  ! TestData.eq.2 .or. TestData.eq.4


   write(*,*) 'SAGC'
!  SAGC
!   if (TestData.ne.3 .and. TestData.ne.5) then  ! already has valid SAGC0 from cornea_arrays & CUR file NOT YET IT DOES NOT
!  Reload RadSlope with SAGC & re-spline; can't compute it from surface because ill-defined at origin
    do i=1,M1
     do j=1,RadSlope%MV(i)
      RadSlope%Zp(j,i)=JMatrix%SAGC(j,i)
     end do
    end do
    DiaSlope=RadSlope              ! move to diagonal format
    DiaSlope%Zpd2 = .n. DiaSlope

    if ( Testdata .eq. 1 ) then
     if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
       call DiaSplineCenter(DiaSlope) ! re-spline, with center node
     endif
     if (btest(dat, 1)) then          ! moving each meridian to align curves
      call AdjustRadSplineCenter     ! changes r only
      DiaSlope%Zpd2 = .n. DiaSlope   ! re-spline, standard
     endif
    endif

    do i=1,M1
     if (btest(dat,0)) then
      call SplineEval1Dx1D(10,JMatrix%R0,JMatrix%THT(i),JMatrix%SAGC(N1+1,i))  ! center value
     else
      call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT(i),JMatrix%SAGC(N1+1,i))  ! center value
     endif
     JMatrix%SAGC0(1)=(i*JMatrix%SAGC0(1)+JMatrix%SAGC(N1+1,i))/(i+1)      ! cumulative average
    end do
    if (JMatrix%SAGC0(1) <= JMatrix%SAGC0(2)) JMatrix%SAGC0(2)=JMatrix%SAGC0(1)
    if (JMatrix%SAGC0(1) >= JMatrix%SAGC0(3)) JMatrix%SAGC0(3)=JMatrix%SAGC0(1)
!   endif   ! TestData.eq.3 .or. TestData.eq.5

   write(*,*) 'INSTC'
!  INSTC
!  Reload RadSlope & respline
   do i=1,M1
    do j=1,RadSlope%MV(i)
     RadSlope%Zp(j,i)=JMatrix%INSTC(j,i)
    end do
   end do
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope
   if ( Testdata .eq. 1 ) then
    if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
      call DiaSplineCenter(DiaSlope) ! re-spline, with center node
    endif
    if (btest(dat, 1)) then          ! moving each meridian to align curves
     call AdjustRadSplineCenter     ! changes r only
     DiaSlope%Zpd2 = .n. DiaSlope   ! re-spline, standard
    endif
   endif
   do i=1,M1
    if (btest(dat,0)) then
     call SplineEval1Dx1D(10,JMatrix%R0,JMatrix%THT(i),JMatrix%INSTC(N1+1,i))  ! center value
    else
     call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT(i),JMatrix%INSTC(N1+1,i))  ! center value
   endif
   JMatrix%INSTC0(1)=(i*JMatrix%INSTC0(1)+JMatrix%INSTC(N1+1,i))/(i+1)      ! cumulative average
  end do
  if (JMatrix%INSTC0(1) <= JMatrix%INSTC0(2)) JMatrix%INSTC0(2)=JMatrix%INSTC0(1)
  if (JMatrix%INSTC0(1) >= JMatrix%INSTC0(3)) JMatrix%INSTC0(3)=JMatrix%INSTC0(1)

  write(*,*) 'INSTC2'
! INSTC2
! Reload RadSlope & respline
  do i=1,M1
   do j=1,RadSlope%MV(i)
    RadSlope%Zp(j,i)=JMatrix%INSTC2(j,i)
   end do
  end do
  DiaSlope=RadSlope              ! move to diagonal format
  DiaSlope%Zpd2 = .n. DiaSlope
  if ( Testdata .eq. 1 ) then
   if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
     call DiaSplineCenter(DiaSlope) ! re-spline, with center node
   endif
   if (btest(dat, 1)) then          ! moving each meridian to align curves
    call AdjustRadSplineCenter     ! changes r only
    DiaSlope%Zpd2 = .n. DiaSlope   ! re-spline, standard
   endif
  endif
  do i=1,M1
   if (btest(dat,0)) then
    call SplineEval1Dx1D(10,JMatrix%R0,JMatrix%THT(i),JMatrix%INSTC2(N1+1,i))  ! center value
   else
    call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT(i),JMatrix%INSTC2(N1+1,i))  ! center value
   endif
   JMatrix%INSTC20(1)=(i*JMatrix%INSTC20(1)+JMatrix%INSTC2(N1+1,i))/(i+1)      ! cumulative average
  end do
  if (JMatrix%INSTC20(1) <= JMatrix%INSTC20(2)) JMatrix%INSTC20(2)=JMatrix%INSTC20(1)
  if (JMatrix%INSTC20(1) >= JMatrix%INSTC20(3)) JMatrix%INSTC20(3)=JMatrix%INSTC20(1)

  write(*,*) 'MEANC'
! MEANC
! Reload RadSlope & respline
  do i=1,M1
   do j=1,RadSlope%MV(i)
    RadSlope%Zp(j,i)=JMatrix%MEANC(j,i)
   end do
  end do
  DiaSlope=RadSlope              ! move to diagonal format
  DiaSlope%Zpd2 = .n. DiaSlope
  if ( Testdata .eq. 1 ) then
   if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
     call DiaSplineCenter(DiaSlope) ! re-spline, with center node
   endif
   if (btest(dat, 1)) then          ! moving each meridian to align curves
    call AdjustRadSplineCenter     ! changes r only
    DiaSlope%Zpd2 = .n. DiaSlope   ! re-spline, standard
   endif
  endif
  do i=1,M1
   if (btest(dat,0)) then
    call SplineEval1Dx1D(10,JMatrix%R0,JMatrix%THT(i),JMatrix%MEANC(N1+1,i))  ! center value
   else
    call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT(i),JMatrix%MEANC(N1+1,i))  ! center value
   endif
   JMatrix%MEANC0(1)=(i*JMatrix%MEANC0(1)+JMatrix%MEANC(N1+1,i))/(i+1)      ! cumulative average
  end do
  if (JMatrix%MEANC0(1) <= JMatrix%MEANC0(2)) JMatrix%MEANC0(2)=JMatrix%MEANC0(1)
  if (JMatrix%MEANC0(1) >= JMatrix%MEANC0(3)) JMatrix%MEANC0(3)=JMatrix%MEANC0(1)

  write(*,*) 'MONGEA'
! MONGEA
! Reload RadSlope & respline
  do i=1,M1
   do j=1,RadSlope%MV(i)
    RadSlope%Zp(j,i)=JMatrix%MONGEA(j,i)
   end do
  end do
  DiaSlope=RadSlope              ! move to diagonal format
  DiaSlope%Zpd2 = .n. DiaSlope
  if ( Testdata .eq. 1 ) then
   if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
     call DiaSplineCenter(DiaSlope) ! re-spline, with center node
   endif
   if (btest(dat, 1)) then          ! moving each meridian to align curves
    call AdjustRadSplineCenter     ! changes r only
    DiaSlope%Zpd2 = .n. DiaSlope   ! re-spline, standard
   endif
  endif
  do i=1,M1
   if (btest(dat,0)) then
    call SplineEval1Dx1D(10,JMatrix%R0,JMatrix%THT(i),JMatrix%MONGEA(N1+1,i))  ! center value
   else
    call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT(i),JMatrix%MONGEA(N1+1,i))  ! center value
  endif
  JMatrix%MONGEA0(1)=(i*JMatrix%MONGEA0(1)+JMatrix%MONGEA(N1+1,i))/(i+1)      ! cumulative average
 end do
 if (JMatrix%MONGEA0(1) <= JMatrix%MONGEA0(2)) JMatrix%MONGEA0(2)=JMatrix%MONGEA0(1)
 if (JMatrix%MONGEA0(1) >= JMatrix%MONGEA0(3)) JMatrix%MONGEA0(3)=JMatrix%MONGEA0(1)
! end populating JMatrix

!zernike coefficents
if (mod(flag,100) == 1) then
call Ccounter(0)
call LogC("Starting zernike computation"//c_null_char)
! relies on saved MM,N
nrhs=(M1*N1+1)

  if (allocated(JMatrix%R)) then
! Try to generate zernike coefficients based on central elevations & lsq to zernike polynomials
!  call CPU_TIME(time_start)
  time_start=omp_get_wtime()
!  Reload RadSlope & respline
   do i=1,M1
    do j=1,RadSlope%MV(i)
     RadSlope%Zp(j,i)=JMatrix%Z(j,i)
    end do
   end do
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope
   if ( Testdata .eq. 1 ) then
    if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
      call DiaSplineCenter(DiaSlope) ! re-spline, with center node
    endif
    if (btest(dat, 1)) then          ! moving each meridian to align curves
     call AdjustRadSplineCenter     ! changes r only
     DiaSlope%Zpd2 = .n. DiaSlope   ! re-spline, standard
    endif
   endif

! allocate working matrices
   kk_max=5*12
   k=0
   do m=-4,4
    do nn=ABS(m),4
     if (mod(nn-m,2) == 0) then
      k=k+1
!      write(*,*) 'zernike coefficent k,n,m: ',k,nn,m  !maps kth computed zernike coefficient to index k
     endif
    end do
   end do
   k_max=k   
   allocate (B_Matrix(k_max,kk_max),zernC(kk_max,nrhs),rlocal(kk_max),thtlocal(kk_max),stat=ierr) ! zernC(kk_max) to hold data though only k_max zernike coeficients
   if (ierr /= 0) then
!    write(*,*) 'unable to allocate memory in zernike: ', ierr,k_max,kk_max,nrhs
    return
   endif
   zernC=0

   do ii=1,nrhs
   call Ccounter(ii/40)
!  cycle through i1 1 to MM and j1 1 to N with one point for origin at N+1
   i1=mod(ii,M1)
   j1=int(ii/M1)+1
   if (i1 .eq. 0) then
    i1=M1
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
    else
     Theta_Talus=0
    endif
    call SplineEval1Dx1D(iflag,R_Talus,Theta_Talus,zernC(kk,ii))  ! elevation for zernike; use coefficient vector as temporary storage
    end do
   end do
   end do  ! end ii to nrhs

   RadSlope=JMatrix                      ! restore RadSlope

! generate the Zpolynomial degree_polynomial values for each point, makes a matrix degree_polynomials x length_data
! if n >= 0 ABS(m) <= n  & mod(n-m,2) = 0
  do kk=1,kk_max   
   k=0
   do m=-4,4
    do nn=ABS(m),4
     if (mod(nn-m,2) == 0) then
      k=k+1
      B_Matrix(k,kk)=zernfct(nn,m,rlocal(kk),thtlocal(kk))  ! local cylindrical coordinates
     else
      cycle
     endif
    end do
   end do
  end do

call LogC("pre-LSQ"//c_null_char)

! solve the LSQ equations for zernC(k): solution is degree_polynomials number of coefficients;  B_Matrix(k,kk)*zernC(k)=z(kk)
! Use normal equation XTX.c=X.z ie. B_Matrix(k,kk)*zernC(k)=z(kk) or use LAPACKs dgels()
! only have to call this once; NRHS can be for the whole talus plot since B_Matrix is invariant.
! have to allocate XTX,EE,IPIV for DGESV, XTX,EE for G-J
   allocate(XTX(k_max,k_max),EE(k_max,nrhs),IPIV(k_max),stat=ierr)
   if (ierr /= 0) then
    write(*,*) 'unable to allocate memory in zernike for GJ '
    return
   endif
   XTX=matmul(B_matrix,Transpose(B_matrix))
   EE=matmul(B_matrix,zernC)  ! with a second dimension for EE
!   call DGESV(k_max,nrhs,XTX,k_max,IPIV,EE,k_max,INFO) ! overwrites EE into solution
   call GaussJordan(k_max,nrhs,XTX,k_max,EE,k_max,INFO )  ! overwrites EE into solution
!!  have to allocate WORK for DGELS, to use these uncomment them in declarations too
!  LWORK = min(k_max,kk_max) + max( min(k_max,kk_max), nrhs )
!  allocate (WORK(LWORK))! WORK is dimension LWORK
!  call DGELS( 'T', k_max, kk_max, nrhs, B_Matrix, k_max, zernC , kk_max, WORK, LWORK, INFO ) ! overwrites zernC (only to k_max)
!! if using DGELS have to replace EEwith zernC below ie EE(1:k_max,kk) => zernC(1:k_max,kk)

call LogC("post-LSQ"//c_null_char)

!$OMP PARALLEL DO PRIVATE(i1,j1,i,j,kk)
do kk=1,nrhs
!  cycle through i1 1 to MM and j1 1 to N with one point for origin at N+1
i1=mod(kk,M1)
j1=int(kk/M1)+1
if (i1 .eq. 0) then
 i1=M1
 j1=j1-1
endif
  JMatrix%ZC(j1,i1,1:k_max)=EE(1:k_max,kk)
end do
!$OMP END PARALLEL DO

! center values
do k=1,15
 JMatrix%ZC0(1,:)=EE(1:k_max,nrhs)
end do
! find min and max
JMatrix%ZC0(2,:)=1E30
JMatrix%ZC0(3,:)=-1E30
do i =1,M1
 do j = 1,RadSlope%MV(i)
  do k = 1,15
   if (JMatrix%ZC(j,i,k) <= JMatrix%ZC0(2,k)) JMatrix%ZC0(2,k)=JMatrix%ZC(j,i,k)
   if (JMatrix%ZC(j,i,k) >= JMatrix%ZC0(3,k)) JMatrix%ZC0(3,k)=JMatrix%ZC(j,i,k)
   if (JMatrix%ZC0(1,k) <= JMatrix%ZC0(2,k)) JMatrix%ZC0(2,k)=JMatrix%ZC0(1,k)
   if (JMatrix%ZC0(1,k) >= JMatrix%ZC0(3,k)) JMatrix%ZC0(3,k)=JMatrix%ZC0(1,k)
  end do
 end do
end do

write(*,*) 'center zernike values: ',EE(1:k_max,nrhs)
call LogC("Finished zernike"//c_null_char)
call Ccounter(100)
! Done with zernike

  deallocate(XTX,EE,IPIV)  !if used above
!  deallocate(WORK,B_Matrix)
  deallocate(zernC,rlocal,thtlocal)

!  call CPU_TIME(time_end)
  time_end=omp_get_wtime()
  write(*,*) 'Time to compute zernike: ',(time_end-time_start)
  return
 else
  call LogC("Have to open a file prior to computing zernike"//c_null_char)
  return ! if last digits of flag==1 and not allocated do nothing
 endif
endif  ! end of flag=1


! flag 4 and 0, as 1,2,3 have return statements
!  use fillarray to fill DiaSlope Zp with calculated value based on IuseG, optionally generate LIOC
!  using SplineEval1Dx1D to refill a new matrix RadSlope using f0, derivatives to get calculated powers







! simple difference/subtraction the second time through
if (allocated(JMatrix1%R)) then
! skip this for now
 if (.false.) then
  JMatrix%SAGC(:,:)=ABS(JMatrix1%SAGC(:,:)-JMatrix%SAGC(:,:))
  JMatrix%INSTC(:,:)=ABS(JMatrix1%INSTC(:,:)-JMatrix%INSTC(:,:))
  JMatrix%INSTC2(:,:)=ABS(JMatrix1%INSTC2(:,:)-JMatrix%INSTC2(:,:))
  JMatrix%MEANC(:,:)=ABS(JMatrix1%MEANC(:,:)-JMatrix%MEANC(:,:))
  JMatrix%MONGEA(:,:)=ABS(JMatrix1%MONGEA(:,:)-JMatrix%MONGEA(:,:))
  JMatrix%Z0(:)=ABS(JMatrix1%Z0(:)-JMatrix%Z0(:))
  JMatrix%SAGC0(:)=ABS(JMatrix1%SAGC0(:)-JMatrix%SAGC0(:))
  JMatrix%INSTC0(:)=ABS(JMatrix1%INSTC0(:)-JMatrix%INSTC0(:))
  JMatrix%INSTC20(:)=ABS(JMatrix1%INSTC20(:)-JMatrix%INSTC20(:))
  JMatrix%MEANC0(:)=ABS(JMatrix1%MEANC0(:)-JMatrix%MEANC0(:))
  JMatrix%MONGEA0(:)=ABS(JMatrix1%MONGEA0(:)-JMatrix%MONGEA0(:))
 endif
endif


! need RadSlope for WriteCenter/LIOC
RadSlope=JMatrix
DiaSlope=RadSlope              ! move to diagonal format
DiaSlope%Zpd2 = .n. DiaSlope


if ( Testdata .eq. 1 ) then
 call MakeRadSplineCenter(0)     ! remakes RadSplineCenter(1,:)
 if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
   call DiaSplineCenter(DiaSlope) ! re-spline, with center node
 endif
 if (btest(dat, 1)) then          ! moving each meridian to align curves
  call AdjustRadSplineCenter     ! changes r only
  DiaSlope%Zpd2 = .n. DiaSlope   ! re-spline, standard
 endif
endif






! WriteCenter
! WriteCenter shows where the spline of slopes is zero, it should be close to zero for a concave center with a unique maximum
  call WriteCenter(RadSlope,'Center.dat')! biggest deviation with nSplineCenter zero slope forced at origin,
                                            ! then with zero slope forced at average (r(low)+r(high))/2.0
                                            ! smallest deviation without nSplineCenter; view with set polar; plot 'Center.dat' with lines

!   call execute_command_line ("gnuplot -p plotcenter.gnu &", exitstat=i)
!plots spread of values at origin for each meridian from average
 call WriteCenterJ(JMatrix%SAGC0(1),JMatrix%SAGC,'CenterSAGC.dat')
 call WriteCenterJ(JMatrix%INSTC0(1),JMatrix%INSTC,'CenterINSTC.dat')
 call WriteCenterJ(JMatrix%MEANC0(1),JMatrix%MEANC,'CenterMEANC.dat')
 call WriteCenterJ(JMatrix%MONGEA0(1),JMatrix%MONGEA,'CenterMONGEA.dat')
 call WriteCenterJ(JMatrix%Z0(1),JMatrix%Z,'CenterZ.dat')

!  Generate LIOC with vector format
   LinesOfCurv='LIOC.CAR'
   call FILLARRAY(8,LinesOfCurv)    ! don't redo bounds consider optional !  plot 'LIOC.CAR' using 1:2:3:4 with vectors
!   call execute_command_line ("gnuplot -p plotlioc.gnu &", exitstat=i)


! writes values in openGL friendly format to matrices for passing to C/C++
! flag/fct determines what to write for elevation and color, just like in flag=2,3 output versions above
   donut = .FALSE.
   if (fct .lt. 16 .and. fct .gt. 0) then
     powctr=JMatrix%ZC0(1,fct)
     powmin=JMatrix%ZC0(2,fct)
     powmax=JMatrix%ZC0(3,fct)
   else
   SELECT CASE (fct)
     CASE (0)
     powctr=JMatrix%SAGC0(1)
     powmin=JMatrix%SAGC0(2)
     powmax=JMatrix%SAGC0(3)
     CASE (16)
     powctr=JMatrix%INSTC0(1)
     powmin=JMatrix%INSTC0(2)
     powmax=JMatrix%INSTC0(3)
     CASE (17)
     powctr=JMatrix%INSTC20(1)
     powmin=JMatrix%INSTC20(2)
     powmax=JMatrix%INSTC20(3)
     CASE (18)
     powctr=JMatrix%MEANC0(1)
     powmin=JMatrix%MEANC0(2)
     powmax=JMatrix%MEANC0(3)
     CASE (19)
     powctr=JMatrix%MONGEA0(1)
     powmin=JMatrix%MONGEA0(2)
     powmax=JMatrix%MONGEA0(3)
     CASE (20)
     powctr=JMatrix%Z0(1)
     powmin=JMatrix%Z0(2)
     powmax=JMatrix%Z0(3)
     CASE DEFAULT
     powctr=JMatrix%SAGC0(1)
     powmin=JMatrix%SAGC0(2)
     powmax=JMatrix%SAGC0(3)     
   END SELECT
  endif
!   write(*,*) 'powctr,POWMIN,POWMAX',powctr,POWMIN,POWMAX

  call Geom(flag, JMatrix, donut, powmin, powmax, elements, vertices, nV, nE)

! eigenvalues show shape of RadSlope without make_rings but with FillArray 7 elevations
!  atmp=pca(2,RadSlope) 
!  atmp=pca(3,RadSlope)


!gnuplot output

   call CPU_TIME(time_start)
   RadSlope=JMatrix
   DiaSlope=RadSlope            
   DiaSlope%Zpd2 = .n. DiaSlope
   if ( Testdata .eq. 1 ) then
    if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
      call DiaSplineCenter(DiaSlope) ! re-spline, with center node
    endif
    if (btest(dat, 1)) then          ! moving each meridian to align curves
     call AdjustRadSplineCenter     ! changes r only
     DiaSlope%Zpd2 = .n. DiaSlope   ! re-spline, standard
    endif
   endif
   call FILLARRAY(4,LinesOfCurv)
   write(*,*) 'POWMIN,POWMAX',POWMIN,POWMAX
   call CPU_TIME(time_end)
   write(*,*) 'Time to make plot',i,(time_end-time_start)*1000   

! GENERATE PRINT FILES
!  RadSlope=DiaSlope
  do i=1,M1
   do j=1,RadSlope%MV(i)
    RadSlope%Zp(j,i)=JMatrix%SAGC(j,i)
   end do
  end do
  call WRITEARRAY(RadSlope,BigPlot)  !plots RadSlope%Zp(j,i)

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
  call LogC("Done: janus"//c_null_char)  !has to be C and declared, not cpp

  return        

  END subroutine janus
