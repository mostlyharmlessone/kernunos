  subroutine Janus(flag,file_from_C,elements,vertices,legend,cardinal,zern,nV,nE,nL,nC,pupil_elements,pupil_vertices,pupil_nV,pupil_nE,err_janus) bind(C,name='janus_')
! back end for calculations
  use set_precision, ONLY : wp, sk
  use lapackinterface
  use cornea_arrays
  use special_fct
  use io_functions
  use spline_interfaces
  use,intrinsic :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char,c_int64_t,c_double
  use, intrinsic :: iso_fortran_env
  use,intrinsic :: ieee_arithmetic
  use c_interfaces, ONLY : LogC, Ccounter, ConvertPLYtoBIN, charcount
  use omp_lib
  IMPLICIT NONE
  integer :: i, j, k, ii, kk, m, nn, i1, j1, ierr, info, nrhs
  integer,save :: MM, N ,M1, N1, Power_Rings_Count, loaded_files, crop
  integer,save :: TestData             ! TestData: -1=test, 0=EyeSys, 1=Atlas, (2-5)=Penta, 6=Nidek, 7=Keratograph
  integer,save :: NP                   ! PentaCam=141
  integer :: unitno1
  character(c_char), INTENT(IN), DIMENSION(4096) :: file_from_C
  integer(c_int64_t), INTENT(INOUT) :: flag
  integer(c_int), INTENT(INOUT) :: nV 
  integer(c_int), INTENT(INOUT) :: nE               
  real(c_float), INTENT(INOUT) :: vertices(*)
  integer(c_int), INTENT(INOUT) :: elements(*)
  integer(c_int), INTENT(INOUT) :: pupil_nV
  integer(c_int), INTENT(INOUT) :: pupil_nE
  integer(c_int), INTENT(INOUT) :: err_janus
  real(c_float), INTENT(INOUT) :: pupil_vertices(*)
  integer(c_int), INTENT(INOUT) :: pupil_elements(*)
  integer(c_int), INTENT(INOUT) :: nL,nC
  real(c_float), INTENT(INOUT) :: legend(*)
  real(c_double), INTENT(INOUT) :: cardinal(*)
  real(c_float), INTENT(INOUT) :: zern(*)
  real(c_float) :: dist
  character(len=4096) :: new_path
  character(:),save, ALLOCATABLE :: inputfile1,inputfile2,inputfile3,inputfile4,inputfile5,inputfile6,inputfile7
  character(:),save, ALLOCATABLE :: logfile,BigPlot,gnu_instruct,cab_inputfile1,cab_inputfile2,cab_inputfile3,cab_inputfile4
  integer ::  nblines, file_idx, read_error, io, new_crop
  integer,allocatable :: MV(:)
  real(8) :: time_start, time_end
  real(wp) :: POWMIN,POWMAX,POWMAX2,POWCTR,POW,P1,X1,X2,U,V
  logical :: donut, exists
  real(wp) :: Y,YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA,rBi,rBo,dvert,dhoriz,percent_squash
  integer :: k_max, kk_max, iflag, LWORK, rotationdegrees
  integer(c_int64_t) :: dat, fct, map
  integer(c_int) ::  error_report
  real(wp), allocatable :: zernC(:,:), B_Matrix(:,:), rlocal(:), thtlocal(:), WORK(:)
  real(wp), allocatable :: UT(:,:),VT(:,:) !,XTX(:,:),EE(:,:)
!  integer, allocatable :: IPIV(:)
  real(wp) :: ctr_circle_x, ctr_circle_y, R_global, Theta_global, R_MV, R_TST !, P_TEMP
  real(wp) :: gaussian,meanpower,princ1,princ2,astigm
  real(wp), allocatable :: temp(:,:)
  logical :: lsq
  integer(c_int) :: periodcount
  integer(c_int64_t), parameter :: zero_int64 = 0

err_janus = 0 ; error_report = 0 ;
if (loaded_files .le. 0) loaded_files = 0
!write(*,*) 'flag to Fortran:',flag
!write(*,*) 'flag(action) last digits to Fortran:',mod(flag,100)
!! last two digits are the program function
!! 99 = deallocate arrays for program closure
!! 11 = swap
!! 10 = compare
!! 9 = show zernike coefficients
!! 8 = show circumferential ring lsqfillin/splinefillin
!! 7 = make lioc
!! 6 = make centers
!! 5 = make gnuplotsplot
!! 4 = redraw without reloading new file (also used with decenter)
!! 3 = write ASCII PLY file
!! 2 = write OFF file
!! 1 = compute zernike coefficients/maps
!! 0 = open a file, display
dat=(flag-mod(flag,1000000))/1000000 ! first two digits are tweaks
!write(*,*) 'dat to Fortran:',dat,btest(dat,0),btest(dat,1),btest(dat,2),btest(dat,3),btest(dat,4),btest(dat,5),btest(dat,6),btest(dat,7),btest(dat,8),btest(dat,9),btest(dat,10),btest(dat,11)
fct=mod(((flag-mod(flag,10000))/10000),100) ! second two digits, fct to be plotted
!write(*,*) 'fct to Fortran:',fct
map=mod((flag-mod(flag,100))/100,100)  ! last two digits color map functions
!write(*,*) 'color(map) to Fortran:',map
! dat = first binary bit 0/1 centernode tweak ie btest(dat,0) = .true.
! dat = second binary bit 0/1 shift r-values tweak ie btest(dat,1) = .true.
! integration of slopes for elevation:
! dat = third binary bit 0/1 cubic spline integration (=1)(ie btest(dat,2) = .true.) vs trapezoidal rule (default = 0)
! dat = fourth binary bit 0/1 fillin2 cannot be combined with splinefillin ie btest(dat,3) = .true.
! dat = fifth binary bit 0/1 splinefillin cannot be combined with lsqfillin ie btest(dat,4) = .true.
! dat = sixth binary bit 0/1 decenter tweak ie btest(dat,5) = .true.
! dat = seventh binary bit 0/1 pupilregister tweak ie btest(dat,6) = .true.
! dat = eighth binary bit 0/1 atlas spline consistency check tweak ie btest(dat,7) = .true.
! dat = ninth binary bit 0/1 lsq instead of circumferential spline tweak ie btest(dat,8) = .true.
! dat = tenth binary bit 0/1 lsqspline tweak ie btest(dat,9) = .true.
! dat = eleventh binary bit 0/1 axisymmetric tweak ie btest(dat,10) = .true.
! dat = twelfth binary bit 0/1 elevation is switched for power on 3-D display ie btest(dat,11) = .true.
! dat = thirteenth binary bit 0/1 crop the data ie btest(dat,12) = .true.

! iflag passing of dat to SplineEval1Dx1D centernode splines, integration of splines and LSQ vs circumferential splining
! first mod((iflag-mod(iflag,100))/100,100)
! second digit mod((iflag-mod(iflag,10))/10,10)
! third digit mod(iflag,10)
!btest(dat, 2)     T   F
!
!              T  X12  X11
!btest(dat,0)
!              F  X02  X01
! iflag =0 ! no integration, no centernode, circumferential spline, so then 10 or 0 or 100 or 110
! iflag =10 ! no integration, centernode, circumferential spline, so then 10 or 0 or 100 or 110
! iflag =110 ! no integration, centernode, LSQ, so then 10 or 0 or 100 or 110
! iflag =100 ! no integration, no centernode, LSQ, so then 10 or 0 or 100 or 110
! iflag =110 ! no integration, centernode, LSQ, so then 10 or 0 or 100 or 110
! iflag =111 ! integration, centernode, LSQ, so then 10 or 0 or 100 or 110

! mod(flag,100) == 99 Deallocate
if (mod(flag,100) == 99) then
    loaded_files = 0
    if (allocated(JMatrix%R)) then
     JMatrix=0
    endif
    if (allocated(JMatrix1%R)) then
     JMatrix1=0
    endif
    if (allocated(JMatrix2%R)) then
     JMatrix2=0
    endif
    if (allocated(DiaSlope%rd)) then
     DiaSlope=0
    endif
    if (allocated(RadSlope%r)) then
     RadSlope=0
    endif
    if (allocated(inputfile1)) then
     deallocate(inputfile1)
    endif
    if (allocated(inputfile2)) then
     deallocate(inputfile2)
    endif
    if (allocated(inputfile3)) then
     deallocate(inputfile3)
    endif
    if (allocated(inputfile4)) then
     deallocate(inputfile4)
    endif
    if (allocated(inputfile5)) then
     deallocate(inputfile5)
    endif
    if (allocated(inputfile6)) then
     deallocate(inputfile6)
    endif
    if (allocated(inputfile7)) then
     deallocate(inputfile7)
    endif
    if (allocated(logfile)) then
     deallocate(logfile)
    endif
    if (allocated(cab_inputfile1)) then
     deallocate(cab_inputfile1)
    endif
    if (allocated(cab_inputfile2)) then
     deallocate(cab_inputfile2)
    endif
    if (allocated(cab_inputfile3)) then
     deallocate(cab_inputfile3)
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
    if (allocated(Oculus%Y)) then
     Oculus=0
    endif
    if (allocated(Penta%DAT)) then
     Penta=0
    endif
    if (allocated(Skyline%DAT)) then
     Skyline=0
    endif
    return
endif

! allocate JMatrix needed for file import
!  JMatrix was picked to be 180x22 to make importing from Atlas easier.
!  M1,N1 avoid overwriting MM,N at this point
   M1=180
   N1=22
if (.not.allocated(JMatrix%R)) then
  call init_mat_JMatrix(M1,N1,JMatrix)
endif
if (.not.allocated(JMatrix1%R)) then
 call init_mat_JMatrix(M1,N1,JMatrix1)
endif
if (.not.allocated(JMatrix2%R)) then
 call init_mat_JMatrix(M1,N1,JMatrix2)
endif
if (.not.allocated(JMatrix3%R)) then
 call init_mat_JMatrix(M1,N1,JMatrix3)
endif

if (mod(flag,100) == 0 ) then  !store last JMatrix when reading in new
 JMatrix1%R(:,:)=JMatrix%R(:,:)
 JMatrix1%PU(:)=JMatrix%PU(:)
 JMatrix1%Pupil_Center(:)=JMatrix%Pupil_Center(:)
 JMatrix1%Z(:,:)=JMatrix%Z(:,:)
 JMatrix1%YPR(:,:)=JMatrix%YPR(:,:)
 JMatrix1%YPTHETA(:,:)=JMatrix%YPTHETA(:,:)
 JMatrix1%THT(:)=JMatrix%THT(:)
 JMatrix1%SAGC(:,:)=JMatrix%SAGC(:,:)
 JMatrix1%Warp(:,:)=JMatrix%Warp(:,:)
 JMatrix1%INSTC(:,:)=JMatrix%INSTC(:,:)
 JMatrix1%GAUSSC(:,:)=JMatrix%GAUSSC(:,:)
 JMatrix1%MEANC(:,:)=JMatrix%MEANC(:,:)
 JMatrix1%MONGEA(:,:)=JMatrix%MONGEA(:,:)
 JMatrix1%RC(:,:)=JMatrix%RC(:,:)
 JMatrix1%MV(:)=JMatrix%MV(:)
 JMatrix1%R0=JMatrix%R0
 JMatrix1%Z0(:)=JMatrix%Z0(:)
 JMatrix1%THT0=JMatrix%THT0
 JMatrix1%SAGC0(:)=JMatrix%SAGC0(:)
 JMatrix1%Warp0(:)=JMatrix%Warp0(:)
 JMatrix1%INSTC0(:)=JMatrix%INSTC0(:)
 JMatrix1%GAUSSC0(:)=JMatrix%GAUSSC0(:)
 JMatrix1%MEANC0(:)=JMatrix%MEANC0(:)
 JMatrix1%MONGEA0(:)=JMatrix%MONGEA0(:)
 JMatrix1%ZC0(:,:)=JMatrix%ZC0(:,:)
 JMatrix1%ZC(:,:,:)=JMatrix%ZC(:,:,:)
endif

if (mod(flag,100) == 0 .or. mod(flag,100) == 2 .or. mod(flag,100) == 3) then
!  only need new file name if opening a file or printing, or compare for degree information and
!  local save of inputfile1,inputfile2,logfile
!  write(*,*) 'file from kernunos: ',file_from_C  ! this will have a lot of extra random non ASCII stuff after the file name
!! did this because GCC11 isn't F2018 compliant with deferred length character with Bind C
!! ie. can't do CHARACTER(*,c_char), INTENT(IN) :: file_from_C_1 with BIND(C) with GCC
!! Or is it that ISO_Fortran_binding.h isn't available, or C++ not C?
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
! write(*,*) 'file from kernunos: ',trim(new_path)
 nblines=len(trim(new_path))
 if (allocated(inputfile1)) then
  deallocate(inputfile1)
  deallocate(inputfile2)
  deallocate(inputfile3)
  deallocate(inputfile4)
  deallocate(inputfile5)
  deallocate(logfile)
 endif
 allocate(character(nblines) :: inputfile1)
 allocate(character(nblines) :: logfile)
 inputfile1=trim(new_path)
 allocate(character(nblines) :: inputfile2)
 allocate(character(nblines) :: inputfile3)
 allocate(character(nblines+3) :: inputfile4)
 allocate(character(nblines+3) :: inputfile5)
endif  ! mod(flag,100) == 0, 10, 2, or 3


if (mod(flag,100) .eq. 5 .or. mod(flag,100) .eq. 6 .or.&
    mod(flag,100) .eq. 7 .or. mod(flag,100) .eq. 8 ) then  !gnuplot files&calls
 new_path = " "
 do i=1, 4096
    if ( file_from_C (i) == c_null_char ) then
        exit
    else
        new_path (i:i) = file_from_C (i)
    end if
 end do
! write(*,*) 'file from kernunos: ',trim(new_path)
 nblines=len(trim(new_path))
 if (allocated(BigPlot)) then
  deallocate(BigPlot)
  deallocate(gnu_instruct)
 endif
  allocate(character(nblines) :: BigPlot)
  allocate(character(nblines) :: gnu_instruct)
  gnu_instruct=trim(new_path)
  BigPlot=replacestr(string=gnu_instruct,search=".gnu",substitute=".plt")
endif

if (mod(flag,100) .eq. 10  ) then  ! compare with btest(dat,6) = .true. or .false.
 new_path = " "
 do i=1, 4096
    if ( file_from_C (i) == c_null_char ) then
        exit
    else
        new_path (i:i) = file_from_C (i)
    end if
 end do
! write(*,*) 'file from kernunos: ',trim(new_path)
 new_path=trim(new_path)
 read(new_path,*) rotationdegrees
 write(*,*) "compare rotation, pupilregister: ",rotationdegrees,btest(dat,6)
endif

if (mod(flag,100) .eq. 5 ) then
! gnuplot splot output
! needs powmin & powmax
 if (allocated(JMatrix%R)) then
  donut = .FALSE.
  fct=mod(((flag-mod(flag,10000))/10000),100)
  call selectfunction(0,JMatrix,flag,powctr,powmin,powmax,cardinal,nC)
 endif
! generate data file
  unitno1 = get_new_fileunit()
  open(unitno1, file = BigPlot, action="write", iostat=ierr)
  do i=1,M1
   do j=1,JMatrix%MV(i)
    X1=JMatrix%tht(i)
    X2=JMatrix%r(j,i)
    SELECT CASE (fct)
      CASE (0)
         p1=JMatrix%SAGC(j,i)
      CASE (16)
          p1=JMatrix%INSTC(j,i)
      CASE (17)
         p1=JMatrix%GAUSSC(j,i)
      CASE (18)
         p1=JMatrix%MEANC(j,i)
      CASE (19)
         p1=JMatrix%MONGEA(j,i)
      CASE (20)
         p1=JMatrix%Z(j,i)
      CASE (21)
         p1=JMatrix%Warp(j,i)
      CASE DEFAULT
         p1=JMatrix%SAGC(j,i)
   END SELECT
    IF((ABS(P1).GT.0).AND.(ABS(X2).GT.0.01)) THEN
      WRITE(unitno1,*) ABS(X2)*COS(X1),ABS(X2)*SIN(X1),p1
    ENDIF
   end do
   WRITE(unitno1,*) ' '
  end do
! REPEAT FIRST ANGLE
  i=1
  do J=1,JMatrix%MV(i)
   X1=JMatrix%tht(i)
   X2=JMatrix%r(j,i)
   SELECT CASE (fct)
     CASE (0)
        p1=JMatrix%SAGC(j,i)
     CASE (16)
         p1=JMatrix%INSTC(j,i)
     CASE (17)
        p1=JMatrix%GAUSSC(j,i)
     CASE (18)
        p1=JMatrix%MEANC(j,i)
     CASE (19)
        p1=JMatrix%MONGEA(j,i)
     CASE (20)
        p1=JMatrix%Z(j,i)
     CASE (21)
        p1=JMatrix%Warp(j,i)
     CASE DEFAULT
        p1=JMatrix%SAGC(j,i)
  END SELECT
   IF((P1.GT.0).AND.(ABS(X2).GT.0.01)) THEN
    WRITE(unitno1,*) ABS(X2)*COS(X1),ABS(X2)*SIN(X1),p1
   ENDIF
  end do
  CLOSE (unitno1)
! instruction file
   unitno1 = get_new_fileunit()
   open(unitno1, file = gnu_instruct, action="write", iostat=ierr)
   WRITE(unitno1,*) 'reset'
   WRITE(unitno1,*) 'set size square'
   WRITE(unitno1,*) 'set macros'
   WRITE(unitno1,*) 'NOXTICS = "set format x ''''; unset xlabel"'
   WRITE(unitno1,*) 'NOYTICS = "set format y ''''; unset ylabel"'
   CALL PRINTGRAPH(unitno1,POWMIN,POWMAX,BigPlot)
   CLOSE (unitno1)
!  return to kernunos for gp command to use gnu_instruct (BigPlot is not needed in kernunos)
!  call execute_command_line ("gnuplot -p " gnu_instruct " &", exitstat=i)
   return
 endif ! end (mod(flag,100) .eq. 5)


if (mod(flag,100) .eq. 6) then
! WriteCenter
! need RadSlope for WriteCenter
RadSlope=JMatrix
DiaSlope=RadSlope              ! move to diagonal format
DiaSlope%Zpd2 = .n. DiaSlope
 if ( Testdata .le.1 .or. TestData .eq. 6 ) then     ! only for test/Atlas/EyeSys/NIDEK at present
  call MakeRadSplineCenter(zero_int64,error_report)   ! remakes RadSplineCenter(1,:)
  if (error_report .ne. 0) then
   write(*,*)' janus line number: ',__LINE__
  endif
 else
  RadSplineCenter(1,:)=0      ! pentacam by definition is at 0
 endif
 if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
  DiaSlope%Zpd2 = .nc. DiaSlope ! re-spline, with center node
 endif
 if (btest(dat, 1)) then          ! moving each meridian to align curves
  call AdjustRadSplineCenter     ! changes r only
  DiaSlope%Zpd2 = .n. DiaSlope    ! re-spline, standard
 endif
  call MakeRadSplineCenter(dat,error_report)        ! generates spline centers with tweaks
  if (error_report .ne. 0) then
   write(*,*)' janus line number: ',__LINE__
  endif
! WriteCenter shows where the spline of slopes is zero, it should be close to zero for a concave center with a unique maximum
 BigPlot=replacestr(string=gnu_instruct,search=".gnu",substitute=".plt")
 call WriteCenter(RadSlope,BigPlot)! biggest deviation with nSplineCenter zero slope forced at origin,
                                   ! then with zero slope forced at average (r(low)+r(high))/2.0
                                   ! smallest deviation without nSplineCenter; view with set polar; plot 'Center.dat' with lines
!plots spread of values at origin for each meridian from average
 BigPlot=replacestr(string=gnu_instruct,search=".gnu",substitute=".sag")
 call WriteCenterJ(JMatrix%SAGC0(1),JMatrix%SAGC,BigPlot)
 BigPlot=replacestr(string=gnu_instruct,search=".gnu",substitute=".int")
 call WriteCenterJ(JMatrix%INSTC0(1),JMatrix%INSTC,BigPlot)
 BigPlot=replacestr(string=gnu_instruct,search=".gnu",substitute=".mea")
 call WriteCenterJ(JMatrix%MEANC0(1),JMatrix%MEANC,BigPlot)
 BigPlot=replacestr(string=gnu_instruct,search=".gnu",substitute=".mon")
 call WriteCenterJ(JMatrix%MONGEA0(1),JMatrix%MONGEA,BigPlot)
return
endif !  (mod(flag,100) .eq. 6)

if (mod(flag,100) .eq. 7) then
!  Generate LIOC with vector format
!  'plot ' gnu_instruct ' using 1:2:3:4 with vectors'
 allocate(UT(N1,M1),VT(N1,M1))
 UT=0 ; VT=0
 do i=1,M1
  do j=1,RadSlope%MV(i)
    CALL LIOC_Fortran(RadSlope%thta(i),RadSlope%r(j,i),JMatrix%YPR(j,i),JMatrix%YPTHETA(j,i),UT(j,i),VT(j,i))
   end do
 end do
 unitno1 = get_new_fileunit()
 open(unitno1, file=trim(gnu_instruct), action="write", iostat=ierr)
 do i=1,M1
  do j=1,RadSlope%MV(i)
   if (RadSlope%r(j,i) > 0) then
     U=RadSlope%r(j,i)*COS(RadSlope%thta(i))
     V=RadSlope%r(j,i)*SIN(RadSlope%thta(i))
   else
     U=-RadSlope%r(j,i)*COS(RadSlope%thta(i))
     V=-RadSlope%r(j,i)*SIN(RadSlope%thta(i))
   endif
   WRITE(unitno1,*) U,V,100*UT(j,i),100*VT(j,i)
  end do
  WRITE(unitno1,*) ' '
 end do
 close (unitno1)
 deallocate(UT,VT)
 return
endif ! (mod(flag,100) .eq. 7)

! Writes ASCII PLY file
if (mod(flag,100) == 3) then
if (allocated(JMatrix%R)) then
file_idx=index(inputfile1, ".ply")
 if( file_idx == 0) then
   write(*,*) inputfile1, 'is not a ply file'
   err_janus=3
   return
  else
  donut = .FALSE.
  call selectfunction(0,JMatrix,flag,powctr,powmin,powmax,cardinal,nC)
!  write(*,*) 'powctr,POWMIN,POWMAX',powctr,POWMIN,POWMAX
  call WriteGeomPLY(flag,JMatrix,donut,powmin,powmax,inputfile1)
  write(*,*) 'Wrote ply file...',inputfile1
  return
 endif
else
  write(*,*) 'Have to allocate data prior to writing a ply file'
  err_janus=3
 return ! if flag==3 and not allocated do nothing
endif
endif

! Writes OFF file
if (mod(flag,100) == 2) then
if (allocated(JMatrix%R)) then
file_idx=index(inputfile1, ".off")
 if( file_idx == 0) then
   write(*,*) inputfile1,'is not an off file'
   err_janus=2
   return
  else
  donut = .FALSE.
  call selectfunction(0,JMatrix,flag,powctr,powmin,powmax,cardinal,nC)
  write(*,*) 'powctr,POWMIN,POWMAX',powctr,POWMIN,POWMAX
  call WriteGeomOFF(flag,JMatrix,donut,powmin,powmax,inputfile1)
  write(*,*) 'Wrote off file...',inputfile1
  return
 endif
else
  write(*,*) 'Have to allocate data prior to writing an off file'
  err_janus=2
 return ! if flag==2 and not allocated do nothing
endif
endif

! simple swap
if (mod(flag,100) == 11) then
 if (allocated(JMatrix2%R) .and. loaded_files .ge. 2) then
  JMatrix2=JMatrix
  JMatrix=JMatrix1
  JMatrix1=JMatrix2
  donut = .FALSE.
  call selectfunction(0,JMatrix,flag,powctr,powmin,powmax,cardinal,nC)
  dist = real(-2*JMatrix%Z0(3),kind=sk)
! generate buffer data
  pupil_elements(1:pupil_nE)=0
  pupil_vertices(1:pupil_nV)=0
  elements(1:nE) = 0
  vertices(1:nV) = 0
  call Geom(flag, JMatrix, donut, powmin, powmax, elements, vertices, nV, nE)
  call Pupil(JMatrix, dist, pupil_elements, pupil_vertices, pupil_nV, pupil_nE)
  call makelegend(flag, powmin, powmax, legend, nL)
  return
 else
  write(*,*) "Needs two scans for swap"
  err_janus=11
  return
 endif
endif

! decenter is compare without a second scan
if (btest(dat,5)) then
  new_path = " "
  do i=1, 4096
   if ( file_from_C (i) == c_null_char ) then
       exit
   else
       new_path (i:i) = file_from_C (i)
   end if
  end do
! write(*,*) 'file from kernunos: ',trim(new_path)
  new_path=trim(new_path)
  read(new_path,*) dhoriz, dvert
 if (.true.) then  ! in case I can check need for valid data prior to running
!generate new JMatrix
  JMatrix3=JMatrix
  ctr_circle_x = dhoriz
  ctr_circle_y = dvert
  write(*,*) 'Decentering by',ctr_circle_x,ctr_circle_y
  call PolarTranslate(ctr_circle_x,ctr_circle_y,0.0_wp,0.0_wp,JMatrix3%R0,JMatrix3%THT0)
  do i=1,M1
   do j=1,JMatrix%MV(i)
    call PolarTranslate(ctr_circle_x, ctr_circle_y,JMatrix%R(j,i),JMatrix%THT(i),JMatrix3%R(j,i),JMatrix3%THT(i))
   end do
  end do
! regenerates based on new R/tht

  if (btest(dat,0)) then   !centernode
   iflag=10
  else
   iflag=0
  endif
! lsq instead of circumferential spline
  if (btest(dat, 8)) then
   iflag = iflag+100
  endif

! make the elevation based on the rings above and the original RadSlope for Z
  call selectfunction(2,JMatrix3,flag,powctr,powmin,powmax,cardinal,nC)

! remake Radslope/DiaSlope based on new JMatrix only on elevations, just like ELE or ELE_CSV
  RadSlope=0
  DiaSlope=0
  deallocate(RadSplineCenter)
! reinitialize with M1 and N1
  call init_mat(M1,N1,RadSlope,DiaSlope,RadSplineCenter)

! put elevation into Zp for splining without integration
  RadSlope = JMatrix3
! RadSlope%Zp(:,:)=0 ; JMatrix%SAGC(:,:) = 0 ; JMatrix%SAGC0(:) = 0 ! should not have anything in Zp or SAGC yet
  do i=1,M1
   do j=1,RadSlope%MV(i)
     RadSlope%Zp(j,i)=JMatrix3%Z(j,i) ! = RadSlope%Z(j,i) ! at this point
   end do
  end do
  DiaSlope = RadSlope
! use elevations, no integration
  if (btest(dat,0)) then
   iflag=10
   DiaSlope%Zpd2 = .nc. DiaSlope ! re-spline, with center node
  else
   iflag=0
   DiaSlope%Zpd2 = .n. DiaSlope
  endif

  if ( Testdata .le.1 .or. Testdata .eq. 6) then     ! not for Oculus PentaCam or Keratograph
   call MakeRadSplineCenter(zero_int64,error_report)   ! remakes RadSplineCenter(1,:)
   if (error_report .ne. 0) then
    write(*,*)' janus line number: ',__LINE__
   endif
  else
   RadSplineCenter(1,:)=0      ! pentacam and keratograph by definition is at 0
  endif

  if (btest(dat, 1)) then          ! moving each meridian to align curves
   call AdjustRadSplineCenter     ! changes r only
   DiaSlope%Zpd2 = .n. DiaSlope    ! re-spline, standard
  endif

  call MakeRadSplineCenter(dat,error_report)        ! generates spline centers with tweaks
  if (error_report .ne. 0) then
   write(*,*)' MakeRadSplineCenter janus line number: ',__LINE__
  endif

! Generate the surface
  do i=1,M1
   do j=1,JMatrix3%MV(i) ! does not include center point
!   generate elevations and derivatives; iflag no integration
    call SplineEval1Dx1D(iflag,JMatrix3%R(j,i),JMatrix3%THT(i),JMatrix3%Z(j,i),YPR,YP2R2,YPTHETA,YPRTHETA,YP2THETA)
!   save for vertex normals and for LIOC
    JMatrix3%YPR(j,i)=YPR
    JMatrix3%YPTHETA(j,i)=YPTHETA
!   powers
    call AXIALP(JMatrix3%R(j,i),YPTHETA/JMatrix3%R(j,i),YP2THETA/JMatrix3%R(j,i),JMatrix3%Warp(j,i))
!   not elevations
    if (btest(dat,10)) then
     call axisymmetric_principal(JMatrix%R(j,i),YPR,YP2R2,gaussian,meanpower,princ1,princ2,astigm)
    else
     call principal(JMatrix3%THT(i),JMatrix3%R(j,i),YPR,YPTHETA,YPRTHETA,YP2THETA,YP2R2,gaussian,meanpower,princ1,princ2,astigm)
    endif
    JMatrix3%MONGEA(j,i)=RFCT*astigm
    JMatrix3%MEANC(j,i)=RFCT*meanpower
    JMatrix3%INSTC(j,i)=RFCT*princ1
    JMatrix3%SAGC(j,i)=RFCT*princ2
    JMatrix3%GAUSSC(j,i)=RFCT*gaussian
   end do
  end do

! make new central values
  call centersJMatrix(JMatrix3,TestData,dat,iflag,cardinal,nC)

! find min and maximum
  call minmax(JMatrix3)
! draw
  donut = .FALSE.
  elements(1:nE) = 0
  vertices(1:nV) = 0
  call selectfunction(0,JMatrix3,flag,powctr,powmin,powmax,cardinal,nC)  !with 0 only loads powctr, powmin, powmax, cardinals
  call Geom(flag, JMatrix3, donut, powmin, powmax, elements, vertices, nV, nE)
  call makelegend(flag, powmin, powmax, legend, nL)
 !reset
  JMatrix=JMatrix3
  return
 endif
endif

! cropping removes borders from JMatrix
 if (btest(dat,12)) then
 !generate new JMatrix
   JMatrix3=JMatrix
  new_path = " "
  do i=1, 4096
   if ( file_from_C (i) == c_null_char ) then
       exit
   else
       new_path (i:i) = file_from_C (i)
   end if
  end do
  percent_squash = 0
! write(*,*) 'file from kernunos: ',trim(new_path)
  new_path=trim(new_path)
  read(new_path,*) new_crop, percent_squash
  if (crop .lt. 0) crop = 0 ! initializes and is a sanity check
  crop = crop + new_crop
  if (abs(crop) .le. 10) then
   write(*,*) 'Cropping by',crop, 'Squashing by', percent_squash
   if (.not. allocated(MV)) then
    allocate(MV(M1))
   else
    deallocate(MV)
    allocate(MV(M1))
   endif
!  store
   MV(:)=JMatrix%MV(:)
   JMatrix3%MV(:)=min(JMatrix%MV(:)-crop,MV(:))
!  find min and maximum
   call minmax(JMatrix3)
   if (percent_squash .gt.0) then
    call squash(JMatrix3,percent_squash)
    call minmax(JMatrix3)
   endif
!  draw
   donut = .FALSE.
   elements(1:nE) = 0
   vertices(1:nV) = 0
   call selectfunction(0,JMatrix3,flag,powctr,powmin,powmax,cardinal,nC)  !with 0 only loads powctr, powmin, powmax, cardinals
   call Geom(flag, JMatrix3, donut, powmin, powmax, elements, vertices, nV, nE)
   call makelegend(flag, powmin, powmax, legend, nL)
   return
!  reset
   JMatrix%MV(:)=MV(:)
  else
   write(*,*) 'Crop must be greater than zero and less than six, got previous crop, new crop:', crop, new_crop
  endif
 endif

! simple difference/subtraction with compare
if (mod(flag,100) == 10) then
!generate new JMatrix
 JMatrix3=JMatrix
  if (allocated(JMatrix2%R) .and. loaded_files .ge. 2) then
! use geometry from current JMatrix to populate
  JMatrix2%R(:,:)=JMatrix%R(:,:) ! might have zeroes if smaller, but should be caught by MV below
  JMatrix2%THT(:)=JMatrix%THT(:)
  JMatrix2%R0=JMatrix%R0
  JMatrix2%THT0=JMatrix%THT0
! if no pupil registration and rotationdegrees is even, then there's a shortcut not requiring resplining
 if (btest(dat,6)) then
  ctr_circle_x=JMatrix1%Pupil_Center(1)-JMatrix%Pupil_Center(1)
  ctr_circle_y=JMatrix1%Pupil_Center(2)-JMatrix%Pupil_Center(2)
  write(*,*) 'Decentering by',ctr_circle_x,ctr_circle_y
  call PolarTranslate(ctr_circle_x,ctr_circle_y,0.0_wp,0.0_wp,JMatrix3%R0,JMatrix3%THT0)
  do i=1,M1
   do j=1,JMatrix%MV(i)
    call PolarTranslate(ctr_circle_x, ctr_circle_y,JMatrix%R(j,i),JMatrix%THT(i),JMatrix3%R(j,i),JMatrix3%THT(i))
   end do
  end do
! regenerates based on new R/tht
  call selectfunction(1,JMatrix3,flag,powctr,powmin,powmax,cardinal,nC)
 endif

 rotationdegrees=mod(270-rotationdegrees/2,180) ! makes 180 no rotation without risk of negative indices, odd rotation degrees get floor
 if (rotationdegrees .ne. 0) then
  do i=1,M1
   j = mod(i + rotationdegrees,180)
   if (j .eq. 0) j = 180
   JMatrix2%MV(i)=min(JMatrix%MV(j),JMatrix1%MV(i))
   JMatrix2%Z(:,i)=ABS(JMatrix1%Z(:,i)-JMatrix3%Z(:,j))
   JMatrix2%SAGC(:,i)=ABS(JMatrix1%SAGC(:,i)-JMatrix3%SAGC(:,j))
   JMatrix2%Warp(:,i)=ABS(JMatrix1%Warp(:,i)-JMatrix3%Warp(:,j))
   JMatrix2%INSTC(:,i)=ABS(JMatrix1%INSTC(:,i)-JMatrix3%INSTC(:,j))
   JMatrix2%GAUSSC(:,i)=ABS(JMatrix1%GAUSSC(:,i)-JMatrix3%GAUSSC(:,j))
   JMatrix2%MEANC(:,i)=ABS(JMatrix1%MEANC(:,i)-JMatrix3%MEANC(:,j))
   JMatrix2%MONGEA(:,i)=ABS(JMatrix1%MONGEA(:,i)-JMatrix3%MONGEA(:,j))
   JMatrix2%ZC(:,i,:)=ABS(JMatrix1%ZC(:,i,:)-JMatrix3%ZC(:,j,:))
  end do
 else
  JMatrix2%MV(:)=min(JMatrix%MV(:),JMatrix1%MV(:))
  JMatrix2%Z(:,:)=ABS(JMatrix1%Z(:,:)-JMatrix3%Z(:,:))
  JMatrix2%SAGC(:,:)=ABS(JMatrix1%SAGC(:,:)-JMatrix3%SAGC(:,:))
  JMatrix2%Warp(:,:)=ABS(JMatrix1%Warp(:,:)-JMatrix3%Warp(:,:))
  JMatrix2%INSTC(:,:)=ABS(JMatrix1%INSTC(:,:)-JMatrix3%INSTC(:,:))
  JMatrix2%GAUSSC(:,:)=ABS(JMatrix1%GAUSSC(:,:)-JMatrix3%GAUSSC(:,:))
  JMatrix2%MEANC(:,:)=ABS(JMatrix1%MEANC(:,:)-JMatrix3%MEANC(:,:))
  JMatrix2%MONGEA(:,:)=ABS(JMatrix1%MONGEA(:,:)-JMatrix3%MONGEA(:,:))
 endif
 JMatrix2%SAGC0(:)=ABS(JMatrix1%SAGC0(:)-JMatrix3%SAGC0(:))
 JMatrix2%Z0(:)=ABS(JMatrix1%Z0(:)-JMatrix3%Z0(:))
 JMatrix2%Warp0(:)=ABS(JMatrix1%Warp0(:)-JMatrix3%Warp0(:))
 JMatrix2%INSTC0(:)=ABS(JMatrix1%INSTC0(:)-JMatrix3%INSTC0(:))
 JMatrix2%GAUSSC0(:)=ABS(JMatrix1%GAUSSC0(:)-JMatrix3%GAUSSC0(:))
 JMatrix2%MEANC0(:)=ABS(JMatrix1%MEANC0(:)-JMatrix3%MEANC0(:))
 JMatrix2%MONGEA0(:)=ABS(JMatrix1%MONGEA0(:)-JMatrix3%MONGEA0(:))
 JMatrix2%ZC(:,:,:)=ABS(JMatrix1%ZC(:,:,:)-JMatrix3%ZC(:,:,:))
 JMatrix2%ZC0(:,:)=ABS(JMatrix1%ZC0(:,:)-JMatrix3%ZC0(:,:))
! have to re-do min/max
 JMatrix2%SAGC0(2)=1E30   ;  JMatrix2%SAGC0(3)=-1E30
 JMatrix2%Warp0(2)=1E30   ;  JMatrix2%Warp0(3)=-1E30
 JMatrix2%Z0(2)=1E30      ;  JMatrix2%Z0(3)=-1E30
 JMatrix2%INSTC0(2)=1E30  ;  JMatrix2%INSTC0(3)=-1E30
 JMatrix2%GAUSSC0(2)=1E30 ;  JMatrix2%GAUSSC0(3)=-1E30
 JMatrix2%MEANC0(2)=1E30  ;  JMatrix2%MEANC0(3)=-1E30
 JMatrix2%MONGEA0(2)=1E30 ;  JMatrix2%MONGEA0(3)=-1E30
 JMatrix2%ZC0(2,:)=1E30   ;  JMatrix2%ZC0(3,:)=-1E30
 if (JMatrix2%INSTC0(1) <= JMatrix2%INSTC0(2)) JMatrix2%INSTC0(2)=JMatrix2%INSTC0(1)
 if (JMatrix2%INSTC0(1) >= JMatrix2%INSTC0(3)) JMatrix2%INSTC0(3)=JMatrix2%INSTC0(1)
 if (JMatrix2%GAUSSC0(1) <= JMatrix2%GAUSSC0(2)) JMatrix2%GAUSSC0(2)=JMatrix2%GAUSSC0(1)
 if (JMatrix2%GAUSSC0(1) >= JMatrix2%GAUSSC0(3)) JMatrix2%GAUSSC0(3)=JMatrix2%GAUSSC0(1)
 if (JMatrix2%Z0(1) <= JMatrix2%Z0(2)) JMatrix2%Z0(2)=JMatrix2%Z0(1)
 if (JMatrix2%Z0(1) >= JMatrix2%Z0(3)) JMatrix2%Z0(3)=JMatrix2%Z0(1)
 if (JMatrix2%SAGC0(1) <= JMatrix2%SAGC0(2)) JMatrix2%SAGC0(2)=JMatrix2%SAGC0(1)
 if (JMatrix2%SAGC0(1) >= JMatrix2%SAGC0(3)) JMatrix2%SAGC0(3)=JMatrix2%SAGC0(1)
 if (JMatrix2%Warp0(1) <= JMatrix2%Warp0(2)) JMatrix2%Warp0(2)=JMatrix2%Warp0(1)
 if (JMatrix2%Warp0(1) >= JMatrix2%Warp0(3)) JMatrix2%Warp0(3)=JMatrix2%Warp0(1)
 if (JMatrix2%MEANC0(1) <= JMatrix2%MEANC0(2)) JMatrix2%MEANC0(2)=JMatrix2%MEANC0(1)
 if (JMatrix2%MEANC0(1) >= JMatrix2%MEANC0(3)) JMatrix2%MEANC0(3)=JMatrix2%MEANC0(1)
 if (JMatrix2%MONGEA0(1) <= JMatrix2%MONGEA0(2)) JMatrix2%MONGEA0(2)=JMatrix2%MONGEA0(1)
 if (JMatrix2%MONGEA0(1) >= JMatrix2%MONGEA0(3)) JMatrix2%MONGEA0(3)=JMatrix2%MONGEA0(1)
 do k = 1,15
  if (JMatrix2%ZC0(1,k) <= JMatrix2%ZC0(2,k)) JMatrix2%ZC0(2,k)=JMatrix2%ZC0(1,k)
  if (JMatrix2%ZC0(1,k) >= JMatrix2%ZC0(3,k)) JMatrix2%ZC0(3,k)=JMatrix2%ZC0(1,k)
 end do
 do i=1,M1
  do j=1,JMatrix2%MV(i)
   if (JMatrix2%INSTC(j,i) <= JMatrix2%INSTC0(2)) JMatrix2%INSTC0(2)=JMatrix2%INSTC(j,i)
   if (JMatrix2%INSTC(j,i) >= JMatrix2%INSTC0(3)) JMatrix2%INSTC0(3)=JMatrix2%INSTC(j,i)
   if (JMatrix2%GAUSSC(j,i) <= JMatrix2%GAUSSC0(2)) JMatrix2%GAUSSC0(2)=JMatrix2%GAUSSC(j,i)
   if (JMatrix2%GAUSSC(j,i) >= JMatrix2%GAUSSC0(3)) JMatrix2%GAUSSC0(3)=JMatrix2%GAUSSC(j,i)
   if (JMatrix2%Z(j,i) <= JMatrix2%Z0(2)) JMatrix2%Z0(2)=JMatrix2%Z(j,i)
   if (JMatrix2%Z(j,i) >= JMatrix2%Z0(3)) JMatrix2%Z0(3)=JMatrix2%Z(j,i)
   if (JMatrix2%SAGC(j,i) <= JMatrix2%SAGC0(2)) JMatrix2%SAGC0(2)=JMatrix2%SAGC(j,i)
   if (JMatrix2%SAGC(j,i) >= JMatrix2%SAGC0(3)) JMatrix2%SAGC0(3)=JMatrix2%SAGC(j,i)
   if (JMatrix2%Warp(j,i) <= JMatrix2%Warp0(2)) JMatrix2%Warp0(2)=JMatrix2%Warp(j,i)
   if (JMatrix2%Warp(j,i) >= JMatrix2%Warp0(3)) JMatrix2%Warp0(3)=JMatrix2%Warp(j,i)
   if (JMatrix2%MEANC(j,i) <= JMatrix2%MEANC0(2)) JMatrix2%MEANC0(2)=JMatrix2%MEANC(j,i)
   if (JMatrix2%MEANC(j,i) >= JMatrix2%MEANC0(3)) JMatrix2%MEANC0(3)=JMatrix2%MEANC(j,i)
   if (JMatrix2%MONGEA(j,i) <= JMatrix2%MONGEA0(2)) JMatrix2%MONGEA0(2)=JMatrix2%MONGEA(j,i)
   if (JMatrix2%MONGEA(j,i) >= JMatrix2%MONGEA0(3)) JMatrix2%MONGEA0(3)=JMatrix2%MONGEA(j,i)
   do k = 1,15
    if (JMatrix2%ZC(j,i,k) <= JMatrix2%ZC0(2,k)) JMatrix2%ZC0(2,k)=JMatrix2%ZC(j,i,k)
    if (JMatrix2%ZC(j,i,k) >= JMatrix2%ZC0(3,k)) JMatrix2%ZC0(3,k)=JMatrix2%ZC(j,i,k)
   end do
  end do
 end do
  donut = .FALSE.
  elements(1:nE) = 0
  vertices(1:nV) = 0
  call selectfunction(0,JMatrix2,flag,powctr,powmin,powmax,cardinal,nC)
  call Geom(flag, JMatrix2, donut, powmin, powmax, elements, vertices, nV, nE)
  call makelegend(flag, powmin, powmax, legend, nL)
  return
 else
  write(*,*) "Needs two scans for compare"
  err_janus=10
  return
 endif
endif

! last two digits of flag == 0 parse file name, assign TestData type and MM,N
! TestData -1  Test, no data file
! TestData 0 EyeSys file
! TestData 1 Zeiss Atlas file
! TestData 2 Oculus PentaCam .ELE file
! TestData 3 Oculus PentaCam .CUR file
! TestData 4 Oculus PentaCam _ELE.CSV file
! TestData 5 Oculus PentaCam _ELE.CSV file
! TestData 6 NIDEK
! TestData 7 Oculus Keratograph 5M
if (mod(flag,100) == 0) then
 call CCounter(0,inputfile1//c_null_char)
! For EyeSys either RA?.? or XX?.?, set inputfile1 to the XX version, inputfile2 to the RA version, inputfile3 to the PU version,
! For NIDEK either RA?.? or ED?.?, set inputfile1 to the XX version, inputfile2 to the RA version, inputfile3 to the HT version, inputfile4 to PE
! For PentaCam set inputfile1 for _ELE.CSV or .ELE, set inputfile2 for _CUR.CSV or .CUR
! For CSV but not _ELE.CSV or _CUR.CSV set inputfile1 to Atlas file
! For Oculus Keratograph files ending in .OD or .OS set inputfile2 to CURVAT or CURVAT_F, inputfile1 to CORNEA or CORNEA_F, inputfile3 to PUPIL, inputfile4 to CENTER
 file_idx=index(inputfile1, "RA")+index(inputfile1, "XX")+index(inputfile1, "ED")
   if( file_idx == 0)then
      file_idx=index(inputfile1, ".CSV")
      if( file_idx == 0) then
       file_idx=index(inputfile1, ".CUR")
       if( file_idx == 0) then
        file_idx=index(inputfile1, ".ELE")
        if( file_idx == 0) then
         file_idx=index(inputfile1, ".OD")+index(inputfile1, ".OS")+index(inputfile1, "EXP_Topo")
         if( file_idx == 0) then
         write(*,*) 'Unknown file type: make some test data, flag = ',flag
         TestData=-1; MM=360; N=16 ; NP=141
         else
         TestData=7; MM=100; N=60 ; NP=141
          file_idx=index(inputfile1, "EXP_Topo")
          if( file_idx .ne. 0) then    !set the files to versions in the base directory that were decompressed by kernunos.cpp
           if (allocated(cab_inputfile1)) then
            deallocate(cab_inputfile1)
           endif
           allocate(character(nblines) :: cab_inputfile1)
           cab_inputfile1=inputfile1
           file_idx=index(inputfile1, "_OS.")
           if (file_idx == 0 ) then  !OD
            inquire(file="CURVAT_F.OD", exist=exists)
            if (exists) then
             inputfile2="CURVAT_F.OD"
             inputfile1="CORNEA_F.OD"
            else
             inputfile2="CURVAT.OD"
             inputfile1="CORNEA.OD"
            endif
            inputfile3="PUPIL.OD"
            inputfile4="CENTER.OD"
            inputfile5=replacestr(string=cab_inputfile1,search="EXP_Topo_OD.zip",substitute="ZERNIKE.CSV")
            inputfile6="PATIENT.TXT"
            inputfile7="EXAM.TXT"
           else  !OS
           inquire(file="CURVAT_F.OS", exist=exists)
            if (exists) then
             inputfile2="CURVAT_F.OS"
             inputfile1="CORNEA_F.OS"
            else
             inputfile2="CURVAT.OS"
             inputfile1="CORNEA.OS"
            endif
            inputfile3="PUPIL.OS"
            inputfile4="CENTER.OS"
            inputfile5=replacestr(string=cab_inputfile1,search="EXP_Topo_OS.zip",substitute="ZERNIKE.CSV")
            inputfile6="PATIENT.TXT"
            inputfile7="EXAM.TXT"
           endif
          else   !not compressed
          file_idx=index(inputfile1, "CURVAT")
          if( file_idx == 0) then ! a CORNEA file
           file_idx=index(inputfile1, "CORNEA")
           if( file_idx == 0) then
            write(*,*) 'Error in finding Keratograph file'
            return
           endif
           file_idx=index(inputfile1, "_F")
           if( file_idx == 0) then
            inputfile2=replacestr(string=inputfile1,search="CORNEA",substitute="CURVAT")
            inquire(file=trim(inputfile2), exist=exists)
             if (.not. exists) then
              inputfile2=replacestr(string=inputfile1,search="CORNEA",substitute="CURVAT_F")
             endif
             inputfile3=replacestr(string=inputfile1,search="CORNEA",substitute="PUPIL")
             inputfile4=replacestr(string=inputfile1,search="CORNEA",substitute="CENTER")
           else
            inputfile2=replacestr(string=inputfile1,search="CORNEA_F",substitute="CURVAT_F")
            inquire(file=trim(inputfile2), exist=exists)
             if (.not. exists) then
              inputfile2=replacestr(string=inputfile1,search="CORNEA_F",substitute="CURVAT")
             endif
              inputfile3=replacestr(string=inputfile1,search="CORNEA_F",substitute="PUPIL")
              inputfile4=replacestr(string=inputfile1,search="CORNEA_F",substitute="CENTER")
           endif
           else ! a CURVAT file
            file_idx=index(inputfile1, "CURVAT")
            if( file_idx == 0) then
             write(*,*) 'Error in finding Keratograph file'
             return
            endif
            file_idx=index(inputfile1, "_F")
             if( file_idx == 0) then
              inputfile2 = inputfile1
              inputfile1=replacestr(string=inputfile1,search="CURVAT",substitute="CORNEA")
              inquire(file=trim(inputfile1), exist=exists)
               if (.not. exists) then
                inputfile1=replacestr(string=inputfile2,search="CURVAT",substitute="CORNEA_F")
               endif
               inputfile3=replacestr(string=inputfile2,search="CURVAT",substitute="PUPIL")
               inputfile4=replacestr(string=inputfile2,search="CURVAT",substitute="CENTER")
             else
              inputfile2 = inputfile1
              inputfile1=replacestr(string=inputfile2,search="CURVAT_F",substitute="CORNEA_F")
              inquire(file=trim(inputfile1), exist=exists)
              if (.not. exists) then
               inputfile1=replacestr(string=inputfile2,search="CURVAT_F",substitute="CORNEA")
               endif
              inputfile3=replacestr(string=inputfile2,search="CURVAT_F",substitute="PUPIL")
              inputfile4=replacestr(string=inputfile2,search="CURVAT_F",substitute="CENTER")
             endif
           endif !cornea or curvat
           file_idx=index(inputfile1, ".OD")
           if (file_idx .ne.0) then
            inputfile5=replacestr(string=inputfile3,search="PUPIL.OD",substitute="ZERNIKE.CSV")
            inputfile6=replacestr(string=inputfile3,search="PUPIL.OD",substitute="PATIENT.TXT")
            inputfile7=replacestr(string=inputfile3,search="PUPIL.OD",substitute="EXAM.TXT")
           endif
           file_idx=index(inputfile1, ".OS")
           if (file_idx .ne.0) then
            inputfile5=replacestr(string=inputfile3,search="PUPIL.OS",substitute="ZERNIKE.CSV")
            inputfile6=replacestr(string=inputfile3,search="PUPIL.OS",substitute="PATIENT.TXT")
            inputfile7=replacestr(string=inputfile3,search="PUPIL.OS",substitute="EXAM.TXT")
           endif
          endif
         endif !Keratograph
        else
        inputfile2=replacestr(string=inputfile1,search=".ELE",substitute=".CUR")
        write(*,*) "PentaCam .ELE file",inputfile1
        inquire(file=trim(inputfile2), exist=exists)
        if(exists) then
         write(*,*) "Matching .CUR file found"
        endif
        TestData=2; MM=180; N=22; NP=141 ! PentaCam ELE
       endif
      else
       inputfile2=inputfile1
       inputfile1=replacestr(string=inputfile2,search=".CUR",substitute=".ELE")
       write(*,*) "PentaCam .CUR file: ",inputfile2
       inquire(file=trim(inputfile1), exist=exists)
       if(exists) then
        write(*,*) "Matching .ELE file found"
       endif
       TestData=3; MM=180; N=22; NP=141 ! PentaCam CUR
      endif
      else
       file_idx=index(inputfile1, "_CUR")
       if( file_idx == 0) then
        file_idx=index(inputfile1, "_ELE")
        if( file_idx == 0) then
         TestData=1; MM=180; N=25   ! Atlas 900 can be 25, 9000 seems to be 22
         write(*,*) "Atlas file: ",inputfile1
        else
         inputfile2=replacestr(string=inputfile1,search="_ELE.CSV",substitute="_CUR.CSV")
         write(*,*) "PentaCam _ELE.CSV file: ",inputfile1
         inquire(file=trim(inputfile2), exist=exists)
         if(exists) then
          write(*,*) "Matching _CUR.CSV file found"
         endif
         TestData=4; MM=180; N=22; NP=141 ! PentaCam _ELE.CSV
        endif
        else
         inputfile2=inputfile1
         inputfile1=replacestr(string=inputfile2,search="_CUR.CSV",substitute="_ELE.CSV")
         write(*,*) "PentaCam _CUR.CSV file: ",inputfile2
         inquire(file=trim(inputfile1), exist=exists)
         if(exists) then
          write(*,*) "Matching _ELE.CSV file found"
         endif
         TestData=5; MM=180; N=22; NP=141 ! PentaCam _CUR.CSV
       endif
      endif
   else
!  EyeSys or Nidek
      file_idx=index(inputfile1, "XX")  !index(inputfile1, "XX", back)
      if (file_idx /= 0) then
!       write(*,*) 'prefix is found at index: ',file_idx,"length: ",len(inputfile1)
!       write(*,*) 'prefix:',inputfile1(file_idx:file_idx+1)
       write(*,*) 'EyeSys XX file: ',inputfile1
       inputfile2=replacestr(string=inputfile1,search="XX",substitute="RA")
       inquire(file=trim(inputfile2), exist=exists)
       if(exists) then
        TestData=0 ; MM=360; N=16   ! EyeSys

!        TestData=0 ; MM=256; N=24   ! Visia


        write(*,*) "Matching EyeSys RA file",inputfile2
       endif
       inputfile3=replacestr(string=inputfile1,search="XX",substitute="PU")
       inputfile4=replacestr(string=inputfile1,search="XX",substitute="HX")
       file_idx=index(inputfile4, ".")
!          write(*,*) 'suffix is found at index: ',file_idx,"length: ",len(inputfile4)
!          write(*,*) 'suffix:',inputfile4(file_idx:len(inputfile4))
       inputfile4=inputfile4(1:file_idx) // "HDR"
       inquire(file=trim(inputfile4), exist=exists)
       if(exists) then
        write(*,*) "Matching EyeSys HX/HDR file found"
       endif
       inquire(file=trim(inputfile3), exist=exists)
       if(exists) then
        write(*,*) "Matching EyeSys PU file found"
       endif
       inquire(file=trim(inputfile2), exist=exists)
       if(.NOT.exists) then
        inputfile2=replacestr(string=inputfile1,search="/XX",substitute="/RA")
        inquire(file=trim(inputfile2), exist=exists)
        if(exists) then
         TestData=0 ; MM=360; N=16   ! EyeSys

!         TestData=0 ; MM=256; N=24   ! Visia

         write(*,*) "Matching EyeSys RA file",inputfile2
        endif
        inputfile3=replacestr(string=inputfile1,search="/XX",substitute="/PU")
        inputfile4=replacestr(string=inputfile1,search="/XX",substitute="/HX")
        file_idx=index(inputfile4, ".")
!          write(*,*) 'suffix is found at index: ',file_idx,"length: ",len(inputfile4)
!          write(*,*) 'suffix:',inputfile4(file_idx:len(inputfile4))
        inputfile4=inputfile4(1:file_idx) // "HDR"
        inquire(file=trim(inputfile4), exist=exists)
        if(exists) then
         write(*,*) "Matching EyeSys HX/HDR file found"
        endif
        inquire(file=trim(inputfile3), exist=exists)
        if(exists) then
         write(*,*) "Matching EyeSys PU file found"
        endif
        inquire(file=trim(inputfile2), exist=exists)
        if(.NOT.exists) then
         write(*,*) 'Error: EyeSys files have to be in pairs, or file name has XX other than prefix'
         write(*,*) 'No corresponding',inputfile2,'for',inputfile1,'found'
         err_janus=-1
         return
        endif
       endif
      else
       file_idx=index(inputfile1, "RA") !index(inputfile1, "RA", back)
       if (file_idx /= 0) then
        write(*,*) 'EyeSys or Nidek RA file: ',inputfile1
        inputfile2=inputfile1
        inputfile1=replacestr(string=inputfile2,search="RA",substitute="XX")
        inquire(file=trim(inputfile1), exist=exists)
        if(.NOT.exists) then
         inputfile1=replacestr(string=inputfile2,search="RA",substitute="ED")
         inquire(file=trim(inputfile1), exist=exists)
         if(.NOT.exists) then
          write(*,*) 'Error: EyeSys and Nidek files have to be in pairs RA/XX or RA/ED, or file name has RA other than prefix'
          write(*,*) 'No corresponding',inputfile1,'for',inputfile2,'found'
          err_janus=-1
          return
         else
          write(*,*) 'Matching Nidek ED file: ',inputfile1
          TestData=6 ; MM=360   ! Nidek
          inputfile3=replacestr(string=inputfile2,search="RA",substitute="HT")
          inputfile4=replacestr(string=inputfile2,search="RA",substitute="PE")
          inquire(file=trim(inputfile3), exist=exists)
          if(exists) then
           write(*,*) "Matching Nidek HT file found",inputfile3
          endif
          inquire(file=trim(inputfile4), exist=exists)
          if(exists) then
           write(*,*) "Matching Nidek PE file found ",inputfile4
          endif
         endif
        else
         TestData=0 ; MM=360; N=16   ! EyeSys

!         TestData=0 ; MM=256; N=24   ! Visia

         write(*,*) 'Matching EyeSys XX file: ',inputfile1
         inputfile3=replacestr(string=inputfile2,search="RA",substitute="PU")
         inputfile4=replacestr(string=inputfile2,search="RA",substitute="HX")
         file_idx=index(inputfile4, ".")
!          write(*,*) 'suffix is found at index: ',file_idx,"length: ",len(inputfile4)
!          write(*,*) 'suffix:',inputfile4(file_idx:len(inputfile4))
         inputfile4=inputfile4(1:file_idx) // "HDR"
         inquire(file=trim(inputfile4), exist=exists)
         if(exists) then
          write(*,*) "Matching EyeSys HX/HDR file found"
         endif
         inquire(file=trim(inputfile3), exist=exists)
         if(exists) then
          write(*,*) "Matching EyeSys PU file found"
         endif
        endif
        inquire(file=trim(inputfile1), exist=exists)
        if(.NOT.exists) then
         inputfile1=replacestr(string=inputfile2,search="/RA",substitute="/XX")
         inquire(file=trim(inputfile1), exist=exists)
         if(.NOT.exists) then
          inputfile1=replacestr(string=inputfile2,search="/RA",substitute="/ED")
          inquire(file=trim(inputfile1), exist=exists)
          if(.NOT.exists) then
           write(*,*) 'Error: EyeSys and Nidek files have to be in pairs, or file name has RA other than prefix'
           write(*,*) 'No corresponding',inputfile1,'for',inputfile2,'found'
           err_janus=-1
           return
          else
           write(*,*) 'Matching Nidek ED file: ',inputfile1
           TestData=6 ; MM=360    ! Nidek
           inputfile3=replacestr(string=inputfile2,search="/RA",substitute="/HT")
           inputfile4=replacestr(string=inputfile2,search="/RA",substitute="/PE")
           inquire(file=trim(inputfile3), exist=exists)
           if(exists) then
            write(*,*) "Matching Nidek HT file found",inputfile3
           endif
           inquire(file=trim(inputfile4), exist=exists)
           if(exists) then
            write(*,*) "Matching Nidek PE file found ",inputfile4
           endif
          endif
         else
          TestData=0 ; MM=360; N=16   ! EyeSys


!          TestData=0 ; MM=256; N=24   ! Visia


          write(*,*) 'Matching EyeSys XX file: ',inputfile1
          inputfile3=replacestr(string=inputfile2,search="/RA",substitute="/PU")
          inputfile4=replacestr(string=inputfile2,search="/RA",substitute="/HX")
          file_idx=index(inputfile4, ".")
!           write(*,*) 'suffix is found at index: ',file_idx,"length: ",len(inputfile4)
!           write(*,*) 'suffix:',inputfile4(file_idx:len(inputfile4))
          inputfile4=inputfile4(1:file_idx) // "HDR"
          inquire(file=trim(inputfile4), exist=exists)
          if(exists) then
           write(*,*) "Matching EyeSys HX/HDR file found"
          endif
          inquire(file=trim(inputfile3), exist=exists)
          if(exists) then
           write(*,*) "Matching EyeSys PU file found"
          endif
         endif
        endif
       else
        file_idx=index(inputfile1, "ED") !index(inputfile1, "ED", back)
        if (file_idx /= 0) then
         write(*,*) 'Nidek ED file: ',inputfile1
         inputfile2=replacestr(string=inputfile1,search="ED",substitute="RA")
         inquire(file=trim(inputfile2), exist=exists)
         if(.NOT.exists) then
          write(*,*) 'Error: EyeSys and Nidek files have to be in pairs, or file name has RA other than prefix'
          write(*,*) 'No corresponding',inputfile2,'for',inputfile1,'found'
          err_janus=-1
          return
         else
          write(*,*) 'Matching Nidek RA file: ',inputfile2
          TestData=6 ; MM=360   ! Nidek
          inputfile3=replacestr(string=inputfile2,search="RA",substitute="HT")
          inputfile4=replacestr(string=inputfile2,search="RA",substitute="PE")
          inquire(file=trim(inputfile3), exist=exists)
          if(exists) then
           write(*,*) "Matching Nidek HT file found",inputfile3
          endif
          inquire(file=trim(inputfile4), exist=exists)
          if(exists) then
           write(*,*) "Matching Nidek PE file found ",inputfile4
          endif
         endif
        else
         write(*,*) 'Error parsing EyeSys file name'
         err_janus=-1
         return
        endif
       endif
      endif
     endif
!    write(*,*)  "TestData,MM,N",TestData,MM,N
 endif ! (mod(flag,100) == 0) parsing the file name,assigning TestData type and MM,N

if (TestData .eq. 0) then
 MM=360 ; N=16 ! EyeSys if file not read; should not be necessary as should agree with previous value.

! MM=256 ; N=24 ! Visia if file not read; should not be necessary as should agree with previous value.

 if (mod(flag,100) == 0) then !read the files
! READ THE EYESYS DATA
! XX????? ARE THE AXIAL DIST. RX???? ARE THE MIRE RADII  
   call CPU_TIME(time_start)
   read_error=0
   if(.not.allocated(EyeSys%RA)) then
    call init_mat_EyeSys(MM,N,EyeSys) ! allocate the EyeSys matrices
   else
    EyeSys = 0
    call init_mat_EyeSys(MM,N,EyeSys) ! allocate the EyeSys matrices
   endif
   inquire(file=trim(inputfile3), exist=exists)
   if(.NOT.exists) then

    call RCNVRTE(read_error,inputfile2,inputfile1)


!    call RCNVRTV(read_error,inputfile2,inputfile1) !Visia version


   else
    inquire(file=trim(inputfile4), exist=exists)
    if(.NOT.exists) then
     call RCNVRTE(read_error,inputfile2,inputfile1,inputfile3)
    else
     call RCNVRTE(read_error,inputfile2,inputfile1,inputfile3,inputfile4)
    endif
!   pupil conversion if any
    JMatrix%Pupil_Center=EyeSys%Pupil_Center/10.
    do i=1,MM/2
     JMatrix%PU(i)=(EyeSys%PU(2*i-1)+EyeSys%PU(2*i))/40.  !2x2x10 average,diameter->radius,factor of 10
    end do
   endif
   call CPU_TIME(time_end)
   write(*,*) 'Time to read EyeSys files: ',(time_end-time_start)*1000
   if (read_error > 0) then
    err_janus=read_error*10
    return
   endif
  endif  !(mod(flag,100) = 0
  ! wipe RadSlope/DiaSlope clean to ensure the correct MM,N based on previous assignment
  if (allocated(RadSlope%r)) then
   RadSlope = 0 ; DiaSlope = 0 ; deallocate(RadSplineCenter)
   call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  else
   call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  endif
! Generate the slope matrix

  RadSlope=EyeSys
!  call RadSlope_eq_Visia(RadSlope,EyeSys)

endif

if (TestData .eq. 7) then
 if (mod(flag,100) == 0) then !read the files
! READ THE KERATOGRAPH DATA
  call CPU_TIME(time_start)
  MM=100 ; N=60 ! Keratograph
  read_error=0
  if(.not.allocated(Oculus%SAGC)) then
   call init_mat_Oculus(MM,N,Oculus) ! allocate the matrices
  else
   Oculus = 0
   call init_mat_Oculus(MM,N,Oculus) ! allocate the matrices
  endif
  ! RCNVRTK has to decide whether files exist too.
  inquire(file=trim(inputfile3), exist=exists)
  if(.NOT.exists) then
   call RCNVRTK(read_error,inputfile1,inputfile2)
  else
   inquire(file=trim(inputfile4), exist=exists)
   if(.NOT.exists) then
    call RCNVRTK(read_error,inputfile1,inputfile2,inputfile3)
   else
    inquire(file=trim(inputfile5), exist=exists)  !?ZERNIKE
    if (exists) then
     inquire(file=trim(inputfile6), exist=exists) !?PATIENT.TXT (need PATIENT AND EXAM to read ZERNIKE)
     if (exists) then
      inquire(file=trim(inputfile7), exist=exists) !?EXAM.TXT
      if (exists) then
       call RCNVRTK(read_error,inputfile1,inputfile2,inputfile3,inputfile4,inputfile5,inputfile6,inputfile7)
      endif
     endif
    else
     call RCNVRTK(read_error,inputfile1,inputfile2,inputfile3,inputfile4)
    endif
   endif
   if (cab_inputfile1 .ne. inputfile1) then
    if(allocated(cab_inputfile1)) then
     write(*,*) "Removing temp files"  ! do not remove ZERNIKE inputfile5
     call execute_command_line ('rm ' // inputfile1, exitstat=io)
     read_error=io
     call execute_command_line ('rm ' // inputfile2, exitstat=io)
     read_error=read_error+io
     call execute_command_line ('rm ' // inputfile3, exitstat=io)
     read_error=read_error+io
     call execute_command_line ('rm ' // inputfile4, exitstat=io)
     read_error=read_error+io
     call execute_command_line ('rm ' // inputfile6, exitstat=io)
     read_error=read_error+io
     call execute_command_line ('rm ' // inputfile7, exitstat=io)
     read_error=read_error+io
     if (read_error > 0) then
      write (*,*) 'failed system command to remove one of temporary uncompressed keratograph files '
      read_error=13
     endif
     deallocate(cab_inputfile1)
    endif
   endif
  endif
 endif !(mod(flag,100) = 0
 ! wipe RadSlope/DiaSlope clean to ensure the correct MM,N based on previous assignment
 if (allocated(RadSlope%r)) then
  RadSlope = 0 ; DiaSlope = 0 ; deallocate(RadSplineCenter)
  call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
 else
  call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
 endif
! Generate the slope matrix
 RadSlope=Oculus
endif

if (TestData .eq. 6) then
 if (mod(flag,100) == 0) then !read the files
! READ THE NIDEK DATA
! RA????? ARE THE AXIAL DIST. ED???? ARE THE MIRE RADII; use the first set of 360 from ASCII ED**.DAT
   call CPU_TIME(time_start)
   MM=360 ; N=39 ! Nidek binary default
   read_error=0
   if(.not.allocated(EyeSys%RA)) then
    call init_mat_EyeSys(MM,N,EyeSys) ! allocate the EyeSys matrices
   else
    EyeSys = 0
    call init_mat_EyeSys(MM,N,EyeSys) ! allocate the EyeSys matrices
   endif
!  test for compressed cabinet files
   file_idx=index(inputfile1, ".CAB")
   if ( file_idx .ne. 0 )  then
    allocate(character(nblines) :: cab_inputfile1)
    cab_inputfile1=inputfile1
    call execute_command_line ('cabextract ' // cab_inputfile1, exitstat=io)
    if (io == 0) then
     inputfile1=replacestr(string=inputfile1,search=".CAB",substitute=".DAT")
     file_idx=1+index(inputfile1, "/ED")
     inputfile1=inputfile1(file_idx:len(inputfile1))
    else
     write(*,*) 'Error opening cabinet file',inputfile1
     write(*,*) 'Make sure you have cab_extract installed and/or manually check/decompress the CAB file(s).'
     deallocate(cab_inputfile1)
     return
    endif
    allocate(character(nblines) :: cab_inputfile2)
    cab_inputfile2=inputfile2
    call execute_command_line ('cabextract ' // cab_inputfile2, exitstat=io)
    if (io == 0) then
     inputfile2=replacestr(string=inputfile2,search=".CAB",substitute=".DAT")
     file_idx=1+index(inputfile2, "/RA")
     inputfile2=inputfile2(file_idx:len(inputfile2))
    else
     write(*,*) 'Error opening cabinet file',inputfile2
     write(*,*) 'Make sure you have cab_extract installed and/or manually check/decompress the CAB file(s).'
     deallocate(cab_inputfile2)
     return
    endif
    inquire(file=trim(inputfile4), exist=exists)
    if (exists) then
     allocate(character(nblines) :: cab_inputfile4)
     cab_inputfile4=inputfile4
     call execute_command_line ('cabextract ' // cab_inputfile4, exitstat=io)
     if (io == 0) then
      inputfile4=replacestr(string=inputfile4,search=".CAB",substitute=".DAT")
      file_idx=1+index(inputfile4, "/PE")
      inputfile4=inputfile4(file_idx:len(inputfile4))
     else
      write(*,*) 'Error opening cabinet file',inputfile4
      write(*,*) 'Make sure you have cab_extract installed and/or manually check/decompress the CAB file(s).'
      deallocate(cab_inputfile4)
      return
     endif
    endif
   endif
   inquire(file=trim(inputfile4), exist=exists)
   if(.NOT.exists) then
    call RCNVRTN_binary(read_error,N,inputfile2,inputfile1)
   else
    call RCNVRTN_binary(read_error,N,inputfile2,inputfile1,inputfile4)
   endif
   if (read_error .eq. -1000) then
    write(*,*) 'FATAL error reading NIDEK file'
    return
   endif
   if (read_error == 0) then  ! ASCII
    EyeSys = 0 ! deallocate, then reallocate after charcount
 !  Only use on ASCII, might crash/give incorrect result on binary NIDEK
 !  Calculate number of mires by counting the floating point periods in the file, subtracting the header file extension, and dividing by 360
    periodcount=charcount(trim(inputfile2)//c_null_char)
    N=(periodcount-1)/360
    if (N .lt. 23 )then
     WRITE (*,*) 'Error on mire count in janus: ',N
     read_error=-1
     return
    endif
!   ASCII
    if(.not.allocated(EyeSys%RA)) then
     call init_mat_EyeSys(MM,N,EyeSys) ! allocate the EyeSys matrices
    else
     EyeSys = 0
     call init_mat_EyeSys(MM,N,EyeSys) ! allocate the EyeSys matrices
    endif
    inquire(file=trim(inputfile3), exist=exists)
    if(.NOT.exists) then
     call RCNVRTN(read_error,inputfile2,inputfile1)
    else
     inquire(file=trim(inputfile4), exist=exists)
     if(.NOT.exists) then
      call RCNVRTN(read_error,inputfile2,inputfile1,inputfile3)
     else
      call RCNVRTN(read_error,inputfile2,inputfile1,inputfile3,inputfile4)
     endif
    endif
   else
    if (N .ne. 39) then
     write(*,*) 'Mires set to ', N
     EyeSys = 0
     call init_mat_EyeSys(MM,N,EyeSys) ! reallocate the EyeSys matrices
     inquire(file=trim(inputfile4), exist=exists)
     if(.NOT.exists) then
      call RCNVRTN_binary(read_error,N,inputfile2,inputfile1)
     else
      call RCNVRTN_binary(read_error,N,inputfile2,inputfile1,inputfile4)
     endif
    endif
   endif
!  pupil conversion if any
   JMatrix%Pupil_Center=EyeSys%Pupil_Center*50.
   do i=1,MM/2
    JMatrix%PU(i)=(EyeSys%PU(2*i-1)+EyeSys%PU(2*i))*50.
   end do
   if(allocated(cab_inputfile1)) then
    call execute_command_line ('rm ' // inputfile1, exitstat=io)
    if (io > 0) then
     write (*,*) 'failed system command to remove tmp file',inputfile1
     read_error=12
    endif
    deallocate(cab_inputfile1)
   endif
   if(allocated(cab_inputfile2)) then
    call execute_command_line ('rm ' // inputfile2, exitstat=io)
    if (io > 0) then
     write (*,*) 'failed system command to remove tmp file',inputfile2
     read_error=12
    endif
    deallocate(cab_inputfile2)
   endif
   if(allocated(cab_inputfile4)) then
    call execute_command_line ('rm ' // inputfile4, exitstat=io)
    if (io > 0) then
     write (*,*) 'failed system command to remove tmp file',inputfile4
     read_error=12
    endif
    deallocate(cab_inputfile4)
   endif
   call CPU_TIME(time_end)
   write(*,*) 'Time to read Nidek files: ',(time_end-time_start)*1000
   if (read_error .ne. 0 .and. read_error .ne. 1 ) then
    err_janus=read_error*10
    return
   endif
  endif  !(mod(flag,100) = 0
  ! wipe RadSlope/DiaSlope clean to ensure the correct MM,N based on previous assignment
  if (allocated(RadSlope%r)) then
   RadSlope = 0 ; DiaSlope = 0 ; deallocate(RadSplineCenter)
   call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  else
   call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  endif
! Generate the slope matrix using ZFCT
  RadSlope=EyeSys
 endif

! READ THE ATLAS DATA
if (TestData .eq. 1) then
 MM=180
 if (mod(flag,100) == 0) then  !read the files
  read_error=0
  call CPU_TIME(time_start)
 ! determine the type, prior to allocating Atlas
  call RCNVRTA_type(inputfile1,Power_Rings_Count,read_error)
  inputfile2=inputfile1
  if (read_error .eq. 1) then
   write(*,*) 'Possible semicolon delimited Atlas file, try sed'
   if (index(inputfile1,".CSV") > 0) then
    inputfile2=replacestr(string=inputfile1,search=".CSV",substitute=".TMP")
!   write(*,*) 'sed "s/;/,/g" ' // inputfile1 // ' > ' // inputfile2
    call execute_command_line ('sed "s/;/,/g" ' // inputfile1 // ' > ' // inputfile2, exitstat=io)
   else
    write(*,*) 'No *.CSV file extension found'
    io = -1
   endif
   if (io /= 0) then
    write (*,*) 'system command to sed failed'
    write (*,*) 'Consider using your text editor to search/replace all semicolons with commas in',inputfile1
    write (*,*) 'sed also fails on pathnames with spaces'
    err_janus=11
    return
   else
    call RCNVRTA_type(inputfile2, Power_Rings_Count, read_error)
    if (read_error > 0) then
     write (*,*) 'temp Atlas file read error, probably not because semicolon delimited'
     file_idx=index(inputfile2, ".TMP")
     if (file_idx .ne. 0) then
      call execute_command_line ('rm ' // inputfile2, exitstat=io)
      if (io > 0) write (*,*) 'system command to remove tmp file failed'
     endif
     err_janus=read_error*100
     return
    endif
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
  call RCNVRTA(inputfile2,N,read_error)
  err_janus=read_error*100
  file_idx=index(inputfile2, ".TMP")
  if (file_idx .ne. 0) then
   call execute_command_line ('rm ' // inputfile2, exitstat=io)
   if (io > 0) write (*,*) 'system command to remove tmp file failed'
  endif
  call CPU_TIME(time_end)
  write(*,*) 'Time to read Atlas CSV file: ',(time_end-time_start)*1000
  if (read_error > 0) then
   err_janus=read_error*100
   return
  endif
  ! pupil conversion
   JMatrix%Pupil_Center=Atlas%Pupil_Center*100
   do i=1,MM
    X1=Atlas%PU(i,1)-Atlas%Pupil_Center(1)
    X2=Atlas%PU(i,2)-Atlas%Pupil_Center(2)
    JMatrix%PU(i)=sqrt(X1*X1+X2*X2)*100
   end do
 endif  !(mod(flag,100) /= 0,99,2,3 assume 1 (zernike) or 4 (redraw), or 8 (show rings) reload the original data 
 N=Power_Rings_Count
 ! wipe RadSlope/DiaSlope clean to ensure the correct MM,N based on previous assignment
 if (allocated(RadSlope%r)) then
  RadSlope = 0 ; DiaSlope = 0 ; deallocate(RadSplineCenter)
  call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
 else
  call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
 endif
 RadSlope=Atlas
endif ! end (TestData == 1)

! READ THE PENTACAM DATA
 if (TestData .ge. 2 .AND. TestData .le. 5) then
  call CPU_TIME(time_start)
  MM=180; N=22; NP=141   ! PentaCam
! ELE are elevations, CUR are "sagittal" curvatures in a 141x141 -7 to 7 mm square -1 is no data
! _ELE.CSV or _CUR.CSV versions have less text but use semicolons (;) instead of -1
  if (mod(flag,100) == 0) then ! read the files
  if(.not.allocated(Penta%DAT)) then
   call init_mat_Penta(NP,Penta,Skyline)   !allocate the PentaCam matices
  else
   Penta=0 ; Skyline=0
   call init_mat_Penta(NP,Penta,Skyline)   !allocate the PentaCam matices
  endif
   read_error=0
   if (TestData .eq. 3 .or. TestData .eq. 5) then
    call RCNVRTP(TestData,inputfile2,read_error)  !curvatures
   else
    call RCNVRTP(TestData,inputfile1,read_error)  !elevations TestData .eq. 2 .or. TestData .eq. 4
   endif
   if (read_error > 0) then
    err_janus=read_error*1000
    return
   endif
   ! pupil conversion, could do whole circular spline here
    JMatrix%Pupil_Center=Penta%Pupil_Center/10.
    do i=1,MM
     j=floor(1.+(i-1)*255/179.0)
     X1=Penta%PU(j,1)-Penta%Pupil_Center(1)
     X2=Penta%PU(j,2)-Penta%Pupil_Center(2)
     JMatrix%PU(i)=sqrt(X1*X1+X2*X2)/10.
    end do
  endif  !(mod(flag,100) /= 0,99,2,3 must be 1 or 4-7, reload the original data
! wipe RadSlope/DiaSlope clean to ensure the correct MM,N based on previous assignment
  if (allocated(RadSlope%r)) then
   RadSlope = 0 ; DiaSlope = 0 ; deallocate(RadSplineCenter)
   call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  else
   call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  endif
  ! Easiest decentering for Penta elevation files is by shifting the Penta data; limited to 0.1 mm increments
  if (.false.) then !disabled
  ! if (btest(dat,5)) then
   new_path = " "
   do i=1, 4096
      if ( file_from_C (i) == c_null_char ) then
          exit
      else
          new_path (i:i) = file_from_C (i)
      end if
   end do
  ! write(*,*) 'file from kernunos: ',trim(new_path)
   new_path=trim(new_path)
   read(new_path,*) dvert,dhoriz
   if (TestData .eq. 2 .or. Testdata .eq. 4 .and. allocated(Penta%DAT) ) then
    if(.not.allocated(temp)) then
     allocate(temp(NP,NP))
    endif
    do i=1,NP
     do k=1,NP
      temp(1+modulo(i-1+int(10*dhoriz),NP),1+modulo(k-1+int(10*dvert),NP))=Penta%DAT(i,k)
     end do
    end do
    do k=1,NP
     Penta%DAT(:,k)=temp(:,k)
    end do
    deallocate(temp)
   endif
   do i=1,MM
    j=floor(1.+(i-1)*255/179.0)
    X1=Penta%PU(j,1)-dhoriz
    X2=Penta%PU(j,2)-dvert
    JMatrix%PU(i)=sqrt(X1*X1+X2*X2)/10.
   end do
   endif
! arrange the data
  Skyline=Penta
  lsq = btest(dat,9)
! convert to polar with splining; makes round rings as above with 180x22 - also already has either center value Z0(1) or SAGC0(1)
! RadSlope_eq_Skyline puts elevation into JMatrix%Z(j,i) and possibly populates JMatrix%Z(j,i) with crap
  call RadSlope_eq_Skyline(lsq,JMatrix, RadSlope, Skyline, Penta)  !needs Penta for border check populates RadSlope with ZFCT
  call CPU_TIME(time_end)
  write(*,*) 'Time to convert Penta: ',(time_end-time_start)*1000
  if (TestData.eq.2 .or. TestData.eq.4) then ! ELE or ELE.CSV PentaCam files, put elevation into Zp for splining without integration
!   RadSlope%Zp(:,:)=0 ; JMatrix%SAGC(:,:) = 0 ; JMatrix%SAGC0(:) = 0 ! ELE should not have anything in Zp or SAGC yet
   do i=1,MM
    do j=1,RadSlope%MV(i)
      RadSlope%Zp(j,i)=JMatrix%Z(j,i) !=RadSlope%Z(j,i) ! at this point
    end do
   end do
  endif
!  JMatrix%Z(:,:) = 0
 endif

! OR GENERATE Fake EyeSys data
if (TestData .lt. 0) then
 MM=360; N=16   ! fake EyeSys
 if(.not.allocated(EyeSys%RA)) then
  call init_mat_EyeSys(MM,N,EyeSys) ! allocate the EyeSys matrices
 endif
! wipe RadSlope/DiaSlope clean to ensure the correct MM,N based on previous assignment
  if (allocated(RadSlope%r)) then
   RadSlope = 0 ; DiaSlope = 0 ; deallocate(RadSplineCenter)
   call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  else
   call init_mat(MM,N,RadSlope,DiaSlope,RadSplineCenter)  ! allocate the common arrays
  endif
  if (mod(flag,100) == 0) then !read the files
   call RCNVRTT(MM,N)
  endif
! Generate the slope matrix
  RadSlope=EyeSys
 endif

! fillin tweaks
!  FILL IN MISSING ATLAS DATA
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
  call Atlas_SplineFillin(AtlasSave,AtlasSave%AP,Atlas%AP)
  call Atlas_SplineFillin(AtlasSave,AtlasSave%AD,Atlas%AD)
  call Atlas_SplineFillin(AtlasSave,AtlasSave%AY,Atlas%AY)
 endif
!  FILL IN MISSING ATLAS RING DATA USING LSQ Fourier series
 if (btest(dat, 3)) then
  AtlasSave=Atlas
  call Atlas_LSQFillin(AtlasSave,AtlasSave%AP,Atlas%AP)
  call Atlas_LSQFillin(AtlasSave,AtlasSave%AD,Atlas%AD)
  call Atlas_LSQFillin(AtlasSave,AtlasSave%AY,Atlas%AY)
 endif

 ! gnuplot rings output and exit
  if (mod(flag,100) .eq. 8 ) then
 ! generate data file
   unitno1 = get_new_fileunit()
   BigPlot=replacestr(string=gnu_instruct,search=".gnu",substitute=".plt")
   open(unitno1, file = BigPlot, action="write", iostat=ierr)
 ! Look at these rings using gnuplot set polar
 ! gnuplot 'plot 'datafile dumped with' u 1:2'
    do j=1,size(Atlas%AP,2)
     do i=1,M1
      if ((Atlas%AP(i,j) > 0) .AND. (Atlas%AR(i,j) > 0) .AND. (Atlas%AD(i,j) > 0) .AND. (Atlas%AY(i,j) > 0)) then    ! Only for Atlas with valid data /= 0
       write(unitno1,*) PI*(i-1)/90.0_wp,Atlas%AD(i,j)
      endif
     end do
    end do
    CLOSE (unitno1)
    if (btest(dat, 4) .or. btest(dat, 3)) then
     RadSlope=Atlas
     Atlas=AtlasSave  ! restore Atlas after using it to show rings
    endif
    return
   endif ! end (mod(flag,100) .eq. 8)

 if (btest(dat, 4) .or. btest(dat, 3)) then
  RadSlope=Atlas
  Atlas=AtlasSave  ! restore Atlas after using it to define RadSlope
 endif
endif  !Atlas fillin

! FILL IN MISSING EYESYS/NIDEK DATA, only use sple, LSQ has problem with the one broken EyeSys file I've seen
! probably because I'm making RA and XX now powers, get duplicate radii and slopes at the end points, which break nspline
if (TestData .eq. 0 .or. TestData .eq. 6) then
 if (btest(dat, 4) .or. btest(dat, 3)) then
! make sure I have a backup of EyeSys the same size as EyeSys before fillin
  if(allocated(EyeSysSave%RA)) EyeSysSave=0
  N=size(EyeSys%RA,2)
  MM=size(EyeSys%RA,1)
  call init_mat_EyeSys(MM,N,EyeSysSave)
 endif
 if (btest(dat, 4)) then
  EyeSysSave=EyeSys
  call EyeSys_SplineFillin(EyeSysSave,EyeSysSave%RA,EyeSys%RA)
  call EyeSys_SplineFillin(EyeSysSave,EyeSysSave%XX,EyeSys%XX)
  inquire(file=trim(inputfile3), exist=exists)
  if(exists .and. TestData .eq. 6) call EyeSys_SplineFillin(EyeSysSave,EyeSysSave%HT,EyeSys%HT)
 endif
! FILL IN MISSING EYESYS RING DATA USING LSQ Fourier series
  if (btest(dat, 3)) then
   EyeSysSave=EyeSys
   call EyeSys_LSQFillin(EyeSysSave,EyeSysSave%RA,EyeSys%RA)
   call EyeSys_LSQFillin(EyeSysSave,EyeSysSave%XX,EyeSys%XX)
   inquire(file=trim(inputfile3), exist=exists)
   if(exists .and. TestData .eq. 6) call EyeSys_LSQFillin(EyeSysSave,EyeSysSave%HT,EyeSys%HT)
  endif

 ! gnuplot rings output and exit
  if (mod(flag,100) .eq. 8 ) then
 ! generate data file
   unitno1 = get_new_fileunit()
   BigPlot=replacestr(string=gnu_instruct,search=".gnu",substitute=".plt")
   open(unitno1, file = BigPlot, action="write", iostat=ierr)
 ! Look at these rings using gnuplot set polar
 ! gnuplot 'plot 'datafile dumped with' u 1:2'
    do j=1,size(EyeSys%RA,2)
     do i=1,M1
     if ((EyeSys%RA(i,j) > 0) .AND. (EyeSys%XX(i,j) > 0) ) then ! Only for EyeSys with valid data /= 0
       write(unitno1,*) PI*(i-1)/180.0_wp,EyeSys%RA(i,j)
      endif
     end do
    end do
    CLOSE (unitno1)
    if (btest(dat, 4) .or. btest(dat, 3)) then
     RadSlope=EyeSys
     EyeSys=EyeSysSave  ! restore EyeSys after using it to show rings
    endif
    return
   endif ! end (mod(flag,100) .eq. 8)
 if (btest(dat, 4) .or. btest(dat, 3)) then
  RadSlope=EyeSys
  EyeSys=EyeSysSave  ! restore EyeSys after using it to define RadSlope
 endif
endif  ! EyeSys fillin

! skip all this if we're just displaying zernike coefficients again
if (mod(flag,100) .ne. 9 ) then  ! Spline RadSlope
 DiaSlope=RadSlope              ! move to diagonal format
 DiaSlope%Zpd2 = .n. DiaSlope
 call MakeRadSplineCenter(zero_int64,error_report)   ! remakes RadSplineCenter(1,:)
 if (error_report .ne. 0) then
  write(*,*)' janus line number: ',__LINE__
 endif
 if (TestData.eq.3 .or. TestData.eq.5 ) RadSplineCenter(1,:)=0   ! pentacam data provided
 if (TestData.eq.2 .or. TestData.eq.4 ) RadSplineCenter(1,:)=0
 if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
  DiaSlope%Zpd2 = .nc. DiaSlope ! re-spline, with center node
 endif
 if (btest(dat, 1)) then          ! moving each meridian to align curves
  call AdjustRadSplineCenter     ! changes r only
  DiaSlope%Zpd2 = .n. DiaSlope    ! re-spline, standard
 endif
 call MakeRadSplineCenter(dat,error_report)        ! generates spline centers with tweaks
 if (error_report .ne. 0) then
  write(*,*)' janus line number: ',__LINE__
 endif
! Atlas spline consistency check and computation of elevation by power vs elevation in file
 if ( Testdata .eq. 1 .and. btest(dat,7) ) then
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
 ! lsq instead of circumferential spline do not want to do for spline test
 ! if (btest(dat, 8)) then
 ! iflag =iflag+100
 ! endif
  k=0 ; powmax2 = 0 ; powmax =0  ! Use these temporarily
 ! find max elevation from Atlas file
  do i=1,M1
   do j=1,RadSlope%MV(i)
    if (100*Atlas%AY(i,j) > powmax) powmax=100*Atlas%AY(i,j)
    end do
   end do
 ! check spline power & elevation at knots
   do i=1,M1
    do j=1,RadSlope%MV(i)
     call SplineEval1Dx1D(iflag,100*Atlas%AD(i,j),PI*(i-1)/90.0_wp,Y,YPR,YP2R2,YPTHETA,YPRTHETA,YP2THETA)
     call AXIALP(ABS(100*Atlas%AD(i,j)),ABS(YPR),YP2R2,pow)
!   skip missing elevation points to compute (cumulative) average error
    if (Atlas%AY(i,j) > 0) then
      k=k+1
      powmax2=powmax2+ABS(Y-powmax+100*Atlas%AY(i,j))
     endif
!   checks that power at knots is correct at knots; since this is a spline without LSQ it should be.
     if (ABS(Atlas%AP(i,j)-pow) > EPS .and. (Atlas%AP(i,j) .gt. 0) .and. (Atlas%AD(i,j) .gt. 0) .and. (Atlas%AY(i,j) .gt. 0) .AND. (Atlas%AR(i,j) > 0)) then
      write(*,*) 'Atlas power spline error in janus: ',j,i,Atlas%AP(i,j),pow
     endif
    end do
   end do
   write(*,*) 'Atlas avg abs elevation percent error : ',(100*powmax2/k)/powmax
 endif

! NIDEK spline consistency computation of elevation by power calc by slope vs elevation in file HT
 if ( Testdata .eq. 6 .and. btest(dat,7) ) then
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
  k=0 ; powmax2 = 0 ; powmax =0  ! Use these temporarily
 ! find max elevation from Nidek file
  do i=1,M1
   do j=1,RadSlope%MV(i)
    if (EyeSys%HT(i,j) > powmax) powmax=EyeSys%HT(i,j)
    end do
   end do
 ! check spline power & elevation at knots
   do i=1,M1
    do j=1,RadSlope%MV(i)
    call SplineEval1Dx1D(iflag,EyeSys%RA(i,j),PI*(i-1)/90.0_wp,Y,YPR,YP2R2,YPTHETA,YPRTHETA,YP2THETA)
!   skip missing elevation points to compute (cumulative) average error
    if (EyeSys%HT(i,j) > 0) then
      k=k+1
      powmax2=powmax2+ABS(100*(Y-EyeSys%HT(i,j))/EyeSys%HT(i,j))
!     EyeSys%HT data has few significant digits, is quite flat and deviates more in the center rings
!     skip the two center rings and show errors greater tha 5%
      if (ABS(100*(Y-EyeSys%HT(i,j))/EyeSys%HT(i,j)) > 5.0 .and. j > 2 ) then
       write(*,*) j,(i-1),Y,EyeSys%HT(i,j),100*(Y-EyeSys%HT(i,j))/EyeSys%HT(i,j)
      endif
     endif
    end do
   end do
   if (k .gt. 1) then
    write(*,*) 'NIDEK avg abs elevation percent error : ',powmax2/k
   else
    write(*,*) 'No NIDEK elevation data (Hint: no HT file)'
   endif
 endif

! Keratograph spline consistency computation of elevation by power calc by Power in file CURVAT vs elevation in file CORNEA
! also can check INSTC calculated vs supplied, SAGC is regarded as primary given placido disk technology
  if ( Testdata .eq. 7 .and. btest(dat,7) ) then
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
   k=0 ; powmax2 = 0 ; powmax =0  ! Use these temporarily
  ! check spline power & elevation at knots
    do i=1,MM
     do j=1,RadSlope%MV(i)
     call SplineEval1Dx1D(iflag,100*Oculus%Y(i,j),PI*Oculus%SEG(i)/9000.0_wp,Y,YPR,YP2R2,YPTHETA,YPRTHETA,YP2THETA)
 !   skip missing elevation points to compute average error
     if (Oculus%ELE(i,j) > 0) then
       k=k+1
       powmax=powmax+ABS(100*(YP2R2-RadSlope%zp2(j,i))/RadSlope%zp2(j,i))
       powmax2=powmax2+ABS(100*(Y/100.0-Oculus%ELE(i,j))/Oculus%ELE(i,j))
 !     Oculus%ELE data has few significant digits, is quite flat and deviates more in the center rings
 !     skip the two center rings and show errors greater tha 5%
       if (ABS(100*(Y/100.0-Oculus%ELE(i,j))/Oculus%ELE(i,j)) > 5.0 .and. j > 2 ) then
 !       write(*,*) j,(i-1),Y,100*Oculus%ELE(i,j),100*(Y/100.0-Oculus%ELE(i,j))/Oculus%ELE(i,j)
       endif
       if (ABS(100*(YP2R2-RadSlope%zp2(j,i))/RadSlope%zp2(j,i)) > 5.0 .and. j > 2 ) then
 !       write(*,*) j,(i-1),YP2R2,RadSlope%zp2(j,i),ABS(100*(YP2R2-RadSlope%zp2(j,i))/RadSlope%zp2(j,i))
       endif
      endif
     end do
    end do
    if (k .gt. 1) then
     write(*,*) 'KERATOGRAPH avg abs elevation percent error : ',powmax2/k
     write(*,*) 'KERATOGRAPH avg abs curvature percent error : ',powmax/k
    else
     write(*,*) 'No KERATOGRAPH elevation data (Hint: no CORNEA file)'
    endif
  endif

! Make JMatrix

! set iflag for slope based data, integrate based on iflag with or without cubic/trapez or center point or not for values
 if (TestData.ne.2 .and. TestData.ne.4) then   !not ELE files
  if (btest(dat, 2)) then   ! cubic integration
   if (btest(dat,0)) then   !centernode
    iflag=12
    else
    iflag=2
    endif
   else         !trapezoidal integration
    if (btest(dat,0)) then
     iflag=11
    else
     iflag=1
    endif
   endif
  else                      !ELE files
   if (btest(dat,0)) then   !centernode
    iflag=10
   else
    iflag=0
   endif
  endif
! lsq instead of circumferential spline
   if (btest(dat, 8)) then
    iflag = iflag+100
   endif
! make round rings and if needed convert 360x16 or 180x25 or 100x60 to 180x22
! donut
! Find maximum radius from data in RadSlope
  rBo = 0
  do i=1,MM
   j=RadSlope%MV(i)
   if (ABS(RadSlope%r(j,i)) >= rBo) rBo=ABS(RadSlope%r(j,i))
  end do
! extrapolated keratogrpah data needs trimming at edges
  if (index(inputfile1, "_F") .gt. 0) then
   rBo=0.80*rBo
  endif
  rBi=0.05*rBo
!  min and max bounds
  JMatrix%SAGC0(2)=1E30   ;  JMatrix%SAGC0(3)=-1E30
  JMatrix%Warp0(2)=1E30   ;  JMatrix%Warp0(3)=-1E30 ; JMatrix%Warp0(1)=0
  JMatrix%Z0(2)=1E30      ;  JMatrix%Z0(3)=-1E30
  JMatrix%INSTC0(2)=1E30  ;  JMatrix%INSTC0(3)=-1E30 ; JMatrix%INSTC0(1)=0
  JMatrix%GAUSSC0(2)=1E30 ;  JMatrix%GAUSSC0(3)=-1E30 ; JMatrix%GAUSSC0(1)=0
  JMatrix%MEANC0(2)=1E30  ;  JMatrix%MEANC0(3)=-1E30 ; JMatrix%MEANC0(1)=0
  JMatrix%MONGEA0(2)=1E30 ;  JMatrix%MONGEA0(3)=-1E30 ; JMatrix%MONGEA0(1)=0
  JMatrix%R0=0 ; JMatrix%THT0=0
! determine the boundary
  do i=1,M1                             ! every 2 degrees
   JMatrix%THT(i)=PI*(i-1)/90.0_wp
   JMatrix%MV(i)=N1
   j=N1
   if (MM == 360) then  ! original EyeSys RadSlope or fake data, every degree
    ii=2*i   ! MM =360
   else
    if (MM == 100) then ! Oculus every 4 grads
     ii=max(int(5*i/9),1)   ! MM =100
    else
     ii=i   ! MM = 180
    endif
   endif
   R_TST=ABS(RadSlope%r(RadSlope%MV(ii),ii))
   if (R_TST > 0) then
   R_MV=rBo
    do while (R_MV .gt. R_TST)
     JMatrix%MV(i)=JMatrix%MV(i)-1
     j=j-1
     R_MV=(rBi+(j-1)*(rBo-rBi)/(N1-1))
    end do
   else
    R_MV=-rBo
    do while (R_MV .lt. R_TST)
     JMatrix%MV(i)=JMatrix%MV(i)-1
     j=j-1
     R_MV=-(rBi+(j-1)*(rBo-rBi)/(N1-1))
    end do
   endif
  end do
  if (abs(crop) .gt. 6) crop = 0   !safety setting
  if (.not. allocated(MV)) then
   allocate(MV(M1))
  else
   deallocate(MV)
   allocate(MV(M1))
  endif
!  store
  MV(:)=JMatrix%MV(:)
  JMatrix%MV(:)=min(JMatrix%MV(:)-crop,MV(:))

! Generate the surface
  do i=1,M1
   do j=1,JMatrix%MV(i) ! does not include center point
    JMatrix%R(j,i)=(rBi+(j-1)*(rBo-rBi)/(N1-1))
!   generate elevations and derivatives; iflag no integration if .ELE .ELE_CSV file
    call SplineEval1Dx1D(iflag,JMatrix%R(j,i),JMatrix%THT(i),JMatrix%Z(j,i),YPR,YP2R2,YPTHETA,YPRTHETA,YP2THETA)
!   save for vertex normals and for LIOC
    JMatrix%YPR(j,i)=YPR
    JMatrix%YPTHETA(j,i)=YPTHETA
!   powers
    call AXIALP(JMatrix%R(j,i),YPTHETA/JMatrix%R(j,i),YP2THETA/JMatrix%R(j,i),JMatrix%Warp(j,i))
!   not elevations
    if (btest(dat,10)) then
     call axisymmetric_principal(JMatrix%R(j,i),YPR,YP2R2,gaussian,meanpower,princ1,princ2,astigm)
    else
     call principal(JMatrix%THT(i),JMatrix%R(j,i),YPR,YPTHETA,YPRTHETA,YP2THETA,YP2R2,gaussian,meanpower,princ1,princ2,astigm)
    endif
    JMatrix%MONGEA(j,i)=RFCT*astigm
    JMatrix%MEANC(j,i)=RFCT*meanpower
    JMatrix%INSTC(j,i)=RFCT*princ1
    JMatrix%SAGC(j,i)=RFCT*princ2
    JMatrix%GAUSSC(j,i)=RFCT*gaussian


!!!!!check here for extrapolation with XX; yep still doing it; its at the edge where the data is discontinuous circumferentially

if (abs(YP2THETA) .gt. 2000) then

write(*,*) ABS(JMatrix%R(j,i))*COS(JMatrix%THT(i)),ABS(JMatrix%R(j,i))*SIN(JMatrix%THT(i)),JMatrix%THT(i),JMatrix%Z(j,i),YPR,YP2R2,YPTHETA,YPRTHETA,YP2THETA,ABS(JMatrix%R(j,i))
!nb only for mm=60
! needs modification for oculus
if (i .gt. M1/2) then
 ii=2*(i-M1/2)
else
 ii=2*i
endif
write(*,*) 'abs(YP2THETA) .gt. 2000'
!write(*,*) RadSlope%r(1:N,ii)
write(*,*) j,i,ii
!write(*,*) DiaSlope%rd(1:2*N,ii/2)

endif

!write(*,*) ' '
!call principal(JMatrix%THT(i),JMatrix%R(j,i),YPR,YPTHETA,YPRTHETA,YP2THETA,YP2R2,gaussian,meanpower,princ1,princ2,astigm)
!write(*,*) '2',RFCT*meanpower,RFCT*astigm,RFCT*princ1,RFCT*princ2,RFCT*gaussian

!call axisymmetric_principal(JMatrix%R(j,i),YPR,YP2R2,gaussian,meanpower,princ1,princ2,astigm)
!write(*,*) '3',RFCT*meanpower,RFCT*astigm,RFCT*princ1,RFCT*princ2,RFCT*gaussian

!    if (M1 .eq. 360) then
!     k=8 ! skip this many problematic values at x-axis
!    else
!     k=4  ! fewer because every 2 degrees
!    endif
!    if ( (i .gt. (1+k) .and. i .lt. (M1/2-k)) .or. (i .lt. (M1-k)  .and. i .gt. (M1/2+k))) then
!     call MONGEA(JMatrix%THT(i),JMatrix%R(j,i),ABS(YPR),YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MONGEA(j,i))
!     call MEANP(JMatrix%THT(i),JMatrix%R(j,i),ABS(YPR),YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MEANC(j,i))
!    endif


   end do
  end do !end JMatrix ring generation

! spline over problematic limits at x-axis
!  JMatrix%MEANC=splinefillintranspose(JMatrix%MEANC)
!  JMatrix%MONGEA=splinefillintranspose(JMatrix%MONGEA)

! make RadSplineCenter
  RadSlope=0
  DiaSlope=0
  deallocate(RadSplineCenter)
! reinitialize with M1 and N1
  call init_mat(M1,N1,RadSlope,DiaSlope,RadSplineCenter)
  RadSlope=JMatrix
  DiaSlope=RadSlope              ! move to diagonal format
  if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
   DiaSlope%Zpd2 = .nc. DiaSlope ! re-spline, with center node
  else
   DiaSlope%Zpd2 = .n. DiaSlope
  endif
  if ( Testdata .le.1 .or. Testdata .eq. 6) then     ! not for Oculus PentaCam or Keratograph
   call MakeRadSplineCenter(zero_int64,error_report)   ! remakes RadSplineCenter(1,:)
   if (error_report .ne. 0) then
    write(*,*)' MakeRadSpline Error janus line number: ',__LINE__
   endif
  else
   RadSplineCenter(1,:)=0      ! pentacam and keratograph by definition is at 0
  endif
  if (btest(dat, 1)) then          ! moving each meridian to align curves
   call AdjustRadSplineCenter     ! changes r only
   DiaSlope%Zpd2 = .n. DiaSlope    ! re-spline, standard
  endif
  call MakeRadSplineCenter(dat,error_report)        ! generates spline centers with tweaks
  if (error_report .ne. 0) then
   write(*,*)' janus line number: ',__LINE__
  endif
! Calculate center values for everything
! These have MM different values of the center!
  call centersJMatrix(JMatrix,TestData,dat,iflag,cardinal,nC)
! find min max of everything
  call minmax(JMatrix)
! end populating JMatrix

! Penta file(s) consistency check ELE vs. matching CUR
! simple difference/subtraction with compare for elevation consistency
 if (TestData .ge. 2 .AND. TestData .le. 5 .and. btest(dat,7) .and. exists) then
 k=0 ; powmax2 = 0 ; powmax =0  ! Use these temporarily
! find max elevation from JMatrix
  do i=1,M1
   do j=1,RadSlope%MV(i)
   if (JMatrix%Z(j,i) > powmax) powmax=JMatrix%Z(j,i)
   end do
  end do
! cumulative addition of differences
  do i=1,M1
   do j=1,RadSlope%MV(i)
    if (JMatrix%Z(j,i) > 0 .and. JMatrix1%Z(j,i) > 0) then
     k=k+1
     powmax2=powmax2+ABS(JMatrix1%Z(j,i)-JMatrix%Z(j,i))
    endif
   end do
  end do
  write(*,*) 'Penta avg abs elevation percent error : ',(100*powmax2/k)/powmax
 endif

endif !mod(flag,100) /= 9

!zernike coefficents
if (mod(flag,100) == 1) then
 call Ccounter(0,"zernike.tmp"//c_null_char)
 call LogC("Starting zernike computation"//c_null_char)
! relies on saved MM,N
 nrhs=(M1*N1+1)

  if (allocated(JMatrix%R)) then
! Try to generate zernike coefficients based on central elevations & lsq to zernike polynomials
!  call CPU_TIME(time_start)
  time_start=omp_get_wtime()
  RadSlope=JMatrix

  DiaSlope=RadSlope              ! move to diagonal format
  DiaSlope%Zpd2 = .n. DiaSlope
 if ( Testdata .le.1 ) then     ! only for test/Atlas/EyeSys at present
   call MakeRadSplineCenter(zero_int64,error_report)   ! remakes RadSplineCenter(1,:)
   if (error_report .ne. 0) then
    write(*,*)' janus line number: ',__LINE__
   endif
  else
   RadSplineCenter(1,:)=0      ! pentacam by definition is at 0
  endif
  if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
   DiaSlope%Zpd2 = .nc. DiaSlope ! re-spline, with center node
  endif
  if (btest(dat, 1)) then          ! moving each meridian to align curves
   call AdjustRadSplineCenter     ! changes r only
   DiaSlope%Zpd2 = .n. DiaSlope    ! re-spline, standard
  endif
  call MakeRadSplineCenter(dat,error_report)        ! generates spline centers with tweaks
    write(*,*) dat,error_report,__LINE__
  if (error_report .ne. 0) then
   write(*,*)' janus line number: ',__LINE__
  endif
! reset iflag for generating local elevations for computations
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
! lsq instead of circumferential spline
if (btest(dat, 8)) then
 iflag =iflag+100
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
    err_janus=-2
    return
   endif
   zernC=0

   do ii=1,nrhs
   call Ccounter(ii/40,"zernike.tmp"//c_null_char)
!  cycle through i1 1 to MM and j1 4 to N-3 with one point for origin at nrhs
   i1=mod(ii,M1)
   j1=int(ii/M1)+1
   if (i1 .eq. 0) then
    i1=M1
    j1=j1-1
   endif

!  center of local geometry is ctr_circle_x, ctr_circle_y, add 4 to stay outside center, sub 4 to stay inside edge
   if (ii .lt. nrhs ) then
    if (j1 .eq. 1) then
     ctr_circle_x=(4+abs(JMatrix%R(j1,i1)))*cos(JMatrix%THT(i1))
     ctr_circle_y=(4+abs(JMatrix%R(j1,i1)))*sin(JMatrix%THT(i1))
    endif
    if (j1 .ge. JMatrix%MV(i1)) then
     ctr_circle_x=(-4+abs(JMatrix%R(j1,i1)))*cos(JMatrix%THT(i1))
     ctr_circle_y=(-4+abs(JMatrix%R(j1,i1)))*sin(JMatrix%THT(i1))
    endif
    if (j1 .gt. 1 .and. j1 .lt. JMatrix%MV(i1)) then
     ctr_circle_x=(abs(JMatrix%R(j1,i1)))*cos(JMatrix%THT(i1))
     ctr_circle_y=(abs(JMatrix%R(j1,i1)))*sin(JMatrix%THT(i1))
    endif
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
    call PolarTranslate(ctr_circle_x,ctr_circle_y,rlocal(kk),thtlocal(kk),R_global,Theta_global)
    call SplineEval1Dx1D(iflag,R_global,Theta_global,zernC(kk,ii))
    end do
   end do   
  end do  ! end ii to nrhs

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
! only have to call this once; NRHS can be for the whole plot since B_Matrix is invariant.
! have to allocate XTX,EE,IPIV for DGESV, XTX,EE for G-J
!   allocate(XTX(k_max,k_max),EE(k_max,nrhs),IPIV(k_max),stat=ierr)
!   if (ierr /= 0) then
!    write(*,*) 'unable to allocate memory in zernike for GJ '
!    return
!   endif
!   XTX=matmul(B_matrix,Transpose(B_matrix))
!   EE=matmul(B_matrix,zernC)  ! with a second dimension for EE
!   call DGESV(k_max,nrhs,XTX,k_max,IPIV,EE,k_max,INFO) ! overwrites EE into solution
!   call GaussJordan(k_max,nrhs,XTX,k_max,EE,k_max,INFO )  ! overwrites EE into solution
!!  have to allocate WORK for DGELS, to use these uncomment them in declarations too
  LWORK = min(k_max,kk_max) + max( min(k_max,kk_max), nrhs )
  allocate (WORK(LWORK),stat=ierr) ! WORK is dimension LWORK
  if (ierr /= 0) then
   write(*,*) 'unable to allocate memory in zernike for WORK '
   err_janus=-2
   return
  endif
  call DGELS( 'T', k_max, kk_max, nrhs, B_Matrix, k_max, zernC , kk_max, WORK, LWORK, INFO ) ! overwrites zernC (only to k_max)
!! if not using DGELS have to replace zernC below with EE ie  zernC(1:k_max,kk) => EE(1:k_max,kk) as in commented lines

call LogC("post-LSQ"//c_null_char)

!$OMP PARALLEL DO PRIVATE(i1,j1,i,j,kk)
do kk=1,nrhs
! cycle through i1 1 to MM and j1 1 to N with one point for origin at N+1
  i1=mod(kk,M1)
  j1=int(kk/M1)+1
  if (i1 .eq. 0) then
   i1=M1
   j1=j1-1
  endif
!  JMatrix%ZC(j1,i1,1:k_max)=1000*EE(1:k_max,kk)
  JMatrix%ZC(j1,i1,1:k_max)=1000*zernC(1:k_max,kk)
end do
!$OMP END PARALLEL DO

! zero out the values near the x-axis
do i=1,M1                             ! every 2 degrees
 do j=1,JMatrix%MV(i)
  k=3 ! skip these problematic values at x-axis
  if ( (i .gt. (1+k) .and. i .lt. (M1/2-k)) .or. (i .lt. (M1-k)  .and. i .gt. (M1/2+k)) ) then
!  keep the computed value
  else
  do kk = 1,15
   JMatrix%ZC(j,i,kk)=0
  end do
  endif
 end do
end do
! Use pspli to spline over x-axis
do kk = 1,15
 JMatrix%ZC(:,:,kk)=splinefillintranspose(JMatrix%ZC(:,:,kk))
end do

! center values are the last values at nrhs
do k=1,15
! JMatrix%ZC0(1,k)=1000*EE(k,nrhs)
 JMatrix%ZC0(1,k)=1000*zernC(k,nrhs)
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

!write(*,*) 'center zernike values: ',EE(1:k_max,nrhs)
write(*,*) 'center zernike values: ',zernC(1:k_max,nrhs)
call LogC("Finished zernike"//c_null_char)

! deallocate(XTX,EE,IPIV)  !if used above
deallocate(WORK,B_Matrix)
deallocate(zernC,rlocal,thtlocal)

!  call CPU_TIME(time_end)
  time_end=omp_get_wtime()
 write(*,*) 'Time to compute zernike: ',(time_end-time_start)
 else
 call LogC("Have to open a file prior to computing zernike"//c_null_char)
 err_janus=9
 return ! if last digits of flag==1 and not allocated do nothing
 endif
endif  ! end of flag=1

! plot Zernike central coefficients with gnuplot
if (mod(flag,100) == 1 .or. mod(flag,100) == 9) then
! transfer to C++ for plot
! find min and max of first 12 central aberrations for plot
! 15    "Z(4,4) Vertical Quatrafoil",  Quadrafoil 0 deg
! 13    "Z(4,2) Vertical 2nd Astig.",  4th order astigmatism 0 deg
! 09    "Z(4,0) Spherical Aberration", Spherical Aberration
! 04    "Z(4,-2) Oblique 2nd Astig.",  4th order astigmatism 45 deg
! 01    "Z(4,-4) Oblique Quatrafoil",  Quadrafoil 22.5 deg
! 14    "Z(3,3) Oblique Trefoil",      Trefoil 0 deg
! 11    "Z(3,1) Horizontal Coma",      Coma 0 deg
! 06    "Z(3,-1) Vertical Coma",       Coma 90 deg
! 02    "Z(3,-3) Vertical Trefoil",    Trefoil 30 deg
!      LOA
! 12    "Z(2,2) Vertical Astig.",      Astigmatism 0 deg
! 08    "Z(2,0) Defocus",              Defocus
! 03    "Z(2,-2) Oblique Astigmatism", Astigmatism 45 deg
zern(1)=real(JMatrix%ZC0(1,15),kind=sk)
zern(2)=real(JMatrix%ZC0(1,13),kind=sk)
zern(3)=real(JMatrix%ZC0(1,9),kind=sk)
zern(4)=real(JMatrix%ZC0(1,4),kind=sk)
zern(5)=real(JMatrix%ZC0(1,1),kind=sk)
zern(6)=real(JMatrix%ZC0(1,14),kind=sk)
zern(7)=real(JMatrix%ZC0(1,11),kind=sk)
zern(8)=real(JMatrix%ZC0(1,6),kind=sk)
zern(9)=real(JMatrix%ZC0(1,2),kind=sk)
zern(10)=real(JMatrix%ZC0(1,12),kind=sk)
zern(11)=real(JMatrix%ZC0(1,8),kind=sk)
zern(12)=real(JMatrix%ZC0(1,3),kind=sk)
zern(13)=1E30
zern(14)=-1E30
do k=1,12
 if (zern(13) >= zern(k)) zern(13) = zern(k)
 if (zern(14) <= zern(k)) zern(14) = zern(k)
end do
! only make a plot if there's data
if (ABS(zern(13)-zern(14)) > EPS) then
unitno1 = get_new_fileunit()
open(unitno1, file = "zernike.tmp", action="write", iostat=ierr)
 write(unitno1,*) "reset session"
 write(unitno1,'(A)') "$Data << EOD"                !no leading spaces or gnuplot vomits
 if (zern(1) < 0) then
  write(unitno1,*) "Z(4,4)VerticalQuatrafoil ", zern(1), " 0xff0000 "
 else
  write(unitno1,*) "Z(4,4)VerticalQuatrafoil ", zern(1), " 0x0000ff"
 endif

 if (zern(2) < 0) then
  write(unitno1,*) "Z(4,2)Vertical2ndAstig ", zern(2), " 0xff0000 "
 else
  write(unitno1,*) "Z(4,2)Vertical2ndAstig ", zern(2), " 0x0000ff"
 endif

 if (zern(3) < 0) then
  write(unitno1,*) "Z(4,0)SphericalAberration ", zern(3), " 0xff0000 "
 else
  write(unitno1,*) "Z(4,0)SphericalAberration ", zern(3), " 0x0000ff"
 endif

 if (zern(4) < 0) then
  write(unitno1,*) "Z(4,-2)Oblique2ndAstig ", zern(4), " 0xff0000 "
 else
  write(unitno1,*) "Z(4,-2)Oblique2ndAstig ", zern(4), " 0x0000ff"
 endif

 if (zern(5) < 0) then
  write(unitno1,*) "Z(4,-4)ObliqueQuatrafoil ", zern(5), " 0xff0000 "
 else
  write(unitno1,*) "Z(4,-4)ObliqueQuatrafoil ", zern(5), " 0x0000ff"
 endif

 if (zern(6) < 0) then
  write(unitno1,*) "Z(3,3)ObliqueTrefoil ", zern(6), " 0xff0000 "
 else
  write(unitno1,*) "Z(3,3)ObliqueTrefoil ", zern(6), " 0x0000ff"
 endif

 if (zern(7) < 0) then
  write(unitno1,*) "Z(3,1)HorizontalComa ", zern(7), " 0xff0000 "
 else
  write(unitno1,*) "Z(3,1)HorizontalComa ", zern(7), " 0x0000ff"
 endif

 if (zern(8) < 0) then
  write(unitno1,*) "Z(3,-1)VerticalComa ", zern(8), " 0xff0000 "
 else
  write(unitno1,*) "Z(3,-1)VerticalComa ", zern(8), " 0x0000ff"
 endif

 if (zern(9) < 0) then
  write(unitno1,*) "Z(3,-3)VerticalTrefoil ", zern(9), " 0xff0000 "
 else
  write(unitno1,*) "Z(3,-3)VerticalTrefoil ", zern(9), " 0x0000ff"
 endif

 if (zern(10) < 0) then
  write(unitno1,*) "Z(2,2)VerticalAstig ", zern(10), " 0xff0000 "
 else
 write(unitno1,*) "Z(2,2)VerticalAstig ", zern(10), " 0x0000ff"
 endif

 if (zern(11) < 0) then
  write(unitno1,*) "Z(2,0)Defocus ", zern(11), " 0xff0000 "
 else
  write(unitno1,*) "Z(2,0)Defocus ", zern(11), " 0x0000ff"
 endif

 if (zern(12) < 0) then
  write(unitno1,*) "Z(2,-2)ObliqueAstigmatism ", zern(12), " 0xff0000 "
 else
  write(unitno1,*) "Z(2,-2)ObliqueAstigmatism ", zern(12), " 0x0000ff"
 endif
 write(unitno1,'(A)') "EOD"
 write(unitno1,*) "set style fill solid";
 write(unitno1,*) "unset key";
 write(unitno1,*) "myBoxWidth = 0.8";
 write(unitno1,*) "set offsets 0,0,0.5-myBoxWidth/2.,0.5";
 write(unitno1,*) "plot $Data using (0.5*$2):0:(0.5*$2):(myBoxWidth/2.):($3):ytic(1) with boxxy lc rgb var";
 close(unitno1)

!good place to call a c program to display
 call Ccounter(100,"zernike.tmp"//c_null_char)

else
 call LogC("No Zernike data found"//c_null_char)
endif
 return
endif

! writes values in openGL friendly format to matrices for passing to C/C++
! flag/fct determines what to write for elevation and color, just like in flag=2,3 output versions above
  donut = .FALSE.
  call selectfunction(0,JMatrix,flag,powctr,powmin,powmax,cardinal,nC)
  dist = real(-2*JMatrix%Z0(3),kind=sk)
! generate buffer data
  pupil_elements(1:pupil_nE)=0
  pupil_vertices(1:pupil_nV)=0
  elements(1:nE) = 0
  vertices(1:nV) = 0
  call Geom(flag, JMatrix, donut, powmin, powmax, elements, vertices, nV, nE)
  call Pupil(JMatrix, dist, pupil_elements, pupil_vertices, pupil_nV, pupil_nE)
  call makelegend(flag, powmin, powmax, legend, nL)
  JMatrix%MV(:)=MV(:)


!write(*,*) JMatrix%SAGC0
!write(*,*) powmin,powmax,dist


! eigenvalues show shape of RadSlope without make_rings but with FillArray 7 elevations
!  atmp=pca(2,RadSlope)
!  atmp=pca(3,RadSlope)
  if (mod(flag,100) == 0) loaded_files = loaded_files + 1
  write(*,*) "Files loaded this session: ", loaded_files
  write(*,*) "Computations run under ",trim(compiler_version())
  call LogC("Computations run under " // trim(compiler_version()) // c_null_char)  !has to be C and declared, not cpp

  return        

  END subroutine janus
