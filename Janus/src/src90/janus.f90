  subroutine Janus(flag,file_from_C,elements,vertices,legend,zern,nV,nE,nL,pupil_elements,pupil_vertices,pupil_nV,pupil_nE) bind(C,name='janus_')
! DRIVER PROGRAM FOR SPLINE ROUTINES
  use set_precision, ONLY : wp, sk
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
  integer(c_int), INTENT(INOUT) :: pupil_nV
  integer(c_int), INTENT(INOUT) :: pupil_nE
  real(c_float), INTENT(INOUT) :: pupil_vertices(*)
  integer(c_int), INTENT(INOUT) :: pupil_elements(*)
  integer(c_int), INTENT(INOUT) :: nL
  real(c_float), INTENT(INOUT) :: legend(*)
  real(c_float), INTENT(INOUT) :: zern(*)
  real(c_float) :: dist
  character(len=4096) :: new_path
  character(:),save, ALLOCATABLE :: inputfile1,inputfile2,inputfile3,BigPlot,gnu_instruct
  character(:),save, ALLOCATABLE :: logfile
  integer ::  nblines, file_idx,read_error,io
  integer,allocatable :: MV(:)
  real(8) :: time_start, time_end
  real(wp) :: POWMIN,POWMAX,POWMAX2,POWCTR,POW,P1,X1,X2,U,V
  logical :: donut, exists
  real(wp) :: Y,YPR,YPTHETA,YPRTHETA,YP2R2,YP2THETA,rBi,rBo
  integer :: k_max, kk_max, iflag, LWORK
  integer(c_int) :: dat, fct, map
  real(wp), allocatable :: zernC(:,:), B_Matrix(:,:), rlocal(:), thtlocal(:), WORK(:)
  real(wp), allocatable :: UT(:,:),VT(:,:) !,XTX(:,:),EE(:,:)
!  integer, allocatable :: IPIV(:)
  real(wp) :: ctr_circle_x, ctr_circle_y, R_global, Theta_global, X_global, Y_global

write(*,*) 'flag to Fortran:',flag
write(*,*) 'flag(action) last digits to Fortran:',mod(flag,100)
!! last two digits are the program function
!! 99 = deallocate arrays for program closure
!! 10 = compare
!! 9 = show zernike coefficients
!! 8 = show circumferential ring lsqfillin/splinefillin
!! 7 = make lioc
!! 6 = make centers
!! 5 = make gnuplotsplot
!! 4 = redraw without reloading new file
!! 3 = write ASCII PLY file
!! 2 = write OFF file
!! 1 = compute zernike coefficients/maps
!! 0 = open a file, display
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

! mod(flag,100) == 99 Deallocate
if (mod(flag,100) == 99) then
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

if (mod(flag,100) == 0 .or. mod(flag,100) == 2 .or. mod(flag,100) == 3) then
!  only need new file name if opening a file or printing, and
!  local save of inputfile1,inputfile2,logfile
!  write(*,*) 'file from kernunos: ',file_from_C  ! this will have a lot of extra random non ASCII stuff after the file name
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
! write(*,*) 'file from kernunos: ',trim(new_path)
 nblines=len(trim(new_path))
 if (allocated(inputfile1)) then
  deallocate(inputfile1)
  deallocate(inputfile2)
  deallocate(inputfile3)
  deallocate(logfile)
 endif
 allocate(character(nblines) :: inputfile1)
 allocate(character(nblines) :: logfile)
 inputfile1=trim(new_path)
 allocate(character(nblines) :: inputfile2)
 allocate(character(nblines) :: inputfile3)
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
 write(*,*) 'file from kernunos: ',trim(new_path)
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

if (mod(flag,100) .eq. 5 ) then
! gnuplot splot output
! needs powmin & powmax
 if (allocated(JMatrix%R)) then
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
    CASE (21)
    powctr=JMatrix%Warp0(1)
    powmin=JMatrix%Warp0(2)
    powmax=JMatrix%Warp0(3)
    CASE DEFAULT
    powctr=JMatrix%SAGC0(1)
    powmin=JMatrix%SAGC0(2)
    powmax=JMatrix%SAGC0(3)
  END SELECT
  endif
 endif
! generate data file
  unitno1 = get_new_fileunit()
  open(unitno1, file = BigPlot, action="write", iostat=ierr)
  do i=1,M1
   do j=1,JMatrix%MV(i)
    X1=JMatrix%tht(i)
    X2=JMatrix%r(j,i)
    P1=JMatrix%SAGC(j,i)
    IF((ABS(P1).GT.0).AND.(ABS(X2).GT.0.01)) THEN
      WRITE(unitno1,*) ABS(X2)*COS(X1),ABS(X2)*SIN(X1),P1
    ENDIF
   end do
   WRITE(unitno1,*) ' '
  end do
! REPEAT FIRST ANGLE
  i=1
  do J=1,JMatrix%MV(i)
   X1=JMatrix%tht(i)
   X2=JMatrix%r(j,i)
   P1=JMatrix%SAGC(j,i)
   IF((P1.GT.0).AND.(ABS(X2).GT.0.01)) THEN
    WRITE(unitno1,*) ABS(X2)*COS(X1),ABS(X2)*SIN(X1),P1
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
!! uncomment to restrict to Placido disk formats, ie no pentacam
if ( Testdata .le. 1 ) then      ! test, EyeSys or Atlas
 call MakeRadSplineCenter(0)     ! remakes RadSplineCenter(1,:)
 if (btest(dat, 0) ) then         ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
   DiaSlope%Zpd2 = .nc. DiaSlope ! re-spline, with center node
 endif
 if (btest(dat, 1)) then          ! moving each meridian to align curves
  call AdjustRadSplineCenter     ! changes r only
  DiaSlope%Zpd2 = .n. DiaSlope   ! re-spline, standard
 endif
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
    CASE (21)
    powctr=JMatrix%Warp0(1)
    powmin=JMatrix%Warp0(2)
    powmax=JMatrix%Warp0(3)
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
    CASE (21)
    powctr=JMatrix%Warp0(1)
    powmin=JMatrix%Warp0(2)
    powmax=JMatrix%Warp0(3)
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

! simple difference/subtraction with compare
! currently does not actually work with ZC since there's no compare option with flag=1
if (mod(flag,100) == 10) then
 if (allocated(JMatrix2%R)) then
! use geometry from current JMatrix to populate
  JMatrix2%R(:,:)=JMatrix%R(:,:) ! might have zeroes if smaller, but should be caught by MV below
  JMatrix2%THT(:)=JMatrix%THT(:)
  JMatrix2%R0=JMatrix%R0
  JMatrix2%THT0=JMatrix%THT0

  JMatrix2%Z(:,:)=ABS(JMatrix1%Z(:,:)-JMatrix%Z(:,:))
  JMatrix2%SAGC(:,:)=ABS(JMatrix1%SAGC(:,:)-JMatrix%SAGC(:,:))
  JMatrix2%Warp(:,:)=ABS(JMatrix1%Warp(:,:)-JMatrix%Warp(:,:))
  JMatrix2%INSTC(:,:)=ABS(JMatrix1%INSTC(:,:)-JMatrix%INSTC(:,:))
  JMatrix2%INSTC2(:,:)=ABS(JMatrix1%INSTC2(:,:)-JMatrix%INSTC2(:,:))
  JMatrix2%MEANC(:,:)=ABS(JMatrix1%MEANC(:,:)-JMatrix%MEANC(:,:))
  JMatrix2%MONGEA(:,:)=ABS(JMatrix1%MONGEA(:,:)-JMatrix%MONGEA(:,:))
  JMatrix2%SAGC0(:)=ABS(JMatrix1%SAGC0(:)-JMatrix%SAGC0(:))
  JMatrix2%Z0(:)=ABS(JMatrix1%Z0(:)-JMatrix%Z0(:))
  JMatrix2%Warp0(:)=ABS(JMatrix1%Warp0(:)-JMatrix%Warp0(:))
  JMatrix2%INSTC0(:)=ABS(JMatrix1%INSTC0(:)-JMatrix%INSTC0(:))
  JMatrix2%INSTC20(:)=ABS(JMatrix1%INSTC20(:)-JMatrix%INSTC20(:))
  JMatrix2%MEANC0(:)=ABS(JMatrix1%MEANC0(:)-JMatrix%MEANC0(:))
  JMatrix2%MONGEA0(:)=ABS(JMatrix1%MONGEA0(:)-JMatrix%MONGEA0(:))
  JMatrix2%ZC(:,:,:)=ABS(JMatrix1%ZC(:,:,:)-JMatrix%ZC(:,:,:))
  JMatrix2%ZC0(:,:)=ABS(JMatrix1%ZC0(:,:)-JMatrix%ZC0(:,:))

! have to re-do min/max
 JMatrix2%SAGC0(2)=1E30   ;  JMatrix2%SAGC0(3)=-1E30
 JMatrix2%Warp0(2)=1E30   ;  JMatrix2%Warp0(3)=-1E30
 JMatrix2%Z0(2)=1E30      ;  JMatrix2%Z0(3)=-1E30
 JMatrix2%INSTC0(2)=1E30  ;  JMatrix2%INSTC0(3)=-1E30
 JMatrix2%INSTC20(2)=1E30 ;  JMatrix2%INSTC20(3)=-1E30
 JMatrix2%MEANC0(2)=1E30  ;  JMatrix2%MEANC0(3)=-1E30
 JMatrix2%MONGEA0(2)=1E30 ;  JMatrix2%MONGEA0(3)=-1E30
 JMatrix2%ZC0(2,:)=1E30   ;  JMatrix2%ZC0(3,:)=-1E30
do i=1,M1
 JMatrix2%MV(i)=min(JMatrix%MV(i),JMatrix1%MV(i))
 do j=1,JMatrix2%MV(i)
   if (JMatrix2%INSTC(j,i) <= JMatrix2%INSTC0(2)) JMatrix2%INSTC0(2)=JMatrix2%INSTC(j,i)
   if (JMatrix2%INSTC(j,i) >= JMatrix2%INSTC0(3)) JMatrix2%INSTC0(3)=JMatrix2%INSTC(j,i)
   if (JMatrix2%INSTC2(j,i) <= JMatrix2%INSTC20(2)) JMatrix2%INSTC20(2)=JMatrix2%INSTC2(j,i)
   if (JMatrix2%INSTC2(j,i) >= JMatrix2%INSTC20(3)) JMatrix2%INSTC20(3)=JMatrix2%INSTC2(j,i)
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
    if (JMatrix2%ZC0(1,k) <= JMatrix2%ZC0(2,k)) JMatrix2%ZC0(2,k)=JMatrix2%ZC0(1,k)
    if (JMatrix2%ZC0(1,k) >= JMatrix2%ZC0(3,k)) JMatrix2%ZC0(3,k)=JMatrix2%ZC0(1,k)
   end do
 end do
end do

   donut = .FALSE.
   if (fct .lt. 16 .and. fct .gt. 0) then
     powctr=JMatrix2%ZC0(1,fct)
     powmin=JMatrix2%ZC0(2,fct)
     powmax=JMatrix2%ZC0(3,fct)
   else
   SELECT CASE (fct)
     CASE (0)
     powctr=JMatrix2%SAGC0(1)
     powmin=JMatrix2%SAGC0(2)
     powmax=JMatrix2%SAGC0(3)
     CASE (16)
     powctr=JMatrix2%INSTC0(1)
     powmin=JMatrix2%INSTC0(2)
     powmax=JMatrix2%INSTC0(3)
     CASE (17)
     powctr=JMatrix2%INSTC20(1)
     powmin=JMatrix2%INSTC20(2)
     powmax=JMatrix2%INSTC20(3)
     CASE (18)
     powctr=JMatrix2%MEANC0(1)
     powmin=JMatrix2%MEANC0(2)
     powmax=JMatrix2%MEANC0(3)
     CASE (19)
     powctr=JMatrix2%MONGEA0(1)
     powmin=JMatrix2%MONGEA0(2)
     powmax=JMatrix2%MONGEA0(3)
     CASE (20)
     powctr=JMatrix2%Z0(1)
     powmin=JMatrix2%Z0(2)
     powmax=JMatrix2%Z0(3)
     CASE (21)
     powctr=JMatrix2%Warp0(1)
     powmin=JMatrix2%Warp0(2)
     powmax=JMatrix2%Warp0(3)
     CASE DEFAULT
     powctr=JMatrix2%SAGC0(1)
     powmin=JMatrix2%SAGC0(2)
     powmax=JMatrix2%SAGC0(3)
   END SELECT
  endif

  elements(1:nE)=0
  vertices(1:nV)=0
  call Geom(flag, JMatrix2, donut, powmin, powmax, elements, vertices, nV, nE)
  call makelegend(flag, powmin, powmax, legend, nL)

 return
 else
  write(*,*) "Needs two scans for compare"
  return
 endif
endif

! last two digits of flag == 0 parse file name, assign TestData type and MM,N
if (mod(flag,100) == 0) then
 call CCounter(0,inputfile1//c_null_char)
! From either RA?.? or XX?.?, set inputfile1 to the XX version, inputfile2 to the RA version, inputfile3 to the PU version,
! For PentaCam
! set inputfile1 for _ELE.CSV or .ELE,
! set inputfile2 for _CUR.CSV or .CUR
! For CSV but not _ELE.CSV or _CUR.CSV set inputfile1 to Atlas file
 file_idx=index(inputfile1, "RA")+index(inputfile1, "XX")
   if( file_idx == 0)then
      file_idx=index(inputfile1, ".CSV")
      if( file_idx == 0) then
       file_idx=index(inputfile1, ".CUR")
       if( file_idx == 0) then
        file_idx=index(inputfile1, ".ELE")
        if( file_idx == 0) then
         write(*,*) 'Unknown file type: make some test data, flag = ',flag
         TestData=-1; MM=360; N=16 ; NP=141
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
!  EyeSys
      file_idx=index(inputfile1, "XX")  !index(inputfile1, "XX", back)
      if (file_idx /= 0) then
!       write(*,*) 'prefix is found at index: ',file_idx,"length: ",len(inputfile1)
!       write(*,*) 'prefix:',inputfile1(file_idx:file_idx+1)
       write(*,*) 'EyeSys XX file: ',inputfile1
       inputfile2=replacestr(string=inputfile1,search="XX",substitute="RA")
       inputfile3=replacestr(string=inputfile1,search="XX",substitute="PU")
       inquire(file=trim(inputfile3), exist=exists)
       if(exists) then
        write(*,*) "Matching EyeSys PU file found"
       endif
       inquire(file=trim(inputfile2), exist=exists)
       if(.NOT.exists) then
        inputfile2=replacestr(string=inputfile1,search="/XX",substitute="/RA")
        inputfile3=replacestr(string=inputfile1,search="/XX",substitute="/PU")
        inquire(file=trim(inputfile3), exist=exists)
        if(exists) then
         write(*,*) "Matching EyeSys PU file found"
        endif
        inquire(file=trim(inputfile2), exist=exists)
        if(.NOT.exists) then
         write(*,*) 'Error: EyeSys files have to be in pairs, or file name has XX other than prefix'
         write(*,*) 'No corresponding',inputfile2,'for',inputfile1,'found'
         return
        endif
       endif
      else
       file_idx=index(inputfile1, "RA") !index(inputfile1, "RA", back)
       if (file_idx /= 0) then
!        write(*,*) 'prefix is found at index: ',file_idx,"length: ",len(inputfile1)
!        write(*,*) 'prefix:',inputfile1(file_idx:file_idx+1)
        write(*,*) 'EyeSys RA file: ',inputfile1
        inputfile2=inputfile1
        inputfile1=replacestr(string=inputfile2,search="RA",substitute="XX")
        inputfile3=replacestr(string=inputfile2,search="RA",substitute="PU")
        inquire(file=trim(inputfile3), exist=exists)
        if(exists) then
         write(*,*) "Matching EyeSys PU file found"
        endif
        inquire(file=trim(inputfile1), exist=exists)
        if(.NOT.exists) then
         inputfile1=replacestr(string=inputfile2,search="/RA",substitute="/XX")
         inputfile3=replacestr(string=inputfile2,search="/RA",substitute="/PU")
         inquire(file=trim(inputfile3), exist=exists)
         if(exists) then
          write(*,*) "Matching EyeSys PU file found"
         endif
         inquire(file=trim(inputfile1), exist=exists)
         if(.NOT.exists) then
          write(*,*) 'Error: EyeSys files have to be in pairs, or file name has RA other than prefix'
          write(*,*) 'No corresponding',inputfile1,'for',inputfile2,'found'
          return
         endif
        endif
       else
        write(*,*) 'Error parsing EyeSys file name'
        return
       endif
      endif
     TestData=0 ; MM=360; N=16   ! EyeSys
     inquire(file=trim(inputfile3), exist=exists)
     if(.NOT.exists) then
      write(*,*) "EyeSys files: ",inputfile1," ",inputfile2
     else
      write(*,*) "EyeSys files: ",inputfile1," ",inputfile2," ",inputfile3
     endif
    endif
 endif ! (mod(flag,100) == 0) parsing the file name,assigning TestData type and MM,N


! allocate JMatrix needed for file import
!  JMatrix is 180x22 to make importing from Atlas easier.
!  M1,N1 avoid overwriting MM,N at this point
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
    if (allocated(JMatrix2%R)) then
     write(*,*) 'JMatrix2 allocated'
    else
     write(*,*) 'allocating JMatrix2'
     call init_mat_JMatrix(M1,N1,JMatrix2)
    endif
    if (mod(flag,100) /= 10) then
     JMatrix1%R(:,:)=JMatrix%R(:,:)
     JMatrix1%PU(:)=JMatrix%PU(:)
     JMatrix1%Z(:,:)=JMatrix%Z(:,:)     
     JMatrix1%YPR(:,:)=JMatrix%YPR(:,:)
     JMatrix1%YPTHETA(:,:)=JMatrix%YPTHETA(:,:)
     JMatrix1%THT(:)=JMatrix%THT(:)
     JMatrix1%SAGC(:,:)=JMatrix%SAGC(:,:)
     JMatrix1%Warp(:,:)=JMatrix%Warp(:,:)
     JMatrix1%INSTC(:,:)=JMatrix%INSTC(:,:)
     JMatrix1%INSTC2(:,:)=JMatrix%INSTC2(:,:)
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
     JMatrix1%INSTC20(:)=JMatrix%INSTC20(:)
     JMatrix1%MEANC0(:)=JMatrix%MEANC0(:)
     JMatrix1%MONGEA0(:)=JMatrix%MONGEA0(:)
     JMatrix1%ZC0(:,:)=JMatrix%ZC0(:,:)
     JMatrix1%ZC(:,:,:)=JMatrix%ZC(:,:,:)
    endif
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
   inquire(file=trim(inputfile3), exist=exists)
   if(.NOT.exists) then
    call RCNVRTE(read_error,inputfile2,inputfile1)
   else
    call RCNVRTE(read_error,inputfile2,inputfile1,inputfile3)
!   pupil conversion if any
    JMatrix%Pupil_Center=EyeSys%Pupil_Center/10.
    do i=1,MM/2
     JMatrix%PU(i)=(EyeSys%PU(2*i-1)+EyeSys%PU(2*i))/40.  !2x2x10 average,diameter->radius,factor of 10
    end do
   endif
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
  inputfile2=inputfile1
  if (read_error .eq. 1) then
   write(*,*) 'Possible semicolon delimited Atlas file, try sed'
   inputfile2=replacestr(string=inputfile1,search=".CSV",substitute=".TMP")
!   write(*,*) 'sed "s/;/,/g" ' // inputfile1 // ' > ' // inputfile2
   call system('sed "s/;/,/g" ' // inputfile1 // ' > ' // inputfile2, io)
   if (io > 0) then
    write (*,*) 'system command to sed failed'
    write (*,*) 'Consider using your text editor to search/replace all semicolons with commas in',inputfile1
   else
    call RCNVRTA_type(inputfile2, Power_Rings_Count, read_error)
    if (read_error > 0) write (*,*) 'temp Atlas file read error, probably not because semicolon delimited'
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
  file_idx=index(inputfile2, ".TMP")
  if (file_idx .ne. 0) then
   call system('rm ' // inputfile2, io)
   if (io > 0) write (*,*) 'system command to remove tmp file failed'
  endif
  call CPU_TIME(time_end)
  write(*,*) 'Time to read Atlas CSV file: ',(time_end-time_start)*1000
  if (read_error > 0) return
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
  write(*,*) 'Radslope,DiaSlope need to be reallocated'
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
! ELE are elevations, CUR are "sagittal" curvatures in a 141x141 -7 to 7 mm square -1 is no data
! _ELE.CSV or _CUR.CSV versions have less text but use semicolons (;) instead of -1
  if (mod(flag,100) == 0) then ! read the files
   read_error=0
   if (TestData .eq. 3 .or. TestData .eq. 5) then
   call RCNVRTP(TestData,inputfile2,read_error)  !curvatures
   else
   call RCNVRTP(TestData,inputfile1,read_error)  !elevations TestData .eq. 2 .or. TestData .eq. 4
   endif
   if (read_error > 0) return
   ! pupil conversion, could do whole circular spline here
    JMatrix%Pupil_Center=Penta%Pupil_Center/10.
    do i=1,MM
     j=floor(1.+(i-1)*255/179.0)
     X1=Penta%PU(j,1)-Penta%Pupil_Center(1)
     X2=Penta%PU(j,2)-Penta%Pupil_Center(2)
     JMatrix%PU(i)=sqrt(X1*X1+X2*X2)/10.
    end do
  endif  !(mod(flag,100) /= 0,99,2,3 must be 1 or 4-7, reload the original data
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
  endif
  JMatrix%Z(:,:) = 0 ; JMatrix%Z0(:) = 0
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
  endif
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

 ! gnuplot splot output and exit
  if (mod(flag,100) .eq. 8 ) then
 ! generate data file
   unitno1 = get_new_fileunit()
   BigPlot=replacestr(string=gnu_instruct,search=".gnu",substitute=".plt")
   open(unitno1, file = BigPlot, action="write", iostat=ierr)
 ! Look at these intersecting rings using
 ! gnuplot 'plot 'datafile dumped with' u 1:2' do not set polar or with lines
    do j=1,N1
     do i=1,M1
      write(unitno1,*) Atlas%DEG(i),Atlas%AD(i,j)
     end do
     write(unitno1,*) ' '
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
endif

! skip all this if we're just displaying zernike coefficients again
if (mod(flag,100) .ne. 9 ) then
! Spline RadSlope
 DiaSlope=RadSlope              ! move to diagonal format
 DiaSlope%Zpd2 = .n. DiaSlope   ! spline slopes across center without tweaks
 if ( Testdata .eq. 1 ) then     ! only for Atlas at present
  call MakeRadSplineCenter(0)    ! capture the spline center deviations from unmodified RadSlope
! Have to do AdjustSlope tweak before centernode, since centernode essentially reduces RadSplineCenter(1,:) to 0
  if (btest(dat, 1)) then         ! moving each meridian to align curves
   call AdjustRadSplineCenter     ! changes r only in DiaSlope
   DiaSlope%Zpd2 = .n. DiaSlope   ! re-spline, standard
  endif
  if (btest(dat, 0) ) then        ! use nsplineCenter to force zero slope at origin, changing spline but requiring SplineEvalCenter
   DiaSlope%Zpd2 = .nc. DiaSlope   ! re-spline, standard
  endif
  call MakeRadSplineCenter(dat)        ! generates spline centers with tweaks
  endif

 ! Atlas spline consistency check and computation of elevation by power vs elevation in file
 if ( Testdata .eq. 1 ) then
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
  JMatrix%Warp0(2)=1E30   ;  JMatrix%Warp0(3)=-1E30 ; JMatrix%Warp0(1)=0
  JMatrix%Z0(2)=1E30      ;  JMatrix%Z0(3)=-1E30 ;    JMatrix%Z0(3)=0
  JMatrix%INSTC0(2)=1E30  ;  JMatrix%INSTC0(3)=-1E30 ; JMatrix%INSTC0(1)=0
  JMatrix%INSTC20(2)=1E30 ;  JMatrix%INSTC20(3)=-1E30 ; JMatrix%INSTC20(1)=0
  JMatrix%MEANC0(2)=1E30  ;  JMatrix%MEANC0(3)=-1E30 ; JMatrix%MEANC0(1)=0
  JMatrix%MONGEA0(2)=1E30 ;  JMatrix%MONGEA0(3)=-1E30 ; JMatrix%MONGEA0(1)=0
  JMatrix%R0=0 ; JMatrix%THT0=0
! Generate the rings
! JMatrix
  do i=1,M1                             ! every 2 degrees
   JMatrix%THT(i)=PI*(i-1)/90.0_wp
   if (MM == 360 .and. N == 16) then  ! original EyeSys RadSlope or fake data
    JMatrix%MV(i)=MIN(RadSlope%MV(2*i),RadSlope%MV(2*i-1))  ! close to real boundary
   else  ! MM==180
    JMatrix%MV(i)=min(RadSlope%MV(i),N1)  ! if N=25 don't do more than 22
   endif
   do j=1,JMatrix%MV(i)                             ! does not include center point
    JMatrix%R(j,i)=N1*100*(rBi+(j-1)*(rBo-rBi)/(N1-1))/(1.*N)  !scaled to compensate for 16 vs 22 or 25 rings
! populate JMatrix rings, not the centers
! elevations
    if (TestData.ne.2 .and. TestData.ne.4) then  ! slope based data, integrate based on iflag with or without cubic/trapez or center point or not for values
     call SplineEval1Dx1D(iflag,JMatrix%R(j,i),JMatrix%THT(i),JMatrix%Z(j,i),YPR,YP2R2,YPTHETA,YPRTHETA,YP2THETA)
    else  !TestData.eq.2 .or. TestData.eq.4  ! ELE and ELE.CSV files use elevation, no integration, center point or not
     if (btest(dat,0)) then
      call SplineEval1Dx1D(10,JMatrix%R(j,i),JMatrix%THT(i),JMatrix%Z(j,i),YPR,YP2R2,YPTHETA,YPRTHETA,YP2THETA)
     else
      call SplineEval1Dx1D(0,JMatrix%R(j,i),JMatrix%THT(i),JMatrix%Z(j,i),YPR,YP2R2,YPTHETA,YPRTHETA,YP2THETA)
     endif
    endif
!  save for vertex normals and for LIOC
    JMatrix%YPR(j,i)=YPR
    JMatrix%YPTHETA(j,i)=YPTHETA
!  powers
    call AXIALP(JMatrix%R(j,i),ABS(YPR),YP2R2,JMatrix%SAGC(j,i))
    call AXIALP(JMatrix%R(j,i),YPTHETA/abs(JMatrix%R(j,i)),YP2THETA/abs(JMatrix%R(j,i)),JMatrix%Warp(j,i))
    call INSTANTP(JMatrix%R(j,i),YPR,YPTHETA,YP2R2,JMatrix%INSTC(j,i),JMatrix%INSTC2(j,i))
    call MEANP(JMatrix%THT(i),JMatrix%R(j,i),ABS(YPR),YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MEANC(j,i))
    call MONGEA(JMatrix%THT(i),JMatrix%R(j,i),ABS(YPR),YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MONGEA(j,i))
!    if (M1 .eq. 360) then
!     k=8 ! skip this many problematic values at x-axis
!    else
!     k=4  ! fewer because every 2 degrees
!    endif
!    if ( (i .gt. (1+k) .and. i .lt. (M1/2-k)) .or. (i .lt. (M1-k)  .and. i .gt. (M1/2+k))) then
!     call MONGEA(JMatrix%THT(i),JMatrix%R(j,i),ABS(YPR),YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MONGEA(j,i))
!     call MEANP(JMatrix%THT(i),JMatrix%R(j,i),ABS(YPR),YPTHETA,YPRTHETA,YP2THETA,YP2R2,JMatrix%MEANC(j,i))
!    endif
    if (JMatrix%INSTC(j,i) <= JMatrix%INSTC0(2)) JMatrix%INSTC0(2)=JMatrix%INSTC(j,i)
    if (JMatrix%INSTC(j,i) >= JMatrix%INSTC0(3)) JMatrix%INSTC0(3)=JMatrix%INSTC(j,i)
    if (JMatrix%INSTC2(j,i) <= JMatrix%INSTC20(2)) JMatrix%INSTC20(2)=JMatrix%INSTC2(j,i)
    if (JMatrix%INSTC2(j,i) >= JMatrix%INSTC20(3)) JMatrix%INSTC20(3)=JMatrix%INSTC2(j,i)
    if (JMatrix%Z(j,i) <= JMatrix%Z0(2)) JMatrix%Z0(2)=JMatrix%Z(j,i)
    if (JMatrix%Z(j,i) >= JMatrix%Z0(3)) JMatrix%Z0(3)=JMatrix%Z(j,i)
    if (JMatrix%SAGC(j,i) <= JMatrix%SAGC0(2)) JMatrix%SAGC0(2)=JMatrix%SAGC(j,i)
    if (JMatrix%SAGC(j,i) >= JMatrix%SAGC0(3)) JMatrix%SAGC0(3)=JMatrix%SAGC(j,i)
    if (JMatrix%Warp(j,i) <= JMatrix%Warp0(2)) JMatrix%Warp0(2)=JMatrix%Warp(j,i)
    if (JMatrix%Warp(j,i) >= JMatrix%Warp0(3)) JMatrix%Warp0(3)=JMatrix%Warp(j,i)
   end do
  end do !end JMatrix ring generation
! spline over problematic limits at x-axis
!  JMatrix%MEANC=splinefillintranspose(JMatrix%MEANC)
!  JMatrix%MONGEA=splinefillintranspose(JMatrix%MONGEA)
  do i=1,M1
   do j=1,JMatrix%MV(i)
    if (JMatrix%MEANC(j,i) <= JMatrix%MEANC0(2)) JMatrix%MEANC0(2)=JMatrix%MEANC(j,i)
    if (JMatrix%MEANC(j,i) >= JMatrix%MEANC0(3)) JMatrix%MEANC0(3)=JMatrix%MEANC(j,i)
    if (JMatrix%MONGEA(j,i) <= JMatrix%MONGEA0(2)) JMatrix%MONGEA0(2)=JMatrix%MONGEA(j,i)
    if (JMatrix%MONGEA(j,i) >= JMatrix%MONGEA0(3)) JMatrix%MONGEA0(3)=JMatrix%MONGEA(j,i)
   end do
  end do

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

! this stanza is only for elevations (and therefore for input to zernike also)
 if ( Testdata .eq. 1 ) then
   call MakeRadSplineCenter(0)     ! remakes RadSplineCenter(1,:)
   if (btest(dat, 0) ) then        ! use nsplineCenter to force zero slope at origin,
    DiaSlope%Zpd2 = .nc. DiaSlope ! re-spline, with center node
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

!  Z
!   if (TestData.ne.2 .and. TestData.ne.4) then   ! already has valid Z0 from cornea_arrays & ELE file NOT YET IT DOES NOT
!   notice that we didn't load Z into Zp and then use iflag=10, and 0
    do i=1,M1
     call SplineEval1Dx1D(iflag,JMatrix%R0,JMatrix%THT(i),JMatrix%Z(N1+1,i))  !center value of elevation; needs integration from slopes
     if (i .eq. 1) then
      JMatrix%Z0(1)=JMatrix%Z(N1+1,1)
     else
      JMatrix%Z0(1)=(i*JMatrix%Z0(1)+JMatrix%Z(N1+1,i))/(i+1)      ! cumulative average
     endif
    end do
    if (JMatrix%Z0(1) <= JMatrix%Z0(2)) JMatrix%Z0(2)=JMatrix%Z0(1)
    if (JMatrix%Z0(1) >= JMatrix%Z0(3)) JMatrix%Z0(3)=JMatrix%Z0(1)
!   endif  ! TestData.eq.2 .or. TestData.eq.4

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

    do i=1,M1
     if (btest(dat,0)) then
      call SplineEval1Dx1D(10,JMatrix%R0,JMatrix%THT(i),JMatrix%SAGC(N1+1,i))  ! center value
     else
      call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT(i),JMatrix%SAGC(N1+1,i))  ! center value
     endif
     if (i .eq. 1) then
      JMatrix%SAGC0(1)=JMatrix%SAGC(N1+1,1)
     else
      JMatrix%SAGC0(1)=(i*JMatrix%SAGC0(1)+JMatrix%SAGC(N1+1,i))/(i+1)      ! cumulative average
     endif
    end do
    if (JMatrix%SAGC0(1) <= JMatrix%SAGC0(2)) JMatrix%SAGC0(2)=JMatrix%SAGC0(1)
    if (JMatrix%SAGC0(1) >= JMatrix%SAGC0(3)) JMatrix%SAGC0(3)=JMatrix%SAGC0(1)
!   endif   ! TestData.eq.3 .or. TestData.eq.5

!  Warp
!  Reload RadSlope with SAGC & re-spline; can't compute it from surface because ill-defined at origin
    do i=1,M1
     do j=1,RadSlope%MV(i)
      RadSlope%Zp(j,i)=JMatrix%Warp(j,i)
     end do
    end do
    DiaSlope=RadSlope              ! move to diagonal format
    DiaSlope%Zpd2 = .n. DiaSlope

    do i=1,M1
     if (btest(dat,0)) then
      call SplineEval1Dx1D(10,JMatrix%R0,JMatrix%THT(i),JMatrix%Warp(N1+1,i))  ! center value
     else
      call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT(i),JMatrix%Warp(N1+1,i))  ! center value
     endif
     if (i .eq. 1) then
      JMatrix%Warp0(1)=JMatrix%Warp(N1+1,1)
     else
     JMatrix%Warp0(1)=(i*JMatrix%Warp0(1)+JMatrix%Warp(N1+1,i))/(i+1)      ! cumulative average
     endif
    end do
    if (JMatrix%Warp0(1) <= JMatrix%Warp0(2)) JMatrix%Warp0(2)=JMatrix%Warp0(1)
    if (JMatrix%Warp0(1) >= JMatrix%Warp0(3)) JMatrix%Warp0(3)=JMatrix%Warp0(1)
!   endif

!  INSTC
!  Reload RadSlope & respline
   do i=1,M1
    do j=1,RadSlope%MV(i)
     RadSlope%Zp(j,i)=JMatrix%INSTC(j,i)
    end do
   end do
   DiaSlope=RadSlope              ! move to diagonal format
   DiaSlope%Zpd2 = .n. DiaSlope

   do i=1,M1
    if (btest(dat,0)) then
     call SplineEval1Dx1D(10,JMatrix%R0,JMatrix%THT(i),JMatrix%INSTC(N1+1,i))  ! center value
    else
     call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT(i),JMatrix%INSTC(N1+1,i))  ! center value
   endif
   if (i .eq. 1) then
    JMatrix%INSTC0(1)=JMatrix%INSTC(N1+1,1)
   else
    JMatrix%INSTC0(1)=(i*JMatrix%INSTC0(1)+JMatrix%INSTC(N1+1,i))/(i+1)      ! cumulative average
   endif
  end do
  if (JMatrix%INSTC0(1) <= JMatrix%INSTC0(2)) JMatrix%INSTC0(2)=JMatrix%INSTC0(1)
  if (JMatrix%INSTC0(1) >= JMatrix%INSTC0(3)) JMatrix%INSTC0(3)=JMatrix%INSTC0(1)

! INSTC2
! Reload RadSlope & respline
  do i=1,M1
   do j=1,RadSlope%MV(i)
    RadSlope%Zp(j,i)=JMatrix%INSTC2(j,i)
   end do
  end do
  DiaSlope=RadSlope              ! move to diagonal format
  DiaSlope%Zpd2 = .n. DiaSlope

  do i=1,M1
   if (btest(dat,0)) then
    call SplineEval1Dx1D(10,JMatrix%R0,JMatrix%THT(i),JMatrix%INSTC2(N1+1,i))  ! center value
   else
    call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT(i),JMatrix%INSTC2(N1+1,i))  ! center value
   endif
   if (i .eq. 1) then
    JMatrix%INSTC20(1)=JMatrix%INSTC2(N1+1,1)
   else
    JMatrix%INSTC20(1)=(i*JMatrix%INSTC20(1)+JMatrix%INSTC2(N1+1,i))/(i+1)      ! cumulative average
   endif
  end do
  if (JMatrix%INSTC20(1) <= JMatrix%INSTC20(2)) JMatrix%INSTC20(2)=JMatrix%INSTC20(1)
  if (JMatrix%INSTC20(1) >= JMatrix%INSTC20(3)) JMatrix%INSTC20(3)=JMatrix%INSTC20(1)

! MEANC
! Reload RadSlope & respline
  do i=1,M1
   do j=1,RadSlope%MV(i)
    RadSlope%Zp(j,i)=JMatrix%MEANC(j,i)
   end do
  end do
  DiaSlope=RadSlope              ! move to diagonal format
  DiaSlope%Zpd2 = .n. DiaSlope

  do i=1,M1
   if (btest(dat,0)) then
    call SplineEval1Dx1D(10,JMatrix%R0,JMatrix%THT(i),JMatrix%MEANC(N1+1,i))  ! center value
   else
    call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT(i),JMatrix%MEANC(N1+1,i))  ! center value
   endif
   if (i .eq. 1) then
    JMatrix%MEANC0(1)=JMatrix%MEANC(N1+1,1)
   else
    JMatrix%MEANC0(1)=(i*JMatrix%MEANC0(1)+JMatrix%MEANC(N1+1,i))/(i+1)      ! cumulative average
   endif
  end do
  if (JMatrix%MEANC0(1) <= JMatrix%MEANC0(2)) JMatrix%MEANC0(2)=JMatrix%MEANC0(1)
  if (JMatrix%MEANC0(1) >= JMatrix%MEANC0(3)) JMatrix%MEANC0(3)=JMatrix%MEANC0(1)


! MONGEA
! Reload RadSlope & respline
  do i=1,M1
   do j=1,RadSlope%MV(i)
    RadSlope%Zp(j,i)=JMatrix%MONGEA(j,i)
   end do
  end do
  DiaSlope=RadSlope              ! move to diagonal format
  DiaSlope%Zpd2 = .n. DiaSlope

  do i=1,M1
   if (btest(dat,0)) then
    call SplineEval1Dx1D(10,JMatrix%R0,JMatrix%THT(i),JMatrix%MONGEA(N1+1,i))  ! center value
   else
    call SplineEval1Dx1D(0,JMatrix%R0,JMatrix%THT(i),JMatrix%MONGEA(N1+1,i))  ! center value
  endif
  if (i .eq. 1) then
   JMatrix%MONGEA0(1)=JMatrix%MONGEA(N1+1,1)
  else
   JMatrix%MONGEA0(1)=(i*JMatrix%MONGEA0(1)+JMatrix%MONGEA(N1+1,i))/(i+1)      ! cumulative average
  endif
 end do
 if (JMatrix%MONGEA0(1) <= JMatrix%MONGEA0(2)) JMatrix%MONGEA0(2)=JMatrix%MONGEA0(1)
 if (JMatrix%MONGEA0(1) >= JMatrix%MONGEA0(3)) JMatrix%MONGEA0(3)=JMatrix%MONGEA0(1)
! end populating JMatrix

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

if ( Testdata .eq. 1 ) then
  call MakeRadSplineCenter(0)     ! remakes RadSplineCenter(1,:)
  if (btest(dat, 0) ) then        ! use nsplineCenter to force zero slope at origin,
   DiaSlope%Zpd2 = .nc. DiaSlope ! re-spline, with center node
                                  ! changes spline but requires SplineEvalCenter
                                  ! remakes RadSplineCenter(2,:) and RadSplineCenter(3,:)
  endif
  call MakeRadSplineCenter(dat)        ! this relies on JMatrix, not the original data in RadSlope from the file
  if (btest(dat, 1)) then         ! moving each meridian to align curves
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
!   global cylindrical coordinates
    X_global=(ctr_circle_x-rlocal(kk)*cos(thtlocal(kk)))
    Y_global=(ctr_circle_y-rlocal(kk)*sin(thtlocal(kk)))
    R_global=sqrt(X_global*X_global+Y_global*Y_global)
    if (ABS(X_global) > EPS .AND. ABS(Y_global) > EPS) then
     if (X_global > 0 .AND. Y_global > 0 ) then
      Theta_global=ATan(Y_global/X_global)
     endif
     if (X_global < 0 .AND. Y_global > 0 ) then
      Theta_global=ATan(Y_global/X_global)+PI
     endif
     if (X_global < 0 .AND. Y_global < 0 ) then
      Theta_global=ATan(Y_global/X_global)+PI
     endif
     if (X_global > 0 .AND. Y_global < 0 ) then
      Theta_global=ATan(Y_global/X_global)+2*PI
     endif
    else
     if (ABS(X_global) <= EPS .AND. ABS(Y_global) > EPS) then
      if (Y_global > 0) then
       Theta_global=PI/2
      else
       Theta_global=3*PI/2
      endif
     endif
     if (ABS(X_global) > EPS .AND. ABS(Y_global) <= EPS) then
      if (X_global > 0) then
       Theta_global=0
      else
       Theta_global=PI
      endif
     endif
    endif
    if (Theta_global .gt. PI) then
     R_global = -R_global
    endif
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
     CASE (21)
     powctr=JMatrix%Warp0(1)
     powmin=JMatrix%Warp0(2)
     powmax=JMatrix%Warp0(3)
     CASE DEFAULT
     powctr=JMatrix%SAGC0(1)
     powmin=JMatrix%SAGC0(2)
     powmax=JMatrix%SAGC0(3)
   END SELECT
  endif

  dist = real(-2*JMatrix%Z0(3),kind=sk)
! generate buffer data
  pupil_elements(1:pupil_nE)=0
  pupil_vertices(1:pupil_nV)=0
  call Geom(flag, JMatrix, donut, powmin, powmax, elements, vertices, nV, nE)
  call Pupil(JMatrix, dist, pupil_elements, pupil_vertices, pupil_nV, pupil_nE)
  call makelegend(flag, powmin, powmax, legend, nL)
! eigenvalues show shape of RadSlope without make_rings but with FillArray 7 elevations
!  atmp=pca(2,RadSlope)
!  atmp=pca(3,RadSlope)

  write(*,*) "Done: janus"
  call LogC("Done: janus"//c_null_char)  !has to be C and declared, not cpp

  return        

  END subroutine janus
