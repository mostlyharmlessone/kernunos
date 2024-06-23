module io_functions
! module for opening files sanely
! these are for parsing input
  integer, parameter :: MAX_LINE = 1000    ! max size of line input
  character(MAX_LINE) :: line

   INTERFACE

    subroutine ConvertOFFtoSTL(OFFNAME,STLNAME,STLBINNAME) 
     use special_fct, only : surface_normal,rgb2attr
      use ISO_FORTRAN_ENV, only: INT8,INT16,INT32,REAL32
     character(len=*), intent(in) :: OFFNAME,STLNAME,STLBINNAME
    end subroutine

    SUBROUTINE fillarray(IuseG,KX1)
!     COMPUTES ATLAS DATA 
!     IuseG to select what to place in RadSlope%Zp AND/OR compute LIOC
      USE cornea_arrays, ONLY : RadSlope,AxialP,sagc2,instantp,meanp,mongea,lioc
      USE set_precision, ONLY : wp
      USE spline_interfaces, ONLY : SplineEval1Dx1D
      use,intrinsic :: ieee_arithmetic
      integer, intent(in) :: IuseG 
      character(len=*), intent(in) :: KX1     
    END SUBROUTINE

    SUBROUTINE Geom(flag, b, donut, powmin, powmax, elements, vertices, nV, nE)
       use cornea_arrays
       use set_precision, ONLY : wp
       use special_fct, only : rgb2, rgb5
       use, intrinsic :: iso_c_binding, ONLY : c_float,c_int
       use ISO_FORTRAN_ENV, only: stdin=>input_unit     
       TYPE(wpJMatrix),INTENT(IN) :: b
       logical, intent(IN) :: donut       
       real(wp), intent(INOUT) :: powmin,powmax 
       integer(c_int), INTENT(INOUT) :: elements(*)                          ! faces x 3   index 0
       real(c_float), INTENT(INOUT) :: vertices(*)                           ! vertices x 6
       integer(c_int), INTENT(INOUT) :: flag, nE, nV                         ! passed from janus to call OpenGL
    END SUBROUTINE

    subroutine rcnvrta(KXNAME,N,read_error)
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : Atlas, PI
     character(len=*), intent(in) :: KXNAME
     integer, intent(out) :: N, read_error
    end subroutine

    subroutine rcnvrte(RANAME,XXNAME,read_error)
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : EyeSys
     character(len=*), intent(in) :: RANAME,XXNAME
     integer, intent(out) :: read_error
    end subroutine

    subroutine rcnvrtp(TestData,filename,read_error)
     USE cornea_arrays, ONLY : Penta
     character(len=*), intent(in) :: filename
     integer, intent(in) :: TestData
     integer, intent(out) :: read_error
    end subroutine

    subroutine RCNVRTT(MM,N)
     USE set_precision, ONLY : wp
     USE cornea_arrays
     INTEGER, INTENT(IN) :: MM,N
    end subroutine

    SUBROUTINE WriteGeomOFF(flag,b,donut,powmin,powmax,OFFNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      use, intrinsic :: iso_c_binding, ONLY : c_float,c_int
      TYPE(wpJMatrix),INTENT(IN) :: b
      character(len=*), intent(in) :: OFFNAME
      real(wp), intent(IN) :: powmin,powmax
      logical, intent(IN) :: donut   
      integer(c_int), INTENT(INOUT) :: flag      
    END SUBROUTINE  
    
    SUBROUTINE WriteGeomPLY(flag,b,donut,powmin,powmax,PLYNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      use, intrinsic :: iso_c_binding, ONLY : c_float,c_int
      TYPE(wpJMatrix),INTENT(IN) :: b
      character(len=*), intent(in) :: PLYNAME
      real(wp), intent(IN) :: powmin,powmax
      logical, intent(IN) :: donut   
      integer(c_int), INTENT(INOUT) :: flag      
    END SUBROUTINE    
    
    subroutine WriteCenter(b,KXNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      TYPE(wpRadSlopeMatrix),INTENT(IN) :: b 
      character(len=*), intent(in) :: KXNAME   
    end subroutine

    subroutine WriteCenterJ(a,b,KXNAME)
     USE set_precision, ONLY : wp
     real(wp),INTENT(IN) :: a, b(:,:)
     character(len=*), intent(in) :: KXNAME
    end subroutine

    SUBROUTINE WRITEARRAY(b,KXNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
      character(len=*), intent(in) :: KXNAME 
    END SUBROUTINE

    SUBROUTINE PRINTGRAPH(unitno1,POWMIN,POWMAX,FILENAME)
     use set_precision, only : wp
     REAL(wp), INTENT(IN) :: POWMIN, POWMAX
     integer, intent(in) :: unitno1
     character(len=*), intent(in) :: FILENAME
    END SUBROUTINE
    
  END INTERFACE

 contains

! https://community.intel.com/t5/Intel-Fortran-Compiler/Trouble-reading-a-csv-file/m-p/1034136
! modified to output formatted real, as unformatted reads with semicolons seem broken with the latest gcc-fortran/gfortran
function getArg(n) result(argn)
    implicit none
    character(10) :: arg
    real :: argn
    integer :: n,i,j,count
    j = 0
    do count=1,n
        i = j + 1
        j = INDEX(line(i:),';')
        if(j == 0) exit
        j = j + i - 1
    end do
    if(j == 0) then
        if(count == n) then
            arg = line(i:)
        else
            arg = ' '
        endif
    else
        arg = line(i:j-1)
    endif
        read(arg,'(F23.5)') argn
end function getArg

  
 function get_new_fileunit() result (f)
 implicit none
 logical :: op
 integer :: f
 f = 1
 do
  inquire(f,opened=op)
  if (op .eqv. .false.) exit
  f = f + 1
 end do
 end function
  
end module io_functions

subroutine rcnvrtp(TestData,filename,read_error)
! PENTACAM VERSION FOR ALL
 use io_functions, only : get_new_fileunit,getArg,line
 use cornea_arrays, ONLY : Penta
 implicit none
 character(len=*), intent(in) :: filename
 integer, intent(in) :: TestData
 integer, intent(out) :: read_error
 integer :: unitno1,ierr,readerr,i,k,NP,read_front
 logical :: exists
 character(len=7) :: matrixchar
 character(len=1) :: iter1,equal
 character(len=2) :: iter2
 character(len=3) :: iter3
 character(len=1000) :: somecharacter
 NP=141
    inquire(file=trim(filename), exist=exists)
    if (exists) then
     unitno1 = get_new_fileunit()
     open(unitno1, file=trim(filename), action="read", iostat=ierr)
     if (ierr .eq. 0) then
     read_front=0
     i=0
     Penta%DAT(:,:)=0   ! zero out data matrix
     do
      i=i+1
      read(unitno1, '(A)', iostat=readerr) somecharacter
         if (readerr .eq. 0) then

          if (somecharacter.eq.'[SYSTEM]'.and.(i.eq.1)) then   !testdata 2 or 3
           if (TestData .eq. 2 .or. TestData .eq. 3) then
             write(*,*) 'Read PentaCam CUR/ELE header'
           else
             close(unitno1)
             read_error=1
             write(*,*) 'Could not read PentaCam CUR/ELE header'
             return
           endif
          endif
          if (somecharacter(1:5).eq.'FRONT'.and.(i.eq.1)) then  !testdata 4 or 5
           if (TestData .eq. 4 .or. TestData .eq. 5) then
            write(*,*) 'Read PentaCam CUR.CSV/ELE.CSV header'
           else
           close(unitno1)
           read_error=2
           write(*,*) 'Could not read PentaCam CUR.CSV/ELE.CSV header'
           return
           endif
          endif

          if ((somecharacter.eq."Matrixsize Y=141" .and. read_front.eq.0 .and. TestData.le.3) &
               .or. (read_front.eq.0 .and. TestData.ge.4) ) then
!           print*, "Char in file ", trim(filename), " is ", somecharacter
            k=0 ; read_front=1   ! only read the front elevations or curvatures
           do
            k=k+1
           if (TestData.eq.4 .or. TestData.eq.5) then
             read(unitno1,'(A)',iostat=readerr) somecharacter
           endif
           if (TestData.eq.2 .or. TestData.eq.3) then
            if (k <= 10 ) then
             read(unitno1,'(A,A,A,A)',iostat=readerr) matrixchar,iter1,equal,somecharacter
            endif
            if (k <= 100 .AND. k > 10 ) then           
               read(unitno1,'(A,A,A,A)',iostat=readerr) matrixchar,iter2,equal,somecharacter  
            endif
            if ( k > 100 .AND. k <= NP ) then
               read(unitno1,'(A,A,A,A)',iostat=readerr) matrixchar,iter3,equal,somecharacter
            endif
           endif
            if (k <= NP ) then
               if (readerr .eq. 0) then  ! reads till end of data matches
                 read (somecharacter,*,iostat=readerr) (Penta%DAT(k,i),i=1,NP) !somecharacter read from file above, works for comma-delimited
!                but broken for semicolon delimited sometime in 2024 by gcc changes
                 if (TestData.eq.5) then !this works with getArg for .CUR.CSV
                  line=somecharacter
                  do i=1,NP
                   Penta%DAT(k,i) = getArg(i)
                  end do
                  endif
                  if (TestData.eq.4) then !this works with getArg for _ELE.CSV
                   line=somecharacter
                   do i=1,NP
                    Penta%DAT(k,i) = 100000*getArg(i)
                   end do
                  endif
!               if (k == 76) then
!                write(*,*) Penta%DAT(k,:)
!               endif
               endif  
             else
!                  write(*,*) 'Read ',k-1,' rows from ',trim(filename)
!                  do k=1,NP
!                   write (*,*) 'Matrix ',k-1,'= ',Penta%DAT(:,k)
!                  end do
               exit  ! End of data         
             endif        
           end do 
           endif
         else                       
           exit  !EOF
         endif        
      end do  
      close(unitno1) 
!     First column is invalid for _CUR.CSV and _ELE.CSV files, does no harm for .ELE and .CUR
      Penta%DAT(:,1)=0
      else
         print*, "Error ", ierr ," attempting to open file ", trim(filename)
         read_error=3
        return
    endif
    else
     print*, "Error -- cannot find PentaCam file: ", trim(filename)
     read_error=4
     return
   endif
end subroutine rcnvrtp


subroutine rcnvrte(RANAME,XXNAME,read_error)
! EYESYS VERSION
  use io_functions, only : get_new_fileunit
  USE set_precision, ONLY : wp
  USE cornea_arrays, ONLY : EyeSys
  implicit none
  logical :: exists
  character(len=*), intent(in) :: RANAME,XXNAME
  character(1000) header
  integer :: file_idx1,file_idx2
  integer, intent(out) :: read_error
  REAL(wp) :: ZX(16),YX(16)
  INTEGER :: I,J,ITH,unitno1,unitno2,MM,N,ierr
  MM=360
  N=16
  inquire(file=trim(RANAME), exist=exists)
  if (exists) then
   unitno1 = get_new_fileunit()
   open(unitno1, file=trim(RANAME), action="read", iostat=ierr)
   if (ierr .eq. 0) then            
    inquire(file=trim(XXNAME), exist=exists)    
    if (exists) then
     unitno2 = get_new_fileunit()
     open(unitno2, file=trim(XXNAME), action="read", iostat=ierr)      
     if (ierr .eq. 0) then
      READ (unitno1,*) header
      file_idx1=index(trim(header),"|")
      if (file_idx1>0) then
       write(*,*) 'RA EyeSys header detected: ',trim(header)
      endif
      READ (unitno2,*) header
      file_idx2=index(trim(header),"|")
      if (file_idx2>0) then
       write(*,*) 'XX EyeSys header detected: ',trim(header)
      else
       write(*,*) 'No EyeSys header detected, assuming data only'
       REWIND(unitno1)
       REWIND(unitno2)
      endif
      do I=1,MM
       if (file_idx1>0 .and. file_idx2>0) then
        READ(unitno1,*) header,ZX(:)
        READ(unitno2,*) header,YX(:)
        ITH=I-1
       else
        READ(unitno1,*) ITH,ZX(:)
        READ(unitno2,*) ITH,YX(:)
       endif
       do J=1,N
        EyeSys%RA(i,j)=ZX(j)
        EyeSys%XX(i,j)=YX(j)
!       Sanity check on file data
        if (YX(J) > 0 .AND. ZX(J) > 0) then
         if (YX(J) <= ZX(J)) then
          WRITE (*,*) 'Error on input EyeSys RA/XX files ArcTan'
          read_error=1
          return
          endif
         endif
       end do
       if (ITH == (I-1)) then
        EyeSys%DEG(i)=ITH
       else
        WRITE (*,*) 'Error on input EyeSys RA/XX files with ITH'
        read_error=2
        return
       endif
      end do 
      CLOSE (unitno1)
      CLOSE (unitno2)
      else
         print*, "Error ", ierr ," attempting to open file ", trim(XXNAME)
         read_error=3
        return
      endif         
     else
      print*, "Error -- cannot find file: ", trim(XXNAME)
      read_error=4
      return
     endif 
    else
     print*, "Error ", ierr ," attempting to open file ", trim(RANAME)
     read_error=5
     return
    endif       
   else
    print*, "Error -- cannot find file: ", trim(RANAME)
    read_error=6
    return
   endif              
end subroutine rcnvrte

subroutine rcnvrta(KXNAME,N,read_error)
! ATLAS VERSION
 use io_functions, only : get_new_fileunit
 USE set_precision, ONLY : wp
 USE cornea_arrays, ONLY : Atlas
 implicit none
 logical :: exists
 CHARACTER(80) KH1,KH2,KH3
 character(len=*), intent(in) :: KXNAME
 integer, intent(out) :: N, read_error
 INTEGER :: K,I,J,io,ITH,JTH,unitno,MM,ierr
 REAL(wp) :: R,DIST,Y,POW
 MM=180
 N=22
 inquire(file=trim(KXNAME), exist=exists)
 if (exists) then
  unitno = get_new_fileunit()
  open(unitno, file=trim(KXNAME), action="read", iostat=ierr)
   if (ierr .eq. 0) then                         
!   READ HEADERS
    K=0
    DO 
       K=K+1        
       READ(unitno,*,END=100,IOSTAT=io) KH1
        IF(io.GT.0) THEN
         WRITE(*,*) 'I/O ERROR ON INPUT ATLAS FILE',io, 'line',K  !possibly it's the first semicolon, try sed in janus
         read_error=1
         GOTO 100
        ENDIF

        IF (K .eq. 1) THEN
         IF (KH1.EQ.'#ATLAS')THEN
          WRITE(*,*) 'Atlas header read'
         else
          WRITE(*,*) 'ERROR - Could not read Atlas header'
          read_error=2
          goto 100
         endif
        endif

        IF (KH1.EQ.'#Begin_Table') THEN
         READ(unitno,*,END=100,IOSTAT=io) KH1
         READ(unitno,*,END=100,IOSTAT=io) KH1
         READ(unitno,*,END=100,IOSTAT=io) KH1,KH2,KH3
        ENDIF
!       There only seem to be N=22 of these, and they're of unknown usefulness in calculation
        IF (KH1.EQ.'Ring') THEN
         IF (KH2.EQ.'Point'.AND.KH3.EQ.'Radius') THEN
!         DATA READ RADIUS? RING POSITION
          DO J=1,N
           DO I=1,MM
!          RING, POINT(0-180), RADIUS  
            READ(unitno,*) ITH,JTH,R
!          IN DEGREES
!	   THETA=2*JTH 
!          RING NUMBERS
!          SHOULD ALWAYS BE TRUE: ITH.EQ.(J-1) & JTH.NE.(I-1)
           IF (ITH.NE.(J-1)) then
             WRITE(*,*) 'ATLAS RADIUS READ ERROR'
             read_error=3
             goto 100
            endif
           IF (JTH.NE.(I-1)) then
            WRITE(*,*) 'ATLAS RADIUS POINT=THETA/2 READ ERROR'
            read_error=4
            goto 100
           endif
            Atlas%AR(JTH+1,ITH+1)=R
           end do 
          end do           
         ENDIF
        ENDIF  

        IF (KH1.EQ.'#End_Table') THEN
         READ(unitno,*,END=100,IOSTAT=io) KH1,KH2
         IF (KH1.EQ.'Power_Rings_Count') THEN
          read(KH2,*) N
          if (N > 22) write(*,*) 'Atlas 900 file'
          if (N < 25) write(*,*) 'Atlas 9000 file'
          write(*,*) trim(KH1),N
         ENDIF
        ENDIF

        IF (KH1.EQ.'Ring') THEN
         IF (KH2.EQ.'Point'.AND.KH3.EQ.'Distance(MM)') THEN       
!         DATA READ POWER
          DO J=1,N
           DO I=1,MM
!          RING,POINT(0-180),DISTANCE,ELEVATION,POWER,CHARACTER,CHARACTER 
!          DISTANCE OR RADIUS? ABOVE FOR EACH RING
           READ(unitno,*,END=100,IOSTAT=io) ITH,JTH,DIST,Y,POW,KH1,KH2
!          SHOULD ALWAYS BE TRUE: ITH.EQ.(J-1) & JTH.NE.(I-1)
           IF (ITH.NE.(J-1)) then
            WRITE(*,*) 'ATLAS POWER READ ERROR'
            read_error=5
            goto 100
           endif
           IF (JTH.NE.(I-1)) then
            WRITE(*,*) 'ATLAS POWER POINT=THETA/2 READ ERROR'
            read_error=6
            goto 100
           endif
!          UNLIKELY TO NEED ELEVATION
            Atlas%AY(JTH+1,ITH+1)=Y
            Atlas%AD(JTH+1,ITH+1)=DIST
            Atlas%AP(JTH+1,ITH+1)=POW
           end do 
          end do           
         ENDIF
        ENDIF
       
      END DO
!      FINISHED READING ATLAS FILE
      write(*,*) 'Read ',K,' lines in',trim(KXNAME)     
100   CLOSE (unitno)
      else
       print*, "Error ", ierr ," attempting to open file ", trim(KXNAME)
       read_error=7
       return
      endif       
    else
     print*, "Error -- cannot find file: ", trim(KXNAME)
     read_error=8
     return
    endif

!      POPULATE Atlas DEG
       do i=1,MM
        ITH=2*(i-1)
        Atlas%DEG(i)=ITH
       end do
             
       RETURN
       
end subroutine rcnvrta

