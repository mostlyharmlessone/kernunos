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

    subroutine Geom(flag, b, donut, powmin, powmax, elements, vertices, nV, nE)
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
    end subroutine

    subroutine makelegend(flag, powmin, powmax, legend, nL)
     use set_precision, ONLY : wp
     use special_fct, only : colormap
     use, intrinsic :: iso_c_binding, ONLY : c_float,c_int
     use, intrinsic ::  ieee_arithmetic
     use ISO_FORTRAN_ENV, only: stdin=>input_unit     ! for the pause read(stdin,*)
     real(wp), intent(INOUT) :: powmin,powmax
     real(c_float), INTENT(INOUT) :: legend(*)
     integer(c_int), INTENT(INOUT) :: flag, nL
    end subroutine

    subroutine Pupil(b, dist, pupil_elements, pupil_vertices, pupil_nV, pupil_nE)
    use cornea_arrays, ONLY : wpJMatrix
    use set_precision, ONLY : wp
    use, intrinsic :: iso_c_binding, ONLY : c_float,c_int
    use, intrinsic ::  ieee_arithmetic
    use ISO_FORTRAN_ENV, only: stdin=>input_unit     ! for the pause read(stdin,*)
    TYPE(wpJMatrix),INTENT(IN) :: b
    integer(c_int), INTENT(INOUT) :: pupil_elements(*)                          ! faces x 3
    real(c_float), INTENT(INOUT) :: pupil_vertices(*), dist                     ! vertices x 6
    integer(c_int), INTENT(INOUT) :: pupil_nE, pupil_nV
    end subroutine

    subroutine rcnvrta(KXNAME,N,read_error)
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : Atlas, PI
     character(len=*), intent(in) :: KXNAME
     integer, intent(in) :: N
     integer, intent(out) :: read_error
    end subroutine

    subroutine rcnvrta_type(KXNAME,N,read_error)
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : Atlas, PI
     character(len=*), intent(in) :: KXNAME
     integer, intent(out) :: N, read_error
    end subroutine

    subroutine rcnvrte(read_error,RANAME,XXNAME,PUNAME,HXNAME)
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : EyeSys
     character(len=*), intent(in) :: RANAME,XXNAME
     character(len=*), intent(in), optional :: PUNAME,HXNAME
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
 use set_precision, ONLY : wp
 use cornea_arrays, ONLY : Penta
 use special_fct, ONLY : replacestr
 implicit none
 character(len=*), intent(in) :: filename
 integer, intent(in) :: TestData
 integer, intent(out) :: read_error
 integer :: unitno1,ierr,readerr,i,k,NP,read_front,meridians,file_idx
 logical :: exists
 character(len=7) :: matrixchar
 character(len=1) :: iter1,equal
 character(len=2) :: iter2
 character(len=3) :: iter3
 character(len=1000) :: somecharacter,someline
 real(wp) :: temp(141,141)
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
            write(*,*) 'Read PentaCam _CUR.CSV/_ELE.CSV header'
           else
           close(unitno1)
           read_error=2
           write(*,*) 'Could not read PentaCam _CUR.CSV/_ELE.CSV header'
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
!                but broken for semicolon delimited sometime in 2024 by ?gcc changes
                 if (TestData.eq.5) then !this works with getArg for _CUR.CSV
                  line=somecharacter
                  do i=1,NP
                   Penta%DAT(k,i) = getArg(i+1)   !can change to i or i+2 to simulate decentering here and below
                  end do
                 endif
                 if (TestData.eq.4) then !this works with getArg for _ELE.CSV
                   line=somecharacter
                   do i=1,NP
                    Penta%DAT(k,i) = 100000*getArg(i+1)
                   end do
                 endif
               endif
            else
!              write(*,*) 'Read ',k-1,' rows from ',trim(filename)
!               do k=1,NP
!                write (*,*) 'Matrix ',k-1,'= ',Penta%DAT(:,k)
!               end do
             exit     ! End of cornea data, k=NP
            endif
           end do
          endif

          if (somecharacter(1:7).eq.'[PUPIL]') then
           if (TestData .ge. 4) then  ! _CUR.CSV or _ELE.CSV
            write(*,*) 'Found pupil data in Penta _CUR.CSV or _ELE.CSV'
            read(unitno1, '(A)', iostat=readerr) someline
            read(unitno1, '(A)', iostat=readerr) someline
            somecharacter=replacestr(string=someline,search=";",substitute=",")
            read(somecharacter,*,iostat=readerr) someline,Penta%Pupil_Center(1)
            read(unitno1, '(A)', iostat=readerr) someline
            somecharacter=replacestr(string=someline,search=";",substitute=",")
            read(somecharacter,*,iostat=readerr) someline,Penta%Pupil_Center(2)
            read(unitno1, '(A)', iostat=readerr) someline
            read(unitno1, '(A)', iostat=readerr) someline
            do k=1,256
             read(unitno1, '(A)', iostat=readerr) someline
             if (readerr .ne. 0) then
              write(*,*) 'Read Error in [PUPIL]'
              read_error = 10
              return
             endif
             file_idx = 0
             file_idx=index(someline, ";")
             if (file_idx .eq. 0) then
              write(*,*) 'Error reading pupil data'
              read_error = 10
              return
             endif
             somecharacter=replacestr(string=someline,search=";",substitute=",")
             read(somecharacter,*,iostat=readerr) Penta%PU(k,1),Penta%PU(k,2)
            end do
           endif
           if (TestData .le. 3) then  ! .CUR or .ELE
            write(*,*) 'Found pupil data in Penta .CUR or .ELE'
            read(unitno1, '(A)', iostat=readerr) someline
            read(unitno1, '(A)', iostat=readerr) someline
            somecharacter=replacestr(string=someline,search="=",substitute=", ")
            read(somecharacter,*,iostat=readerr) someline,Penta%Pupil_Center(1)
            read(unitno1, '(A)', iostat=readerr) someline
            somecharacter=replacestr(string=someline,search="=",substitute=", ")
            read(somecharacter,*,iostat=readerr) someline,Penta%Pupil_Center(2)
            read(unitno1, '(A)', iostat=readerr) someline
            somecharacter=replacestr(string=someline,search="=",substitute=", ")
            read(somecharacter,*,iostat=readerr) someline,meridians
            if (size(Penta%PU,1) .ne. meridians) then
             write(*,*) 'Size mismatch in pupil meridians, 256 expected'
             read_error = 10
             return
            else
            read(unitno1, '(A)', iostat=readerr) someline
             somecharacter=replacestr(string=someline,search="=",substitute=", ")
             read(somecharacter,*,iostat=readerr) someline,Penta%PU(:,1)
            read(unitno1, '(A)', iostat=readerr) someline
             somecharacter=replacestr(string=someline,search="=",substitute=", ")
             read(somecharacter,*,iostat=readerr) someline,Penta%PU(:,2)
            endif
           endif
          endif
          if (somecharacter(1:4).eq.'HWTW' .or. somecharacter(1:4).eq.'CRC3') exit  ! End of data
         else
           exit  !EOF this doesn't work if you never leave k do loop above
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
   if (TestData .le. 3) then !.CUR/.ELE need to be flipped
   temp=Penta%DAT
    do i=1,NP
     do k=1,NP
      Penta%DAT(i,k)=temp(NP-i+1,k)
      Penta%DAT(NP-i+1,k)=temp(i,k)
     end do
    end do
   endif
!  multiple adjacent points have identical elevations, resulting in local flat surfaces and distortion of spline approximations
!   if (TestData .eq. 2 .or. TestData .eq. 4) then !_ELE.CSV/.ELE
!    do i=2,NP-1
!     do k=2,NP-1
!      if ( Penta%DAT(i,k) .ge. 0) then
!       if (Penta%DAT(i,k) .eq. Penta%DAT(i-1,k)) write (*,*) 'adjacent points: ',i,k,Penta%DAT(i,k)
!       if (Penta%DAT(i,k) .eq. Penta%DAT(i+1,k)) write (*,*) 'adjacent points: ',i,k,Penta%DAT(i,k)
!       if (Penta%DAT(i,k) .eq. Penta%DAT(i,k-1)) write (*,*) 'adjacent points: ',i,k,Penta%DAT(i,k)
!       if (Penta%DAT(i,k) .eq. Penta%DAT(i,k+1)) write (*,*) 'adjacent points: ',i,k,Penta%DAT(i,k)
!      endif
!     end do
!    end do
!   endif
end subroutine rcnvrtp

subroutine rcnvrte(read_error,RANAME,XXNAME,PUNAME,HXNAME)
! EYESYS VERSION
  USE io_functions, ONLY : get_new_fileunit
  USE set_precision, ONLY : wp
  USE cornea_arrays, ONLY : EyeSys
  USE special_fct, ONLY : replacestr
  implicit none
  logical :: exists
  character(len=*), intent(in) :: RANAME,XXNAME
  character(len=*), intent(in), optional :: PUNAME,HXNAME
  character(1000) header,header_space
  integer :: file_idx1,file_idx2,file_idx3,file_idx4,readerr
  integer, intent(out) :: read_error
  REAL(wp) :: ZX(16),YX(16),PX,CX,CY
  INTEGER :: I,J,ITH,unitno1,unitno2,unitno3,unitno4,MM,N,ierr
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
      if (file_idx1 > 0) then
       write(*,*) 'RA EyeSys header detected: ',trim(header)       
      endif
      READ (unitno2,*) header
      file_idx2=index(trim(header),"|")
      if (file_idx2 > 0) then
       write(*,*) 'XX EyeSys header detected: ',trim(header)
      else
       write(*,*) 'No XX/RA EyeSys headers detected, assuming data only'
       REWIND(unitno1)
       REWIND(unitno2)
      endif
      do I=1,MM
       if (file_idx1>0 .and. file_idx2>0) then
        READ(unitno1,*,iostat=readerr) header,ZX(:)
        if (readerr .ne. 0) then
         WRITE (*,*) 'Error on input EyeSys RA/XX files on', I,'row'
         read_error=3
         return
        endif
        READ(unitno2,*,iostat=readerr) header,YX(:)
        if (readerr .ne. 0) then
         WRITE (*,*) 'Error on input EyeSys RA/XX files on', I,'row'
         read_error=3
         return
        endif
        ITH=I-1
       else
        READ(unitno1,*,iostat=readerr) ITH,ZX(:)
        if (readerr .ne. 0) then
         WRITE (*,*) 'Error on headerless input EyeSys RA/XX files on', I,'row'
         read_error=3
         return
        endif
        READ(unitno2,*,iostat=readerr) ITH,YX(:)
        if (readerr .ne. 0) then
         WRITE (*,*) 'Error on headerless input EyeSys RA/XX files on', I,'row'
         read_error=3
         return
        endif
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
      if (Present(HXNAME)) then
       inquire(file=trim(HXNAME), exist=exists)
       if (exists) then
        unitno4 = get_new_fileunit()
        open(unitno4, file=trim(HXNAME), action="read", iostat=ierr)
        if (ierr .eq. 0) then
         READ (unitno4,*) header
         file_idx4=index(trim(header),"|")
         if (file_idx4 > 0) then
          write(*,*) 'HX EyeSys header detected: ',trim(header)
         endif
        endif
        CLOSE(unitno4)
       endif
      endif
      if (Present(PUNAME)) then
       inquire(file=trim(PUNAME), exist=exists)
       if (exists) then
        unitno3 = get_new_fileunit()
        open(unitno3, file=trim(PUNAME), action="read", iostat=ierr)
        if (ierr .eq. 0) then
         READ (unitno3,*) header
         file_idx3=index(trim(header),"|")
         if (file_idx3 > 0) then
          write(*,*) 'PU EyeSys header detected: ',trim(header)
          READ (unitno3,*) CX,CY ! next line has two numbers
         else
          write(*,*) 'No PU EyeSys header detected, assuming data only'
!         assuming a headerless PU file exists,it probably has two numbers to skip, no REWIND
         endif
        do I=1,MM
         if (file_idx3 > 0) then
          READ(unitno3,*,iostat=readerr) header
          header_space=replacestr(string=header,search=":",substitute=": ")
          READ(header_space,*,iostat=readerr) header,PX
          if (readerr .ne. 0) then
           WRITE (*,*) 'Error on input EyeSys PU file on', I,'row'
           read_error=7
           return
          endif
         else
!         headerless, assuming comma delimited
          READ(unitno3,*,iostat=readerr) ITH,PX
          if (readerr .ne. 0) then
           WRITE (*,*) 'Error on input headerless EyeSys PU file on', I,'row'
           read_error=8
           return
          endif
         endif
         EyeSys%PU(i)=PX
         end do
         EyeSys%Pupil_Center(1)=CX ; EyeSys%Pupil_Center(1)=CY
         CLOSE (unitno3)
        endif
       else
        print*, "Error ", ierr ," attempting to open file ", trim(PUNAME)
        read_error=9
        return
       endif
      endif
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

subroutine rcnvrta_type(KXNAME,N,read_error)
! determine ATLAS VERSION if 900 or 9000; N=25 or 22
 use io_functions, only : get_new_fileunit
 implicit none
 logical :: exists
 CHARACTER(80) KH1,KH2
 character(len=*), intent(in) :: KXNAME
 integer, intent(out) :: N, read_error
 INTEGER :: K,io,unitno,ierr
 read_error = 0
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
        IF (KH1.EQ.'#End_Table') THEN
         READ(unitno,*,END=100,IOSTAT=io) KH1,KH2
         IF (KH1.EQ.'Power_Rings_Count') THEN
          read(KH2,*) N
          if (N > 22) write(*,*) 'Atlas 900 file found'
          if (N < 25) write(*,*) 'Atlas 9000 file found'
          write(*,*) trim(KH1),N
         ENDIF
        ENDIF
      END DO
!      FINISHED READING ATLAS FILE
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
  RETURN
end subroutine rcnvrta_type

subroutine rcnvrta(KXNAME,N,read_error)
! ATLAS VERSION
 use io_functions, only : get_new_fileunit
 USE set_precision, ONLY : wp
 USE cornea_arrays, ONLY : Atlas, JMatrix
 implicit none
 logical :: exists
 CHARACTER(80) KH1,KH2,KH3
 character(len=*), intent(in) :: KXNAME
 integer, intent(in) :: N
 integer, intent(out) :: read_error
 INTEGER :: K,I,J,io,ITH,JTH,unitno,MM,ierr
 REAL(wp) :: R,DIST,Y,POW,Z
 read_error = 0
 MM=180
 inquire(file=trim(KXNAME), exist=exists)
 if (exists) then
  unitno = get_new_fileunit()
  open(unitno, file=trim(KXNAME), action="read", iostat=ierr)
   if (ierr .eq. 0) then                         
!   READ HEADERS
    K=0
    DO 
       K=K+1        
       READ(unitno,'(A)',END=100,IOSTAT=io) KH1
        IF(io.GT.0) THEN
         WRITE(*,*) 'I/O ERROR ON INPUT ATLAS FILE',io, 'line',K  !possibly it's the first semicolon, try sed in janus
         read_error=1
         GOTO 100
        ENDIF

        IF (K .eq. 1) THEN
         IF (KH1(1:6).EQ.'#ATLAS')THEN
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
!       There only seem to be N=22 of these, and they're of unknown usefulness in calculation; used only as error checking
        IF (KH1.EQ.'Ring') THEN
         IF (KH2.EQ.'Point'.AND.KH3.EQ.'Radius') THEN
!         DATA READ RADIUS? RING POSITION
          DO J=1,22 !use 22 here not N
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
!       ATLAS 900
        IF (N .gt. 22) then
         do J=23,N
          do I=1,MM
           Atlas%AR(I,J)=1   !make > 0 for allowing valid points in AD/AY/AP > 22
          end do
         end do
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
           if (ITH.NE.(J-1)) then
            WRITE(*,*) 'ATLAS POWER READ ERROR'
            read_error=5
            goto 100
           endif
           if (JTH.NE.(I-1)) then
            WRITE(*,*) 'ATLAS POWER POINT=THETA/2 READ ERROR'
            read_error=6
            goto 100
           endif
            Atlas%AY(JTH+1,ITH+1)=Y
            Atlas%AD(JTH+1,ITH+1)=DIST
            Atlas%AP(JTH+1,ITH+1)=POW
           end do 
          end do           
         ENDIF
        ENDIF

        IF (KH1.EQ.'#End_Table') THEN
         READ(unitno,*,END=100,IOSTAT=io) KH2,ITH
         IF (KH2 .eq. "Pupil_Data_Point") THEN
          write(*,*) 'Found Atlas Pupil data'
          READ(unitno,*,IOSTAT=io) KH1,Atlas%Pupil_Center(1)
          READ(unitno,*,IOSTAT=io) KH1,Atlas%Pupil_Center(2)
          write(*,*) "Pupil Center",Atlas%Pupil_Center(:)
          READ(unitno,'(A)',END=100,IOSTAT=io) KH1
          READ(unitno,'(A)',END=100,IOSTAT=io) KH1
          READ(unitno,'(A)',END=100,IOSTAT=io) KH1
          READ(unitno,'(A)',END=100,IOSTAT=io) KH1
          READ(unitno,'(A)',END=100,IOSTAT=io) KH1
          READ(unitno,'(A)',END=100,IOSTAT=io) KH1
          DO I=1,MM
           READ(unitno,*,END=100,IOSTAT=io) ITH,Atlas%PU(I,1),Atlas%PU(I,2)
           if (ITH.NE.(I-1)) then
            WRITE(*,*) 'ATLAS PUPIL READ ERROR'
            read_error=7
            goto 100
           endif
          END DO
         ENDIF
        ENDIF

        IF (KH1.EQ.'#End_Table') THEN
         BACKSPACE 1
         READ(unitno,*,IOSTAT=io) KH1,KH2,ITH
         IF (KH1(1:7) .EQ. 'Zernike') THEN
          if (ITH .lt. 0 .or. ITH .gt. 7) then
           WRITE(*,*) 'ZERNIKE READ ERROR'
           read_error=9
           goto 100
          endif
          WRITE(*,*) 'Zernike coefficients present, order',ITH
          if (ITH .eq. 7) JTH=35
          if (ITH .eq. 6) JTH=27
          if (ITH .eq. 5) JTH=20
          if (ITH .eq. 4) JTH=14
          if (ITH .eq. 3) JTH=9
          if (ITH .eq. 2) JTH=5
          if (ITH .eq. 1) JTH=2
          if (ITH .eq. 0) JTH=0
          READ(unitno,*,END=100,IOSTAT=io) KH1,KH2,KH3,Z
!          WRITE(*,*) 'Zernike Fit Zone',Z
          READ(unitno,*,END=100,IOSTAT=io) KH1,KH2
          READ(unitno,*,END=100,IOSTAT=io) KH1,I,Z
          JMatrix%ZC0(1,7)=Z
          DO K=1,JTH
           READ(unitno,*,END=100,IOSTAT=io) KH1,I,J,Z
!          only store the 4th order Zernikes at this point for display, uncomment to write all to log
!          WRITE(*,*) trim(KH1),I,J,Z
           if (I .eq. 1 .and. J .eq. 1 ) JMatrix%ZC0(1,10)=Z
           if (I .eq. 1 .and. J .eq. -1 ) JMatrix%ZC0(1,5)=Z
           if (I .eq. 2 .and. J .eq. -2 ) JMatrix%ZC0(1,3)=Z
           if (I .eq. 2 .and. J .eq. 0 ) JMatrix%ZC0(1,8)=Z
           if (I .eq. 2 .and. J .eq. 2 ) JMatrix%ZC0(1,12)=Z
           if (I .eq. 3 .and. J .eq. -3 ) JMatrix%ZC0(1,2)=Z
           if (I .eq. 3 .and. J .eq. -1 ) JMatrix%ZC0(1,6)=Z
           if (I .eq. 3 .and. J .eq. 1 ) JMatrix%ZC0(1,11)=Z
           if (I .eq. 3 .and. J .eq. 3 ) JMatrix%ZC0(1,14)=Z
           if (I .eq. 4 .and. J .eq. -4 ) JMatrix%ZC0(1,1)=Z
           if (I .eq. 4 .and. J .eq. -2 ) JMatrix%ZC0(1,4)=Z
           if (I .eq. 4 .and. J .eq. 0 ) JMatrix%ZC0(1,9)=Z
           if (I .eq. 4 .and. J .eq. 2 ) JMatrix%ZC0(1,13)=Z
           if (I .eq. 4 .and. J .eq. 4 ) JMatrix%ZC0(1,15)=Z
          END DO
         ENDIF
        ENDIF
       
      END DO
!      FINISHED READING ATLAS FILE
100   write(*,*) 'Read ',K,' lines in',trim(KXNAME)
      CLOSE (unitno)
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

!   POPULATE Atlas DEG
    do i=1,MM
     ITH=2*(i-1)
     Atlas%DEG(i)=ITH
    end do

    RETURN

 end subroutine rcnvrta
