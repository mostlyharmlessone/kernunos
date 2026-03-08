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

    subroutine rcnvrtn(read_error,EDNAME,RANAME,HTNAME,PENAME)
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : EyeSys
     character(len=*), intent(in) :: RANAME,EDNAME
     character(len=*), intent(in), optional :: PENAME,HTNAME
     integer, intent(out) :: read_error
    end subroutine

    subroutine rcnvrtn_binary(read_error,mirecount,EDNAME,RANAME,PENAME)
     USE set_precision, ONLY : wp
     character(len=*), intent(in) :: RANAME,EDNAME
     character(len=*), intent(in), optional :: PENAME
     integer, intent(out) :: read_error
     integer, intent(out) :: mirecount
    end subroutine

    subroutine rcnvrtp(TestData,filename,read_error)
     USE cornea_arrays, ONLY : Penta
     character(len=*), intent(in) :: filename
     integer, intent(in) :: TestData
     integer, intent(out) :: read_error
    end subroutine

    subroutine rcnvrtk(read_error,ELEVNAME,CURVNAME,PUPILNAME,CENTERNAME,ZERNIKENAME,PATIENTNAME,EXAMNAME)
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : Oculus, EPS, JMatrix
     integer, intent(out) :: read_error
     character(len=*), intent(in), optional :: CURVNAME,ELEVNAME,PUPILNAME,CENTERNAME,ZERNIKENAME,PATIENTNAME,EXAMNAME
    end subroutine rcnvrtk

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
! modified to output formatted real, as unformatted reads with semicolons seems broken to me with gcc-fortran/gfortran
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

!https://fortran-lang.discourse.group/t/joining-strings-problem-with-gfortran/492

module util_mod
implicit none
contains
 function join(words) result(str)
! trim and concatenate a vector of character variables
 character (len=*), intent(in) :: words(:)
 character(:), allocatable :: str
 integer :: i,nw
 allocate(character(sum(len_trim(words)))::str)
 nw  = size(words)
 str = ""
 if (nw < 1) then
  return
 else
  str = words(1)
 end if
 do i=2,nw
  str = trim(str) // words(i)
 end do
 end function join

 function c(x1,x2) result(vec)
! return character array containing present arguments
 character (len=*)  , intent(in), optional    :: x1,x2
 character (len=1000)            , allocatable :: vec(:)
 character (len=1000)            , allocatable :: vec_(:)
 integer                                      :: n
 allocate (vec_(2))
 if (present(x1))  vec_(1)  = x1
 if (present(x2))  vec_(2)  = x2
 n = count([present(x1),present(x2)])
 if (n > 0) vec = vec_(:n)
 end function c
end module util_mod

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

subroutine rcnvrtk(read_error,ELEVNAME,CURVNAME,PUPILNAME,CENTERNAME,ZERNIKENAME,PATIENTNAME,EXAMNAME)
! Oculus Keratograph version
  USE io_functions, ONLY : get_new_fileunit
  USE set_precision, ONLY : wp
  USE cornea_arrays, ONLY : Oculus, EPS, JMatrix
  use util_mod
  USE special_fct, ONLY : replacestr
  use c_interfaces, ONLY : charcount
  USE, INTRINSIC :: iso_c_binding, ONLY : c_int,c_null_char
  implicit none
  logical :: exists, exists2, exists3, valid, name_match, date_match
  character(len=*), intent(in), optional :: ELEVNAME,CURVNAME,PUPILNAME,CENTERNAME,ZERNIKENAME,PATIENTNAME,EXAMNAME
  integer, intent(out) :: read_error
  integer :: i,j,read_front,ierr,unitno1,unitno2,unitno3,grad,file_idx
  real(wp) :: rsag,rtan,ytemp,xtemp
  Character(len=1000) :: someline,somecharacter
  integer :: linecount
  character(1000) header
  character :: ch
  character(:), allocatable :: x, y
  integer :: posmax
  INTEGER :: pos
  REAL (wp) :: ZX(45)
  integer line(200),ix,iy

    if(present(CURVNAME)) then
     inquire(file=trim(CURVNAME), exist=exists)
     if (exists) then
      write(*,*) 'Found ',CURVNAME
      unitno1 = get_new_fileunit()
      open(unitno1, file=trim(CURVNAME), action="read", iostat=ierr)
      if (ierr .eq. 0) then
       read_front=0
       i=0
       do
        i=i+1
        read(unitno1, '(A)', iostat=ierr) someline
!       should look like this
!       Seg: 0  y= 0.00  r(Sag)=  8.11  r(Tan)=  8.00
        if (ierr .eq. 0) then
         somecharacter=replacestr(string=someline,search="Seg:",substitute="")
         somecharacter=replacestr(string=somecharacter,search="y=",substitute=",")
         somecharacter=replacestr(string=somecharacter,search="r(Sag)=",substitute=",")
         somecharacter=replacestr(string=somecharacter,search="r(Tan)=",substitute=",")
         read(somecharacter,*,iostat=ierr) grad,ytemp,rsag,rtan
         Oculus%SEG(grad+1)=grad+1
         j=INT(10*ytemp)+1
         if (ABS(10*ytemp+1-j) .gt. EPS) then  ! y should always be between 0.0 and 6.0 at 0.1 intervals
          write(*,*) 'Error reading Keratograph'
          return
         endif
         Oculus%Y(grad+1,j) = ytemp
         Oculus%SAGC(grad+1,j)=rsag
         Oculus%INSTC(grad+1,j)=rtan
!         write(*,*) someline
!         write(*,*) somecharacter
!         write(*,*) Oculus%SEG(grad+1),j,i,Oculus%Y(grad+1,j),Oculus%SAGC(grad+1,j),Oculus%INSTC(grad+1,j)
        else
          exit  !EOF
        endif
       end do
       close(unitno1)
      else
       write(*,*) "Error Reading ",CURVNAME
       return
      endif
     else
      write(*,*) "No Keratograph CURVAT file ",CURVNAME
     endif
    endif

    if(present(ELEVNAME)) then
     inquire(file=trim(ELEVNAME), exist=exists)
     if (exists) then
      write(*,*) 'Found ',ELEVNAME
      unitno1 = get_new_fileunit()
      open(unitno1, file=trim(ELEVNAME), action="read", iostat=ierr)
      if (ierr .eq. 0) then
       read_front=0
       i=0
       do
        i=i+1
        read(unitno1, '(A)', iostat=ierr) someline
!       should look like this
!       Seg: 0 y= 0.00 x= 0.000000000000
        if (ierr .eq. 0) then
         somecharacter=replacestr(string=someline,search="Seg:",substitute="")
         somecharacter=replacestr(string=somecharacter,search="y=",substitute=",")
         somecharacter=replacestr(string=somecharacter,search="x=",substitute=",")
         read(somecharacter,*,iostat=ierr) grad,ytemp,xtemp
         Oculus%SEG(grad+1)=grad+1
         j=INT(10*ytemp)+1
         if (ABS(10*ytemp+1-j) .gt. EPS) then  ! y should always be between 0.0 and 6.0 at 0.1 intervals
          write(*,*) 'Error reading Keratograph'
          return
         endif
         Oculus%Y(grad+1,j) = ytemp
         Oculus%ELE(grad+1,j)=xtemp
!         write(*,*) someline
!         write(*,*) somecharacter
!         write(*,*) Oculus%SEG(grad+1),j,i,Oculus%Y(grad+1,j),Oculus%ELE(grad+1,j)
        else
          exit  !EOF
        endif
       end do
       close(unitno1)
      else
       write(*,*) "Error Reading ",ELEVNAME
       return
      endif
     else
      write(*,*) "No Keratograph CORNEA file ",ELEVNAME
     endif
    endif

    if(present(PUPILNAME)) then
     inquire(file=trim(PUPILNAME), exist=exists)
     if (exists) then
      write(*,*) 'Found ',PUPILNAME
      unitno1 = get_new_fileunit()
      open(unitno1, file=trim(PUPILNAME), action="read", iostat=ierr)
      if (ierr .eq. 0) then
       read_front=0
       i=0
       do
        i=i+1
        read(unitno1, '(A)', iostat=ierr) someline
!       should look like this
!       Seg: 0 y= 1.89
        if (ierr .eq. 0) then
         somecharacter=replacestr(string=someline,search="Seg:",substitute="")
         somecharacter=replacestr(string=somecharacter,search="y=",substitute=",")
         read(somecharacter,*,iostat=ierr) grad,ytemp
         Oculus%SEG(grad+1)=grad+1
         Oculus%PU(grad+1) = ytemp
!         write(*,*) someline
!         write(*,*) somecharacter
!         write(*,*) Oculus%SEG(grad+1),j,i,Oculus%PU(grad+1)
        else
          exit  !EOF
        endif
       end do
       close(unitno1)
      else
       write(*,*) "Error Reading ",PUPILNAME
       return
      endif
     else
      write(*,*) "No Keratograph PUPIL file ",PUPILNAME
     endif
    endif

    if(present(CENTERNAME)) then
     inquire(file=trim(CENTERNAME), exist=exists)
     if (exists) then
      write(*,*) 'Found ',CENTERNAME
      unitno1 = get_new_fileunit()
      open(unitno1, file=trim(CENTERNAME), action="read", iostat=ierr)
      if (ierr .eq. 0) then
       read_front=0
       i=0
       do
        i=i+1
        read(unitno1, '(A)', iostat=ierr) someline
!       should look like this
!       Iris Center : x=-0.34
!       Iris Center : y=-0.26
!       Iris Diameter : 11.45
!       Pupil Center : x=-0.49
!       Pupil Center : y=0.49
!       Pupil Diameter : 4.85
        if (ierr .eq. 0) then
         file_idx=index(someline, "Pupil Center : x=")
         if (file_idx .ne. 0) then
          somecharacter=replacestr(string=someline,search="Pupil Center : x=",substitute="")
          read(somecharacter,*,iostat=ierr) Oculus%Pupil_Center(1)
         endif
         file_idx=index(someline, "Pupil Center : y=")
         if (file_idx .ne. 0) then
          somecharacter=replacestr(string=someline,search="Pupil Center : y=",substitute="")
          read(somecharacter,*,iostat=ierr) Oculus%Pupil_Center(2)
         endif
         file_idx=index(someline, "Pupil Diameter :")
         if (file_idx .ne. 0) then
          somecharacter=replacestr(string=someline,search="Pupil Diameter : ",substitute="")
          read(somecharacter,*,iostat=ierr) Oculus%Pupil_Center(3)
         endif
!         write(*,*) someline
!         write(*,*) somecharacter
!         write(*,*) Oculus%Pupil_Center(1),Oculus%Pupil_Center(2),Oculus%Pupil_Center(3)
        else
          exit  !EOF
        endif
       end do
       close(unitno1)
      else
       write(*,*) "Error Reading ",CENTERNAME
       return
      endif
     else
      write(*,*) "No Keratograph CENTER file ",CENTERNAME
     endif
    endif

!   first row is semicolon and colon delimited categories
!   semicolon delimited file with 49 columns, last 45 are 8th order Zernike coefficents
!   assumes at least one set of data points
!   only reads 2nd row with first set of data points, no way to select date of data points at this time on 3rd or 4the row etc. if present
!   if presented with time/date stamp on other Keratograph files, then could pick a date.
!   have to read Right eye/left eye and correlate with OD or OS; also could read PATIENT.TXT and EXAM.TXT to verify name and time of exam
    if(present(ZERNIKENAME) .and. present(PATIENTNAME) .and. present(EXAMNAME)) then
     inquire(file=trim(ZERNIKENAME), exist=exists)
     inquire(file=trim(PATIENTNAME), exist=exists2)
     inquire(file=trim(EXAMNAME), exist=exists3)
     if (exists .and. exists2 .and. exists3) then
      write(*,*) 'Found ',ZERNIKENAME,' ',PATIENTNAME,' ',EXAMNAME
      unitno1 = get_new_fileunit()
      unitno2 = get_new_fileunit()
      unitno3 = get_new_fileunit()
      open(unitno2, file=trim(PATIENTNAME), action="read", iostat=ierr)
      if (ierr .eq. 0) then
       read(unitno2, '(A)', iostat=ierr) someline
       close(unitno2)
      else
       write(*,*) 'Error reading ',PATIENTNAME
       read_error=5
       return
      endif
      open(unitno3, file=trim(EXAMNAME), action="read", iostat=ierr)
      if (ierr .eq. 0) then
       read(unitno3, '(A)', iostat=ierr) somecharacter
       close(unitno3)
      else
       write(*,*) 'Error reading ',EXAMNAME
       read_error=5
       return
      endif
      write(*,*) 'Patient name ',trim(someline)
      write(*,*) 'Exam date ',trim(somecharacter)
!     read as binary because of the semicolons
      open(unitno1, file=trim(ZERNIKENAME), status='old', ACCESS='stream', iostat=ierr)
      if (ierr .eq. 0) then
            ch = ' ' ;   x=""
            x = join(c(ch,x)) ; pos = 0
            DO WHILE (ierr == 0)
             READ(unitno1,iostat=ierr) ch
             if (ierr == 0 ) then
              if (iachar(ch) .ne. 10) then  !  0D 0A ends each line
               x = join(c(x,ch))
               pos = pos + 1
              else
          !   found the 0D
               if (x(1:9) .eq. 'LastName:') then
                write(*,*) 'Zernike header found'
!               write(*,*) 'Zernike header ', x
               else
                write(*,*) 'FATAL Error: Unexpected Zernike header found'
                read_error = -1000
                close(unitno1)
                return
               endif
               x=trim(x) ; j = 0
               do i=1,len(x)-1
                READ(x(i:i+1),*,iostat=ierr) ch
                if (ierr == 0) then
                if (ch .eq. ';') j=j+1
!                 write(*,*) ch,i,j
                else
                 exit
                endif
               end do
               exit
              endif
             else
          !  EOF or other read error
              exit
             endif
            END DO
            write(*,*) 'Zernike header length, items',pos,j
            x = ""
          ! READ the data
             pos = 0 ; i=0 ; j=0 ; linecount = 0 ; posmax = 0
             DO
              READ(unitno1,iostat=ierr) ch
              POS=POS+1
              if (ierr == 0 ) then
               write(header,'(z0)') ch
               x = join(c(x,trim(header)))
               line(pos-i)=iachar(ch)
               if (pos-i-1 .ge. 1) then
                if (line(pos-i) .eq. 59 .or. line(pos-i) .eq. 10  ) then  !  3B = ; semicolon delimited data or EOL .and. line(pos-i-1) .eq. 13 removed because edited file under linux may only have 0A
                 j = j+1
                 i = 1
!                 Converts ASCII to hex subtract and leave as decimal digit
                  y=""
                  do ix=0,len(trim(x))-2,2
                   y = join(c(y,achar(iy)))
                   read(x(i+ix:i+ix+1),'(z2)') iy
                  end do
                  if (mod(j-1,93) .eq. 0) then
!                   write(*,*) 'Name found: ', y(2:len(trim(y)))
                   name_match = (y(2:len(trim(y))) .eq. trim(someline))
                  endif
                  if (mod(j-1,93) .eq. 5) then
                   date_match = (index(trim(somecharacter),replacestr(string=(y(2:len(trim(y))-4)//y(len(trim(y))-1:len(trim(y)))),search="/",substitute=".")) .gt. 0)
                  endif
                  if (mod(j-1,93) .eq. 7) then
!                   write(*,*) 'Eye: ',y(2:len(trim(y)))
                   if (y(2:len(trim(y))) .eq. 'Right') then
                    if (index(PUPILNAME, 'OD') .gt. 0) then
                     valid = .true.
                    else
                     valid = .false.
                    endif
                    else
                    if (y(2:len(trim(y))) .eq. 'Left') then
                     if (index(PUPILNAME, 'OS') .gt. 0) then
                      valid = .true.
                     else
                      valid = .false.
                     endif
                    else
                     write(*,*) 'Error reading right or left ',ZERNIKENAME
                    endif
                   endif
                  endif
!                 Only reads the Zernike data if correct eye and same name as PATIENT.TXT
!                 Converts ASCII to hex subtract and leave as decimal digit
                  if (mod(j-1,93)+1 .ge. 49 .and. valid .and. name_match) then
                   read(y(2:len(trim(y))),*) zx(mod(j-1,93)-47)
                  endif
!                  write(*,*) j,mod(j-1,93)+1,linecount
!                  write(*,*) y(2:len(trim(y)))
                 posmax=max(posmax,pos-i)
                 if (line(pos-i) .eq. 10 ) then ! .and. line(pos-i-1) .eq. 13  removed because edited file under linux may only have 0A
                  linecount=linecount+1
                  if (valid .and. name_match .and. date_match) then
                    write(*,*) 'Zernike Data read with valid name, eye and date'
                    exit
                  endif
                 endif
                 x ="" ; i=1 ; pos=1 ; y=""
                endif
               endif
              else
            !  EOF or other read error
               if (j/linecount .ne. 93) then
                read_error = -1000
                write(*,*) 'FATAL Error reading file ', ZERNIKENAME
                close(unitno1)
                exit
               endif
               write(*,*) j,'data items'
               write(*,*) linecount, ' lines read'
               write(*,*) j/linecount, ' items per line'
               write(*,*) posmax, ' maximum data size per item'
               if (.not. date_match) write(*,*) 'Zernike data date does not match exam date'
               if (.not. valid) write(*,*) 'Zernike data not read, no matching eye'
               exit
              endif
             end do
      else
        print*, "Error ", ierr ," attempting to open file ", trim(ZERNIKENAME)
        read_error=5
        return
      endif
     else
      print*, "Error -- cannot find file: ", trim(ZERNIKENAME)
      read_error=6
      return
     endif
     write(*,*) 'Read file ', trim(ZERNIKENAME)
     close(unitno1)
!    only store the 4th order Zernikes at this point for display
!    zx index vs i,j
!    first number comes from header position, 48 is offset for zx array
     JMatrix%ZC0(1,15)=zx(59-48)  !4,4
     JMatrix%ZC0(1,13)=zx(60-48)  !4,2
     JMatrix%ZC0(1,9) =zx(61-48)  !4,0
     JMatrix%ZC0(1,4) =zx(62-48)  !4,-2
     JMatrix%ZC0(1,1) =zx(63-48)  !4,-4
     JMatrix%ZC0(1,14)=zx(55-48)  !3,3
     JMatrix%ZC0(1,11)=zx(56-48)  !3,1
     JMatrix%ZC0(1,6)= zx(57-48)  !3,-1
     JMatrix%ZC0(1,2)= zx(58-48)  !3,-3
     JMatrix%ZC0(1,12)=zx(52-48)  !2,2
     JMatrix%ZC0(1,8)= zx(53-48)  !2,0
     JMatrix%ZC0(1,3)= zx(54-48)  !2,-2
     JMatrix%ZC0(1,10)=zx(50-48)  !1,1
     JMatrix%ZC0(1,5)= zx(51-48)  !1,-1
     JMatrix%ZC0(1,7)= zx(49-48)  !0,0
    else
     write(*,*) 'Missing ',ZERNIKENAME,' ',PATIENTNAME,' ',EXAMNAME
    endif
 end subroutine rcnvrtk

subroutine rcnvrtn(read_error,RANAME,EDNAME,HTNAME,PENAME)
! NIDEK VERSION
! ASCII version
! duplicates Janus in counting mires for 23 to 33
! only reads ED, RA, HT and PE files
! uses EyeSys cornea_array storage files
  USE io_functions, ONLY : get_new_fileunit
  USE set_precision, ONLY : wp
  USE cornea_arrays, ONLY : EyeSys
  USE special_fct, ONLY : replacestr
  use c_interfaces, ONLY : charcount
  USE, INTRINSIC :: iso_c_binding, ONLY : c_int,c_null_char
  implicit none
  logical :: exists
  character(len=*), intent(in) :: RANAME,EDNAME
  character(len=*), intent(in), optional :: PENAME,HTNAME
  character(1000) header,semicolon1,semicolon2
  integer :: file_idx1,file_idx2,file_idx3,file_idx4,readerr,io
  integer, intent(out) :: read_error
  REAL(wp), ALLOCATABLE :: ZX(:),YX(:)
  REAL(wp) :: PX,CX,CY
  INTEGER :: I,J,ITH,unitno1,unitno2,unitno3,unitno4,MM,N,ierr
  integer(c_int) :: periodcount
  MM=360
  inquire(file=trim(EDNAME), exist=exists)
  if (exists) then
   unitno1 = get_new_fileunit()
   open(unitno1, file=trim(EDNAME), action="read", iostat=ierr)
   if (ierr .eq. 0) then
    inquire(file=trim(RANAME), exist=exists)
    if (exists) then
     unitno2 = get_new_fileunit()
     open(unitno2, file=trim(RANAME), action="read", iostat=ierr)
     if (ierr .eq. 0) then
      READ (unitno1,*) header
      file_idx1=index(trim(header),EDNAME(index(EDNAME,"ED"):len(EDNAME)) // ";")
      if (file_idx1 > 0) then
       write(*,*) 'ED Nidek header detected: ',trim(header)
      endif
      READ (unitno2,*) header
      file_idx2=index(trim(header),RANAME(index(RANAME,"RA"):len(RANAME)) // ";")
      if (file_idx2 > 0) then
       write(*,*) 'RA Nidek header detected: ',trim(header)
      else
       write(*,*) 'No RA/ED Nidek headers detected'
       return
      endif
      close(unitno1)
      close(unitno2)
      semicolon1 = trim(EDNAME)
      write(*,*) 'Remove the semicolons with sed because formatted Fortran reads hate them'
      semicolon1=replacestr(string=semicolon1,search=".DAT",substitute=".TMP")
!      write(*,*) 'sed "s/;/ /g" ' // EDNAME // ' > ' // semicolon1
      call execute_command_line ('sed "s/;/ /g" ' // EDNAME // ' > ' // semicolon1, exitstat=io)
      if (io > 0) then
       write (*,*) 'system command to sed failed'
       write (*,*) 'Consider using your text editor to search/replace all semicolons in data statements in',EDNAME
       write (*,*) 'sed also fails on pathnames with spaces'
       read_error=11
       return
      endif
      semicolon2 = trim(RANAME)
      semicolon2=replacestr(string=RANAME,search=".DAT",substitute=".TMP")
!      write(*,*) 'sed "s/;/ /g" ' // RANAME // ' > ' // semicolon2
      call execute_command_line ('sed "s/;/ /g" ' // RANAME // ' > ' // semicolon2, exitstat=io)
      if (io > 0) then
       write (*,*) 'system command to sed failed'
       write (*,*) 'Consider using your text editor to search/replace all semicolons in data statements in',EDNAME
       write (*,*) 'sed also fails on pathnames with spaces'
       read_error=11
       return
      endif
!     Calculate number of mires by counting the floating point periods in the file, subtracting the header file extension, and dividing by 360
      periodcount=charcount(trim(RANAME)//c_null_char)
      write(*,*) 'Number of Nidek mires read: ',(periodcount-1)/360
      N=(periodcount-1)/360
      if (N .lt. 23 )then
       WRITE (*,*) 'Error on mire count in rcnvrtn:',N
       read_error=-1
       return
      endif
      allocate(ZX(N),YX(N))
      open(unitno1, file=trim(semicolon1), action="read", iostat=ierr)
      open(unitno2, file=trim(semicolon2), action="read", iostat=ierr)
      READ (unitno1,*) header
      READ (unitno2,*) header
      do I=1,MM
       if (file_idx1>0 .and. file_idx2>0) then
        READ(unitno1,*,iostat=readerr) header,ZX(:)
        if (readerr .ne. 0) then
         WRITE (*,*) 'Error on input Nidek RA/XX files on', I,'row'
         read_error=3
         if (allocated(ZX)) deallocate(ZX,YX)
         return
        endif
        READ(unitno2,*,iostat=readerr) header,YX(:)
        if (readerr .ne. 0) then
         WRITE (*,*) 'Error on input Nidek RA/XX files on', I,'row'
         read_error=3
         if (allocated(ZX)) deallocate(ZX,YX)
         return
        endif
        ITH=I-1
       endif
       do J=1,N
        EyeSys%RA(i,j)=100*ZX(j)
        EyeSys%XX(i,j)=100*YX(j)
!       Sanity check on file data
        if (YX(J) > 0 .AND. ZX(J) > 0) then
         if (YX(J) <= ZX(J)) then
          WRITE (*,*) 'Error on input Nidek RA/XX files ArcTan'
          read_error=1
          return
          if (allocated(ZX)) deallocate(ZX,YX)
         endif
        endif
       end do
       if (ITH == (I-1)) then
        EyeSys%DEG(i)=ITH
       else
        WRITE (*,*) 'Error on input Nidek RA/XX files with ITH'
        read_error=2
        if (allocated(ZX)) deallocate(ZX,YX)
        return
       endif
      end do
      CLOSE (unitno1)
      CLOSE (unitno2)
      file_idx1=index(semicolon1, ".TMP")
      write(*,*) 'Erasing semicolonless tmp file',semicolon1,file_idx1
      if (file_idx1 .ne. 0) then
       call execute_command_line ('rm ' // semicolon1, exitstat=io)
       if (io > 0) then
        write (*,*) 'failed system command to remove tmp file',semicolon1
        read_error=12
        if (allocated(ZX)) deallocate(ZX,YX)
        return
       endif
      endif
      file_idx2=index(semicolon2, ".TMP")
      write(*,*) 'Erasing semicolonless tmp file',semicolon2,file_idx1
      if (file_idx2 .ne. 0) then
       call execute_command_line ('rm ' // semicolon2, exitstat=io)
       if (io > 0) then
        write (*,*) 'failed system command to remove tmp file',semicolon2
        read_error=12
        if (allocated(ZX)) deallocate(ZX,YX)
        return
       endif
      endif
      if (Present(HTNAME)) then
       inquire(file=trim(HTNAME), exist=exists)
       if (exists) then
        unitno4 = get_new_fileunit()
        open(unitno4, file=trim(HTNAME), action="read", iostat=ierr)
        if (ierr .eq. 0) then
         READ (unitno4,*) header
         file_idx4=index(trim(header),HTNAME(index(HTNAME,"HT"):len(HTNAME)) // ";")
         if (file_idx4 > 0) then
          write(*,*) 'HT Nidek header detected: ',trim(header)
         else
          write(*,*) 'No HT Nidek headers detected'
          close(unitno4)
          if (allocated(ZX)) deallocate(ZX,YX)
          return
         endif
         semicolon2 = HTNAME
         semicolon2=replacestr(string=HTNAME,search=".DAT",substitute=".TMP")
!         write(*,*) 'sed "s/;/ /g" ' // HTNAME // ' > ' // semicolon2
         call execute_command_line ('sed "s/;/ /g" ' // HTNAME // ' > ' // semicolon2, exitstat=io)
         if (io > 0) then
          write (*,*) 'system command to sed failed'
          write (*,*) 'Consider using your text editor to search/replace all semicolons in data statements in',HTNAME
          write (*,*) 'sed also fails on pathnames with spaces'
          read_error=11
          if (allocated(ZX)) deallocate(ZX,YX)
          return
         endif
         open(unitno4, file=trim(semicolon2), action="read", iostat=ierr)
         READ (unitno4,*) header
         do I=1,MM
          if (file_idx1>0 .and. file_idx2>0) then
           READ(unitno4,*,iostat=readerr) header,ZX(:)
           if (readerr .ne. 0) then
            WRITE (*,*) 'Error on input Nidek HT files on', I,'row'
            read_error=3
            if (allocated(ZX)) deallocate(ZX,YX)
            return
           endif
           ITH=I-1
          endif
          do J=1,N
           EyeSys%HT(i,j)=100*ZX(j)
          end do
         end do
        endif
        CLOSE(unitno4)
        file_idx2=index(semicolon2, ".TMP")
        write(*,*) 'Erasing semicolonless tmp file',semicolon2,file_idx1
        if (file_idx2 .ne. 0) then
         call execute_command_line ('rm ' // semicolon2, exitstat=io)
         if (io > 0) then
          write (*,*) 'failed system command to remove tmp file',semicolon2
          read_error=12
          return
         endif
        endif
       endif
      else
       print*, "Error -- cannot find file: ", trim(HTNAME)
       read_error=7
      endif
      if (Present(PENAME)) then
       inquire(file=trim(PENAME), exist=exists)
       if (exists) then
        unitno3 = get_new_fileunit()
        open(unitno3, file=trim(PENAME), action="read", iostat=ierr)
        if (ierr .eq. 0) then
         READ (unitno3,*) header
         file_idx3=index(trim(header),PENAME(index(PENAME,"PE"):len(PENAME)) // ";")
         if (file_idx3 > 0) then
          write(*,*) 'PE Nidek header detected: ',trim(header)
         else
          write(*,*) 'No PE Nidek header detected'
          return
         endif
         close(unitno3)
         semicolon2 = PENAME
         semicolon2=replacestr(string=PENAME,search=".DAT",substitute=".TMP")
!         write(*,*) 'sed "s/;/ /g" ' // PENAME // ' > ' // semicolon2
         call execute_command_line ('sed "s/;/ /g" ' // PENAME // ' > ' // semicolon2, exitstat=io)
         if (io > 0) then
          write (*,*) 'system command to sed failed'
          write (*,*) 'Consider using your text editor to search/replace all semicolons in data statements in',RANAME
          write (*,*) 'sed also fails on pathnames with spaces'
          read_error=11
          if (allocated(ZX)) deallocate(ZX,YX)
          return
         endif
         open(unitno3, file=trim(semicolon2), action="read", iostat=ierr)
         READ (unitno3,*) header
         READ (unitno3,*) CX,CY ! next line has two numbers
         do I=1,MM
          if (file_idx3 > 0) then
           READ(unitno3,*,iostat=readerr) header,PX
           if (readerr .ne. 0) then
           WRITE (*,*) 'Error on input Nidek PE file on', I,'row'
           read_error=7
           if (allocated(ZX)) deallocate(ZX,YX)
           return
          endif
         endif
         EyeSys%PU(i)=PX
         end do
         EyeSys%Pupil_Center(1)=CX ; EyeSys%Pupil_Center(1)=CY
         CLOSE (unitno3)
         file_idx2=index(semicolon2, ".TMP")
         write(*,*) 'Erasing semicolonless tmp file',semicolon2,file_idx1
         if (file_idx2 .ne. 0) then
          call execute_command_line ('rm ' // semicolon2, exitstat=io)
          if (io > 0) then
           write (*,*) 'failed system command to remove tmp file',semicolon2
           if(allocated(ZX)) deallocate(ZX,YX)
           read_error=12
           return
          endif
         endif
        endif
       else
        print*, "Error ", ierr ," attempting to open file ", trim(PENAME)
        read_error=9
        return
       endif
      endif
      else
        print*, "Error ", ierr ," attempting to open file ", trim(RANAME)
        read_error=3
        return
      endif
     else
      print*, "Error -- cannot find file: ", trim(RANAME)
      read_error=4
      return
     endif
    else
     print*, "Error ", ierr ," attempting to open file ", trim(EDNAME)
     read_error=5
     return
    endif
   else
    print*, "Error -- cannot find file: ", trim(EDNAME)
    read_error=6
    return
   endif
end subroutine rcnvrtn

subroutine rcnvrtn_binary(read_error,mirecount,RANAME,EDNAME,PENAME)
! NIDEK VERSION, binary, experimental, assumes peculiar packed BCD scheme for data
! These all have an ASCII header with the file name including the location in the directory tree
! detects binary
! assumes, but checks 39 mires, as all versions of binary seen have that number
! only reads ED, RA files
! uses EyeSys cornea_array storage files
USE io_functions, ONLY : get_new_fileunit
USE set_precision, ONLY : wp
use util_mod
USE cornea_arrays, ONLY : EyeSys
USE special_fct, ONLY : replacestr
use c_interfaces, ONLY : charcount
USE, INTRINSIC :: iso_c_binding, ONLY : c_int,c_null_char
implicit none
character(len=*), intent(in) :: RANAME,EDNAME
character(len=*), intent(in), optional :: PENAME
integer, intent(out) :: read_error
integer, intent(out) :: mirecount
character(1000) header,header2
character :: ch, ych
character(len=100) :: ioerrmsg
character(:), allocatable :: x, y
integer :: file_idx1,file_idx2,file_idx3,file_idx4,posmax
logical :: exists, negative
INTEGER :: I,J,ITH,unitno1,unitno2,unitno3,MM,N,ierr,pos
REAL (wp) :: ZX(39),YX(39) ! should be maximum needed for mires
integer line(200),line2(200),ix,iy
 x = "" ;   y = ""
 read_error = 0
! check that files exist and have a Nidek style header
 inquire(file=trim(EDNAME), exist=exists)
 if (exists) then
  unitno1 = get_new_fileunit()
  open(unitno1, file=trim(EDNAME), action="read", iostat=ierr)
  if (ierr .eq. 0) then
   inquire(file=trim(RANAME), exist=exists)
   if (exists) then
    unitno2 = get_new_fileunit()
    open(unitno2, file=trim(RANAME), action="read", iostat=ierr)
    if (ierr .eq. 0) then
     READ (unitno1,*) header
     file_idx1=index(trim(header),EDNAME(index(EDNAME,"ED"):len(EDNAME)) // ";")
     if (file_idx1 > 0) then
      write(*,*) 'ASCII ED Nidek header detected: ',trim(header)
     endif
     READ (unitno2,*) header
     file_idx2=index(trim(header),RANAME(index(RANAME,"RA"):len(RANAME)) // ";")
     if (file_idx2 > 0) then
      write(*,*) 'ASCII RA Nidek header detected: ',trim(header)
     else
      write(*,*) 'No ASCII RA/ED Nidek headers detected'
      read_error = 2
     endif
    else
     print*, "Error ", ierr ," attempting to open file ", trim(RANAME)
     read_error=3
     return
    endif
   else
    print*, "Error -- cannot find file: ", trim(RANAME)
    read_error=4
    return
   endif
   else
    print*, "Error ", ierr ," attempting to open file ", trim(EDNAME)
    read_error=5
    return
   endif
   else
    print*, "Error -- cannot find file: ", trim(EDNAME)
    read_error=6
    return
   endif
 close(unitno1)
 close(unitno2)
! Check if binary file despite if ASCII header detected...
 open(unitno1, file=trim(EDNAME), status='old', ACCESS='stream', iostat=ierr)
 do i=1,50
   READ(unitno1,iostat=ierr) ch
!  Detect if Non_ASCII
   if (ichar(ch) < 0 .or. ichar(ch) > 127 .and. read_error == 0) then
    write(*,*) 'Non_ASCII characters detected in', trim(EDNAME)
    read_error = 2
   endif
 end do
 close(unitno1)
 ! This will also read ASCII headers, but I only want to run it if the file is a binary Nidek
 if (read_error == 2) then
  read_error = 1
  open(unitno1, file=trim(EDNAME), status='old', ACCESS='stream', iostat=ierr)
! READ the header
  ch = ' ' ;   x = join(c(ch,x))
  DO WHILE (ierr == 0)
   READ(unitno1,iostat=ierr) ch
   if (ierr == 0 ) then
    if (iachar(ch) .ne. 10) then  !  0D 0A ends each line
     x = join(c(x,ch))
    else
!   found the 0D
     write(*,*) 'Nidek header ', x
     exit
    endif
   else
!  EOF or other read error
    exit
   endif
  END DO
  x = ""
! READ the data
   POS=0 ; i=0 ; j=0 ; mirecount = 0 ; posmax = 0
   DO
    READ(unitno1,iostat=ierr) ch
    POS=POS+1
    if (ierr == 0 ) then
     write(header,'(z0)') ch
     x = join(c(x,trim(header)))
     line(pos-i)=iachar(ch)
     if (pos-i-1 .ge. 1) then
      if (line(pos-i) .eq. 10 .and. line(pos-i-1) .eq. 13) then  !  0D 0A ends each line
  ! here's where to read the C's and E's and divide into 24 bit pieces
       j = j+1
       i = 1
      ix = 1
      do while (i .lt. len(trim(x)))
  !  Assumes "2C2222" is the baseline for zero for all PackedBCD codes, converts ASCII to hex subtract and leave as decimal digit
       if (iachar(x(i:i)) .lt. 58) zx(ix)= iachar(x(i:i))-50
       if (iachar(x(i:i)) .ge. 65) zx(ix)= iachar(x(i:i))-57
       if (iachar(x(i+2:i+2)) .lt. 58) zx(ix)= zx(ix)+(iachar(x(i+2:i+2))-50)*0.1
       if (iachar(x(i+2:i+2)) .ge. 65) zx(ix)= zx(ix)+(iachar(x(i+2:i+2))-57)*0.1
       if (iachar(x(i+3:i+3)) .lt. 58) zx(ix)= zx(ix)+(iachar(x(i+3:i+3))-50)*0.01
       if (iachar(x(i+3:i+3)) .ge. 65) zx(ix)= zx(ix)+(iachar(x(i+3:i+3))-57)*0.01
       if (iachar(x(i+4:i+4)) .lt. 58) zx(ix)= zx(ix)+(iachar(x(i+4:i+4))-50)*0.001
       if (iachar(x(i+4:i+4)) .ge. 65) zx(ix)= zx(ix)+(iachar(x(i+4:i+4))-57)*0.001
       if (iachar(x(i+5:i+5)) .lt. 58) zx(ix)= zx(ix)+(iachar(x(i+5:i+5))-50)*0.0001
       if (iachar(x(i+5:i+5)) .ge. 65) zx(ix)= zx(ix)+(iachar(x(i+5:i+5))-57)*0.0001
       i=i+7
       ix=ix+1
       posmax=max(posmax,pos-i)
  !  Assumes "D" is the only other code added, could also consider "F"
       if ( x(i:i) == "D") i=i+1
      end do
      EyeSys%RA(j,:)=100*ZX(:)
      mirecount = max(ix,mirecount)
      x = ""
      i=1 ; pos=1
     endif
     endif
    else
  !  EOF or other read error
     write(*,*) j,'radials'
     mirecount = mirecount -1
     write(*,*) mirecount, ' mires counted'
     write(*,*) posmax, ' maximum data places per line'
     if (j .ne. 360) then
      read_error = -1000
      write(*,*) 'FATAL Error reading NIDEK binary', EDNAME
      close(unitno1)
      exit
     endif
     exit
    endif
   end do
  close(unitno1)
 endif
 ! Check that I read Non-ASCII above
 if (read_error == 1) then
  open(unitno2, file=trim(RANAME), status='old', ACCESS='stream', iostat=ierr)
! READ the header
  ych = ' ' ;  y = join(c(ych,y))
  DO WHILE (ierr == 0)
   READ(unitno2,iostat=ierr) ych
   if (ierr == 0 ) then
    if (iachar(ych) .ne. 10) then  !  0D 0A ends each line
     y = join(c(y,ych))
    else
!   found the 0D
     write(*,*) 'Binary header ', y
     exit
    endif
   else
!  EOF or other read error
    exit
   endif
  END DO
  y = ""
! READ the data
   POS=0 ; ith=0 ; j=0 ; posmax = 0
   DO
    READ(unitno2,iostat=ierr) ych
    POS=POS+1
    if (ierr == 0 ) then
     write(header2,'(z0)') ych
     y = join(c(y,trim(header2)))
     line2(pos-ith)=iachar(ych)
     if (pos-ith-1 .ge. 1) then
      if (line2(pos-ith) .eq. 10 .and. line2(pos-ith-1) .eq. 13) then  !  0D 0A ends each line
  ! here's where to read the C's and E's and divide into 24 bit pieces
      j = j+1
      ith = 1
      iy = 1
      do while (ith .lt. len(trim(y)))
 !   Assumes "2C2222" is the baseline for zero for all PackedBCD codes, converts ASCII to hex subtract and leave as decimal digit
       if (iachar(y(ith:ith)) .lt. 58) yx(iy)= iachar(y(ith:ith))-50
       if (iachar(y(ith:ith)) .ge. 65) yx(iy)= iachar(y(ith:ith))-57
       if (iachar(y(ith+2:ith+2)) .lt. 58) yx(iy)= yx(iy)+(iachar(y(ith+2:ith+2))-50)*0.1
       if (iachar(y(ith+2:ith+2)) .ge. 65) yx(iy)= yx(iy)+(iachar(y(ith+2:ith+2))-57)*0.1
       if (iachar(y(ith+3:ith+3)) .lt. 58) yx(iy)= yx(iy)+(iachar(y(ith+3:ith+3))-50)*0.01
       if (iachar(y(ith+3:ith+3)) .ge. 65) yx(iy)= yx(iy)+(iachar(y(ith+3:ith+3))-57)*0.01
       if (iachar(y(ith+4:ith+4)) .lt. 58) yx(iy)= yx(iy)+(iachar(y(ith+4:ith+4))-50)*0.001
       if (iachar(y(ith+4:ith+4)) .ge. 65) yx(iy)= yx(iy)+(iachar(y(ith+4:ith+4))-57)*0.001
       if (iachar(y(ith+5:ith+5)) .lt. 58) yx(iy)= yx(iy)+(iachar(y(ith+5:ith+5))-50)*0.0001
       if (iachar(y(ith+5:ith+5)) .ge. 65) yx(iy)= yx(iy)+(iachar(y(ith+5:ith+5))-57)*0.0001
       ith=ith+7
       iy=iy+1
       posmax=max(posmax,pos-i)
 !  Assumes "D" is the only other code added, could also consider "F"
       if ( y(ith:ith) == "D") ith=ith+1
      end do
      EyeSys%XX(j,:)=100*YX(:)
      mirecount = max(iy,mirecount)
      y = ""
      ith=1 ; pos=1
     endif
     endif
    else
  !  EOF or other read error
     write(*,*) j,'radials'
     MM = j
     mirecount = mirecount -1
     write(*,*) mirecount, ' mires counted'
     N = mirecount
     write(*,*) posmax, ' maximum data places per line'
     exit
    endif
   end do
  close(unitno2)
 endif
 ! Pupil data from PE
 ! Check that I read Non-ASCII above
 if (read_error == 1) then
  inquire(file=trim(PENAME), exist=exists)
  if (exists) then
   unitno3 = get_new_fileunit()
   open(unitno3, file=trim(PENAME), status='old', ACCESS='stream', iostat=ierr)
 ! READ the header
   ych = ' ' ;  y = join(c(ych,y))

   DO  WHILE (ierr == 0)
    READ(unitno3,iostat=ierr,iomsg=ioerrmsg) ych
    if (ierr == 0 ) then
     if (iachar(ych) .ne. 10) then  !  0D 0A ends each line
      y = join(c(y,ych))
     else
 !   found the 0D
      write(*,*) 'Binary header ', y
      exit
     endif
    else
 !  EOF or other read error
     write (*,*) ioerrmsg
     exit
    endif
   END DO
   y = ""
 ! READ the data
    POS=1 ; ith=0 ; j=0 ; posmax = 0
    DO
     READ(unitno3,iostat=ierr) ych
     POS=POS+1
     if (ierr == 0 ) then
      write(header2,'(z0)') ych
      y = join(c(y,trim(header2)))
      line2(pos-ith)=iachar(ych)
      if (pos-ith-1 .ge. 1) then
       if (line2(pos-ith) .eq. 10 .and. line2(pos-ith-1) .eq. 13) then  !  0D 0A ends each line
   ! here's the 24 bit packed BCD section, slightly different for PENAME
        j = j+1
        iy = 1
!       One positive float
        if (len(trim(y)) .eq. 8) then
           ith = 1 ; iy = j-1
!   Assumes "2C2222" is the baseline for zero for all PackedBCD codes, converts ASCII to hex subtract and leave as decimal digit
           if (iachar(y(ith:ith)) .lt. 58) EyeSys%PU(iy)= iachar(y(ith:ith))-50
           if (iachar(y(ith:ith)) .ge. 65) EyeSys%PU(iy)= iachar(y(ith:ith))-57
           if (iachar(y(ith+2:ith+2)) .lt. 58) EyeSys%PU(iy)= EyeSys%PU(iy)+(iachar(y(ith+2:ith+2))-50)*0.1
           if (iachar(y(ith+2:ith+2)) .ge. 65) EyeSys%PU(iy)= EyeSys%PU(iy)+(iachar(y(ith+2:ith+2))-57)*0.1
           if (iachar(y(ith+3:ith+3)) .lt. 58) EyeSys%PU(iy)= EyeSys%PU(iy)+(iachar(y(ith+3:ith+3))-50)*0.01
           if (iachar(y(ith+3:ith+3)) .ge. 65) EyeSys%PU(iy)= EyeSys%PU(iy)+(iachar(y(ith+3:ith+3))-57)*0.01
           if (iachar(y(ith+4:ith+4)) .lt. 58) EyeSys%PU(iy)= EyeSys%PU(iy)+(iachar(y(ith+4:ith+4))-50)*0.001
           if (iachar(y(ith+4:ith+4)) .ge. 65) EyeSys%PU(iy)= EyeSys%PU(iy)+(iachar(y(ith+4:ith+4))-57)*0.001
           if (iachar(y(ith+5:ith+5)) .lt. 58) EyeSys%PU(iy)= EyeSys%PU(iy)+(iachar(y(ith+5:ith+5))-50)*0.0001
           if (iachar(y(ith+5:ith+5)) .ge. 65) EyeSys%PU(iy)= EyeSys%PU(iy)+(iachar(y(ith+5:ith+5))-57)*0.0001
           EyeSys%PU(iy)=1.0*EyeSys%PU(iy)
        else
!        Two signed floats
          if (len(trim(y)) .ne. 16) write(*,*) 'Error in reading: ', PENAME
!        first float
         if ( y(1:1) == "D" .or. y(1:1) == "B") then   ! sign prefix
          negative = .true.
          ith = 2
         else
          negative = .false.
          ith = 1
          endif
! put first two floats in EyeSys%Pupil_Center
          do iy=1,2
!   Assumes "2C2222" is the baseline for zero for all PackedBCD codes, converts ASCII to hex subtract and leave as decimal digit
           if (iachar(y(ith:ith)) .lt. 58) EyeSys%Pupil_Center(iy)= iachar(y(ith:ith))-50
           if (iachar(y(ith:ith)) .ge. 65) EyeSys%Pupil_Center(iy)= iachar(y(ith:ith))-57
           if (iachar(y(ith+2:ith+2)) .lt. 58) EyeSys%Pupil_Center(iy)= EyeSys%Pupil_Center(iy)+(iachar(y(ith+2:ith+2))-50)*0.1
           if (iachar(y(ith+2:ith+2)) .ge. 65) EyeSys%Pupil_Center(iy)= EyeSys%Pupil_Center(iy)+(iachar(y(ith+2:ith+2))-57)*0.1
           if (iachar(y(ith+3:ith+3)) .lt. 58) EyeSys%Pupil_Center(iy)= EyeSys%Pupil_Center(iy)+(iachar(y(ith+3:ith+3))-50)*0.01
           if (iachar(y(ith+3:ith+3)) .ge. 65) EyeSys%Pupil_Center(iy)= EyeSys%Pupil_Center(iy)+(iachar(y(ith+3:ith+3))-57)*0.01
           if (iachar(y(ith+4:ith+4)) .lt. 58) EyeSys%Pupil_Center(iy)= EyeSys%Pupil_Center(iy)+(iachar(y(ith+4:ith+4))-50)*0.001
           if (iachar(y(ith+4:ith+4)) .ge. 65) EyeSys%Pupil_Center(iy)= EyeSys%Pupil_Center(iy)+(iachar(y(ith+4:ith+4))-57)*0.001
           if (iachar(y(ith+5:ith+5)) .lt. 58) EyeSys%Pupil_Center(iy)= EyeSys%Pupil_Center(iy)+(iachar(y(ith+5:ith+5))-50)*0.0001
           if (iachar(y(ith+5:ith+5)) .ge. 65) EyeSys%Pupil_Center(iy)= EyeSys%Pupil_Center(iy)+(iachar(y(ith+5:ith+5))-57)*0.0001
           if ( negative ) then
            EyeSys%Pupil_Center(iy) = -1.0 * EyeSys%Pupil_Center(iy)
           else
            EyeSys%Pupil_Center(iy) =  1.0 * EyeSys%Pupil_Center(iy)
           endif
           ith = ith + 7
           if ( y(ith-1:ith-1) == "D" .or. y(ith-1:ith-1) == "B") then
            negative = .true.
           else
            negative = .false.
           endif
          end do
         endif
         posmax=max(posmax,pos-i)
       y = ""
       ith=1 ; pos=1
      endif
      endif
     else
   !  EOF or other read error
      write(*,*) posmax, ' maximum data places per line'
      exit
     endif
    end do
   close(unitno3)
  endif
 endif
 do i=1,MM
  do J=1,N
   ZX(j)=EyeSys%RA(i,j)/100
   YX(j)=EyeSys%XX(i,j)/100
!  Sanity check on file data
   if (YX(J) > 0 .AND. ZX(J) > 0) then
    if (YX(J) <= ZX(J)) then
     WRITE (*,*) 'Error on input Nidek RA/XX files ArcTan'
     read_error=-1
     return
    endif
   endif
  end do
  EyeSys%DEG(I)=I-1
 end do
end subroutine rcnvrtn_binary


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
           exit
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
