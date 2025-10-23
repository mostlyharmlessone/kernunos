!https://fortranwiki.org/fortran/show/Stream+Input+Output
!https://fortran-lang.discourse.group/t/joining-strings-problem-with-gfortran/492
!https://stackoverflow.com/questions/62838105/reading-hexadecimal-data-from-file-in-fortran

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


 subroutine rcnvrtn_binary(read_error,RANAME,EDNAME,HTNAME,PENAME)
! NIDEK VERSION, binary, experimental
! These all have an ASCII header with the file name including the location in the directory tree
! RAOPD shows 360 data segments that are 138 bytes long each ?17 numbers(values) x 8 bytes = 136 + 2 (0D 0A)
! EDOPD shows 360 data segments that are 138-141 bytes each  ?17 numbers x 8 bytes + +( 0-3 bytes ) + 2 (0D 0A)  
! AROPD shows 361 data segments the first is a short segment 13 bytes, then 360 x 39-41 bytes each  = (11)37 + (0-2 bytes) + 2 bytes (0D 0A)  
! PROPD shows 361 data segments the first is a long segment 9 bytes, then 360 x 5 bytes each  = (7)3 + 2 bytes (0D 0A)
! PEOPD shows 361 data segments the first is a long segment 9 bytes, then 360 x 5 bytes each  = (7)3 + 2 bytes (0D 0A)
! For DOS/Windows systems, the newline is actually two characters NewLine ( ) is 10 (0xA) and CarriageReturn ( ) is 13 (0xD)
! only have seen version with headers which are the filename with path ending with a semicolon

! assumes maximum mires 39; need to verify with record count 138 to 141

! only reads ED, RA, HT and PE files
! uses EyeSys cornea_array storage files
  use io_functions, ONLY : get_new_fileunit
  use set_precision, ONLY : wp
  use cornea_arrays, ONLY : EyeSys
  use special_fct, ONLY : replacestr
  use c_interfaces, ONLY : charcount
  use, INTRINSIC :: iso_c_binding, ONLY : c_bool, c_int
  use iso_fortran_env !, only: int16, int32, int64
  use ieee_arithmetic
  use util_mod
  implicit none
  logical :: exists
  character(len=*), intent(in) :: RANAME,EDNAME
  character(len=*), intent(in), optional :: PENAME,HTNAME
  character(1000) header,header_space,semicolon1,semicolon2
  integer :: file_idx1,file_idx2,file_idx3,file_idx4,readerr,io
  integer, intent(out) :: read_error
  REAL(wp), ALLOCATABLE :: ZX(:),YX(:)
  REAL(wp) :: PX,CX,CY
  INTEGER :: I,J,ITH,unitno1,unitno2,unitno3,unitno4,MM,N,ierr
  integer(c_int) :: periodcount


  character(:), allocatable :: x
  character :: ch,ch1,ch2,ch3,ch4,ch5,ch6,ch7,ch8
  integer :: pos,unit1,readerr,i,j,record_length,ival,unit2
  character(1000) header,header_space
  integer :: file_idx1,file_idx2,file_idx3,file_idx4
!  integer, intent(out) :: read_error
  integer :: degree, e_idx
  REAL :: ZX(39),YX(39),PX,CX,CY
  integer(INT8) :: IZX(4)
  integer(kind=2) :: IX,JX
  integer(kind=1) ::  i1,i2,i3
  integer line(145)
  integer(c_int) :: ic



 print*,trim(compiler_version())
 inquire(iolength=record_length) ch
! OPEN(NEWUNIT=unit1, file="RAOPD.DAT", status='old', ACCESS='stream')
! OPEN(NEWUNIT=unit1, file="EDOPD.DAT", status='old', ACCESS='stream')
 OPEN(NEWUNIT=unit1, file="edtest11L.DAT", status='old', ACCESS='stream')
! OPEN(NEWUNIT=unit1, file="AROPD.DAT", status='old', ACCESS='stream') 
! OPEN(NEWUNIT=unit1, file="PROPD.DAT", status='old', ACCESS='stream')
! OPEN(NEWUNIT=unit1, file="PEOPD.DAT", status='old', ACCESS='stream')
 POS=0 ; ch = ' ' ;   x = join(c(ch,x)) ; i=0 ; j= 0
! READ the header
 DO
  READ(unit1,iostat=readerr) ch
  POS=POS+1
  if (readerr == 0 ) then  
   if (iachar(ch) .ne. 10) then  !  0D 0A ends each line
    x = join(c(x,ch))
!    WRITE(*,*) 'pos, ch,iachar(ch) ',pos, ch,iachar(ch)
   else 
!   found the 0D
!    WRITE(*,*) 'pos, ch,iachar(ch) ',pos, ch,iachar(ch)  
    write(*,*) 'pos, header', pos, x
    exit
   endif 
  else
!  EOF or other read error
   exit
  endif
 END DO
 x = ""
! READ the data
 POS=0 
 DO

!if (POS < 9) then
!  READ(unit1,iostat=readerr) ch,ch1,ch2,ch3,ch4,ch5,ch6,ch7,ch8
!   write(*,'(z0,a,z0,a,z0,a,z0,a,z0,a,z0,a,z0,a,z0,a,z0)') ch,'    ',ch1,'   ',ch2,'    ',ch3,'   ',ch4,'    ',ch5,'   ',ch6,'    ',ch7,'   ',ch8

!  READ(unit1,iostat=readerr) ch,ch1,ch2,ch3,ch4,ch5,ch6,ch7,ch8
!   write(*,'(z0,a,z0,a,z0,a,z0,a,z0,a,z0,a,z0,a,z0,a,z0)') ch,'    ',ch1,'   ',ch2,'    ',ch3,'   ',ch4,'    ',ch5,'   ',ch6,'    ',ch7,'   ',ch8

!  POS=POS+9
 READ(unit1,iostat=readerr) ch
!  POS=POS+1
!else

!  READ(unit1,iostat=readerr) ch,ch1,ch2,ch3,ch4
!   write(*,'(z0,a,z0,a,z0,a,z0,a,z0)') ch,'    ',ch1,'   ',ch2,'    ',ch3,'   ',ch4
!  second 4 bits are always C, ie 4C,5C,3C ? some kind of float 
!   write(*,'(i0,a,i0,a,i0,a,z0,a,z0)') iachar(ch),'    ',iachar(ch1),'   ',iachar(ch2),'    ',ch3,'   ',ch4
!   write(*,*) pos,iachar(ch2)+100*iachar(ch1)  !+100*(iachar(ch)-12)

!  READ(unit1,iostat=readerr) degree,ch2     !degree consumes 4 bytes
!   write(*,'(i0,a,z0)') degree,'   ',ch2

!  READ(unit1,iostat=readerr) ix,jx,ch2     !ix,jx each consume 2 bytes
!   write(*,'(i0,a,i0,a,z0)') ix,'  ',jx,'   ',ch2

!  READ(unit1,iostat=readerr) ix,ch,ch1,ch2    !ix consumes 2 bytes
!   write(*,'(i0,a,z0,a,z0,a,z0)') ix,'  ',ch,'   ',ch1,'  ',ch2

!  READ(unit1,iostat=readerr) ch,ix,ch1,ch2    !ix consumes 2 bytes
!   write(*,'(z0,a,i0,a,z0,a,z0)') ch,'  ',ix,'   ',ch1,'  ',ch2

!  READ(unit1,iostat=readerr) i1,i2,i3,ch1,ch2    !i1,i2,i3 consumes 1 byte
!   write(*,'(i0,a,i0,a,i0,a,z0,a,z0)') i1,'  ',i2,'  ',i3,'   ',ch1,'  ',ch2

!  READ(unit1,iostat=readerr) ix,i1,ch1,ch2    !i1 consumes 1 byte, ix 2 bytes
!   write(*,'(i0,a,i0,a,z0,a,z0)') ix,'  ',i1,'   ',ch1,'  ',ch2

!  READ(unit1,iostat=readerr) i1,ch,i2,ch1,ch2    !i1 consumes 1 byte, ix 2 bytes
!   write(*,'(i0,a,z0,a,i0,a,z0,a,z0)') i1,'  ',ch,'   ',i2,'  ',ch1,'  ',ch2

!  POS=POS+5
!   pause
! endif

  
  POS=POS+1 
  if (readerr == 0 ) then
!  acculmulate ch, save the read
!   write(*,'(z0,a,i0,a,i0)') ch,'    ',pos-i,'    ',iachar(ch) 
   write(header,'(z0)') ch 
   x = join(c(x,trim(header)))
   line(pos-i)=iachar(ch)
   if (line(pos-i) .eq. 10 .and. line(pos-i-1) .eq. 13) then  !  0D 0A ends each line
!  found the 0D 0A  

!    write(*,*) 'pos,', pos,pos-i  

!  clear accumulated ch buffer
!   write(*,*) line(1:pos-i)
!   line(1:pos-i)=0 
!   write(*,*) pos-i, x

! here's where to read the C's and E's and divide into 24 bit pieces
   i = 1
   ix = 1
   do while (i .lt. len(trim(x)))
    write(*,*) x(i:i+5)
!  Assumes "2C2222" is the baseline for zero for all Multibyte codes, converts ASCII to hex subtract and leave as decimal digit
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
!  Assumes "D" is the only other code added, could also consider "F"
    if ( x(i:i) == "D") i=i+1   
   end do

   write(*,*) zx(:)
   write(*,*) x(:)

!  need something like ths
!        EyeSys%RA(i,j)=100*ZX(j)
!        EyeSys%XX(i,j)=100*YX(j)
! not this j, this is total   write(*,*) j
   x = ""
 
!   exit   !exit after the first record

    i=pos
    j=j+1
   endif 
    j=j+1
  else
!  EOF or other read error
   write(*,*) j,'records'  


   exit
  endif
 END DO

 close(unit=unit1)


 end subroutine rcnvrtn_binary












