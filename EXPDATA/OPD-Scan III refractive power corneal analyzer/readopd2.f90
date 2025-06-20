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
  character (len=100)            , allocatable :: vec(:)
  character (len=100)            , allocatable :: vec_(:)
  integer                                      :: n
  allocate (vec_(2))
  if (present(x1))  vec_(1)  = x1
  if (present(x2))  vec_(2)  = x2
  n = count([present(x1),present(x2)])
  if (n > 0) vec = vec_(:n)
  end function c
 end module util_mod



 program main
 use iso_fortran_env !, only: int16, int32, int64
 use iso_c_binding, only: c_bool, c_int
 use ieee_arithmetic
 use util_mod
 use set_precision

  implicit none
  character(:), allocatable :: x
  character :: ch,ch1,ch2,ch3,ch4,ch5,ch6,ch7,ch8
  integer :: pos,unit1,readerr,i,j,record_length,ival,unit2
  character(1000) header,header_space
  integer :: file_idx1,file_idx2,file_idx3,file_idx4
!  integer, intent(out) :: read_error
  integer :: degree
  REAL :: ZX(16),YX(16),PX,CX,CY
  integer(INT8) :: IZX(4)
  integer(kind=2) :: IX,JX
  integer(kind=1) ::  i1,i2,i3
  integer line(145)
  integer(c_int) :: ic

 print*,trim(compiler_version())
 inquire(iolength=record_length) ch
! OPEN(NEWUNIT=unit1, file="RAOPD.DAT", status='old', ACCESS='stream')
! OPEN(NEWUNIT=unit1, file="EDOPD.DAT", status='old', ACCESS='stream')
! OPEN(NEWUNIT=unit1, file="AROPD.DAT", status='old', ACCESS='stream') 
 OPEN(NEWUNIT=unit1, file="PROPD.DAT", status='old', ACCESS='stream')
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
! READ the data
 POS=0 
 DO

if (POS < 9) then
  READ(unit1,iostat=readerr) ch,ch1,ch2,ch3,ch4,ch5,ch6,ch7,ch8
   write(*,'(z0,a,z0,a,z0,a,z0,a,z0,a,z0,a,z0,a,z0,a,z0)') ch,'    ',ch1,'   ',ch2,'    ',ch3,'   ',ch4,'    ',ch5,'   ',ch6,'    ',ch7,'   ',ch8

!  READ(unit1,iostat=readerr) ch,ch1,ch2,ch3,ch4,ch5,ch6,ch7,ch8
!   write(*,'(z0,a,z0,a,z0,a,z0,a,z0,a,z0,a,z0,a,z0,a,z0)') ch,'    ',ch1,'   ',ch2,'    ',ch3,'   ',ch4,'    ',ch5,'   ',ch6,'    ',ch7,'   ',ch8

  POS=POS+9
! READ(unit1,iostat=readerr) ch
!  POS=POS+1
else

  READ(unit1,iostat=readerr) ch,ch1,ch2,ch3,ch4
   write(*,'(z0,a,z0,a,z0,a,z0,a,z0)') ch,'    ',ch1,'   ',ch2,'    ',ch3,'   ',ch4
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

  POS=POS+5
!   pause
 endif

  
!  POS=POS+1 
  if (readerr == 0 ) then
!  acculmulate ch, save the read
!   write(*,'(z0,a,i0,a,i0)') ch,'    ',pos-i,'    ',iachar(ch) 
!   line(pos-i)=iachar(ch)
!   if (line(pos-i) .eq. 10 .and. line(pos-i-1) .eq. 13) then  !  0D 0A ends each line
!  found the 0D 0A  

!    write(*,*) 'pos,', pos,pos-i  

!  clear accumulated ch buffer
!   write(*,*) line(1:pos-i)
!   line(1:pos-i)=0 

!   exit   !exit after the first record

    i=pos
    j=j+1
!   endif 
 !   j=j+1
  else
!  EOF or other read error
   write(*,*) j,'records'  
!  These all have an ASCII header with the file name including the location in the directory tree
!  RAOPD shows 360 data segments that are 138 bytes long each ?17 numbers(values) x 8 bytes = 136 + 2 (0D 0A)
!  EDOPD shows 360 data segments that are 138-141 bytes each  ?17 numbers x 8 bytes + +( 0-3 bytes ) + 2 (0D 0A)  
!  AROPD shows 361 data segments the first is a short segment 13 bytes, then 360 x 39-41 bytes each  = (11)37 + (0-2 bytes) + 2 bytes (0D 0A)  
!  PROPD shows 361 data segments the first is a long segment 9 bytes, then 360 x 5 bytes each  = (7)3 + 2 bytes (0D 0A)
!  PEOPD shows 361 data segments the first is a long segment 9 bytes, then 360 x 5 bytes each  = (7)3 + 2 bytes (0D 0A)

! For DOS/Windows systems, the newline is actually two characters NewLine ( ) is 10 (0xA) and CarriageReturn ( ) is 13 (0xD)
!?RA==RA
!?ED==XX
!?PR==PU 

   exit
  endif
 END DO

 close(unit=unit1)

!IEEE 754
!    The most significant bit (MSB) is used to store the sign of the number.
!    The next 8 bits are used to store the exponent.
!    The remaining 23 bits are used to store the mantissa.
!Converting 65.125 to Binary form, we get:
!65     = 1000001
!0.125  = 001
!So, 
!65.125 = 1000001.001
!       = 1.000001001 x 106
!Normalized Mantissa = 000001001

!Now, according to the standard,
!we will get the biased exponent by adding the exponent to 127,
!       = 127 + 6 = 133
!Biased exponent = 10000101

!And the signed bit is 0 (positive)

!So, the IEEE 754 representation of 65.125 is,
!0 10000101 00000100100000000000000
! 0100 0010 1000 0010 0100 0000 0000 0000
! 42 82 40 00 


!char=>1 byte (ASCII uses 7 bits, the 8th is zero)
!int4=int=4 bytes
!real=4 bytes
!int2=2 bytes
! two hex-digits represent an 8-bit pattern, i.e. a byte.
!The one-byte logical data type, LOGICAL*1, which has the synonym, BYTE, can hold any of the following:
!    One character
!    An integer between -128 and 127
!    The logical values .TRUE. or .FALSE.
!The value is as defined for LOGICAL, but it can hold a character or small integer. An example:
!	LOGICAL*1 		Bit3 / 8 /, C1 / 'W' /, 
!& 			Counter / 0 /, Switch / .FALSE. / 
!A LOGICAL*1 item occupies one byte of storage
!LOGICAL*1 is aligned on one-byte boundaries.

!test writes
! fp  00 00 00 40 -> 2.  00 00 4c 42 -> 51
!RAOPD -> AC 24 63 EA C2 56 4E
!PEOPD 1st normal data segment-> 4C 22 96 (0D 0A)

 OPEN(NEWUNIT=unit2, file="TEST.DAT", status='old', ACCESS='stream')

     PX=40.0
      J=40
      ix=i
     cx=30
!      write(unit2,iostat=readerr)

! ?formatted stream ?possibly written from C/C++ but hopefully IEEE standard
!  : = 3A =58 isn't part of the file structure above
!   
!	write(unit2,iostat=readerr) "r000078|R|0730074445|7.77 Test angle|||||angle||15d|\n"
!        write(unit2,iostat=readerr) "002: 51 75 100 130 153 182 207 237 262 292 318 347 375 0 405 436"
	write(unit2,iostat=readerr) 2, 0, 51, 75, 100, 130, 153, 182, 207, 237, 262, 292, 318, 347, 375, 0, 405, 436
!	write(unit2,iostat=readerr) 1, 51, 75, 100, 130, 153, 182, 207, 237, 262, 292, 318, 347, 375, 0, 405, 436
	write(unit2,iostat=readerr) 2.,0., 51., 75., 100., 130., 153., 182., 207., 237., 262., 292., 318., 347., 375., 0., 405., 436.
	write(unit2,iostat=readerr) "r000078|R|0730074445|7.77 Test angle|||||angle||15d|"
!	write(unit2,iostat=readerr) 2._dp,0._dp, 51.235_dp, 75.176_dp, 100.28_dp, 130.516_dp, 153.978_dp, 182.31_dp, 207.11_dp, 237.87_dp, 262.92_dp, 292.112_dp, 318.3_dp, 347.76_dp, 375.44_dp, 0.22_dp, 405.21_dp, 436.13_dp







 close(unit2)


 
 
end program main












