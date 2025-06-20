!https://fortranwiki.org/fortran/show/Stream+Input+Output
!https://fortran-lang.discourse.group/t/joining-strings-problem-with-gfortran/492
!https://stackoverflow.com/questions/62838105/reading-hexadecimal-data-from-file-in-fortran

module tiff_reader
implicit none
private
public :: tiff_reader_16bit
contains
subroutine tiff_reader_16bit(filename, tifdata, ndata)
character(len=*), intent(in) :: filename
integer, allocatable, intent(out) :: tifdata(:)
integer, intent(out) :: ndata
integer, parameter :: max_integers=10000000
integer :: unt, status, record_length, i, records, lsb, msb
character ch;
integer, dimension(max_integers) :: temp
ndata=0
inquire(iolength=record_length) ch
open(newunit=unt, file=filename, access='direct', form='unformatted',&
     action='read', status='old', iostat=status, recl=record_length)
if (status /= 0) then
  print "(3a)","Error reading file """,filename,""": File not found."; return
end if
records=1
do i=1,max_integers
  read(unit=unt, rec=records, iostat=status) ch; msb=ichar(ch)
  if (status /= 0) then; records=records-1; ndata=i-1; exit; end if
  read(unit=unt, rec=records+1, iostat=status) ch; lsb=ichar(ch)
  if (status /= 0) then; ndata=i; temp(ndata)=msb; exit; end if
  temp(i)=lsb+256*msb; records=records+2
end do
 close(unit=unt)
if (ndata==0) then
  print "(a)","File partially read."; records=records-1; ndata=max_integers
end if
allocate(tifdata(ndata), stat=status); tifdata=temp(:ndata)
print "(2(i0,a),/)",records," records read, ",ndata," 16-bit integers returned."
end subroutine tiff_reader_16bit
end module tiff_reader

program tiff_reader_example
use tiff_reader
implicit none
integer :: n
integer, allocatable :: tifdata(:)
call tiff_reader_16bit("RAOPD.DAT", tifdata, n);
if (n > 0) then
  print "(a,7(z4.4,tr1),z4.4,a)", "First 8 integers read: (", tifdata(:8), ")"
  print "(a,7(z4.4,tr1),z4.4,a)", " Last 8 integers read: (", tifdata(n-7:), ")"
  deallocate(tifdata)
end if
end program tiff_reader_example






