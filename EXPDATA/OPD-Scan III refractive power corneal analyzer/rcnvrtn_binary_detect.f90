
  subroutine rcnvrtn_binary_detect(read_error,RANAME,EDNAME,HTNAME,PENAME)
! NIDEK VERSION, binary, experimental
! These all have an ASCII header with the file name including the location in the directory tree
! detects binary
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
  character(1000) header,header_space
  character :: ch
  integer :: file_idx1,file_idx2,file_idx3,file_idx4,io
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
       close(unitno1)
       close(unitno2)
       return
      endif
      close(unitno2)
     endif
    endif
    close(unitno1)
   endif

   read_error = 0 
   i = 0
   inquire(iolength=record_length) ch
   unitno1 = get_new_fileunit()
   open(unitno1, file=trim(EDNAME), status='old', ACCESS='stream', iostat=ierr)
   DO while (ierr == 0 .and. i < 200)
    i=i+1
    READ(unitno1,iostat=readerr) ch
    if (ichar(ch) < 0 .or. ichar(ch) > 127) then
     write(*,*) 'Non_ASCII characters detected'
     read_error = 1     
    endif
   end do
   close(unitno1)

  end subroutine rcnvrtn_binar_detect