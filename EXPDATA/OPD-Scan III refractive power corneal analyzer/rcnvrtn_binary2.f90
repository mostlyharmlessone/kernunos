
subroutine rcnvrtn(read_error,RANAME,EDNAME,HTNAME,PENAME)
! NIDEK VERSION
! only have seen uncompressed version with headers which are the filename with path ending with a semicolon
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
  character(1000) header,header_space,semicolon1,semicolon2
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
      write(*,*) 'Remove the semicolons with sed because Fortran hates them'
      semicolon1=replacestr(string=EDNAME,search=".DAT",substitute=".TMP")
    !  write(*,*) 'sed "s/;/ /g" ' // EDNAME // ' > ' // semicolon1
      call system('sed "s/;/ /g" ' // EDNAME // ' > ' // semicolon1, io)
      if (io > 0) then
       write (*,*) 'system command to sed failed'
       write (*,*) 'Consider using your text editor to search/replace all semicolons in data statements in',EDNAME
       read_error=11
       return
      endif
      semicolon2 = trim(RANAME)
      semicolon2=replacestr(string=RANAME,search=".DAT",substitute=".TMP")
    !  write(*,*) 'sed "s/;/ /g" ' // RANAME // ' > ' // semicolon2
      call system('sed "s/;/ /g" ' // RANAME // ' > ' // semicolon2, io)
      if (io > 0) then
       write (*,*) 'system command to sed failed'
       write (*,*) 'Consider using your text editor to search/replace all semicolons in data statements in',RANAME
       read_error=11
       return
      endif
!     Calculate number of mires by counting the floating point periods in the file, subtracting the header file extension, and dividing by 360
      periodcount=charcount(trim(RANAME)//c_null_char)
      write(*,*) 'Number of Nidek mires read: ',(periodcount-1)/360
      N=(periodcount-1)/360
      if (N .lt. 23 )then
       WRITE (*,*) 'Error on mire count in rcnvrtn'
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
       call system('rm ' // semicolon1, io)
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
       call system('rm ' // semicolon2, io)
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
       !  write(*,*) 'sed "s/;/ /g" ' // HTNAME // ' > ' // semicolon2
         call system('sed "s/;/ /g" ' // HTNAME // ' > ' // semicolon2, io)
         if (io > 0) then
          write (*,*) 'system command to sed failed'
          write (*,*) 'Consider using your text editor to search/replace all semicolons in data statements in',RANAME
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
         call system('rm ' // semicolon2, io)
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
       !  write(*,*) 'sed "s/;/ /g" ' // PENAME // ' > ' // semicolon2
         call system('sed "s/;/ /g" ' // PENAME // ' > ' // semicolon2, io)
         if (io > 0) then
          write (*,*) 'system command to sed failed'
          write (*,*) 'Consider using your text editor to search/replace all semicolons in data statements in',RANAME
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
          call system('rm ' // semicolon2, io)
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