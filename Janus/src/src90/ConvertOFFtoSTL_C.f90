 subroutine ConvertOFFtoSTL_C(INAME,ONAME) bind(C,name='ConvertOFFtoSTL_C_')
! Reads OFF file created by WriteOFF and generates ASCII and binary STL files 
! default is ASCII without color, ONAME ending in ".bin.stl" will generate a binary STL file with a Solidworks color attribute.
! modified to be called from C/C++   
 !   https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal
 !   https://en.wikipedia.org/wiki/STL_(file_format)
 !   https://en.wikipedia.org/wiki/OFF_(file_format)
!    https://fortran-lang.discourse.group/t/how-to-write-bytes-in-a-binary-file/763/7
!    https://stackoverflow.com/questions/41254019/reading-variable-length-data-in-fortran

  use io_functions, only : get_new_fileunit
  use special_fct, only : surface_normal,rgb2attr
  use ISO_FORTRAN_ENV, only: INT8,INT16,INT32,REAL32
  use, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char
  implicit none
  character(c_char), INTENT(INOUT), DIMENSION(4096) :: INAME,ONAME
  character(len=4096) :: new_path
  character(:), ALLOCATABLE :: file_from_C
  integer ::  nblines, file_idx
  logical :: exists
  character(80) KH1 
  integer :: i,j,unitno1,ih,io,ierr,nvertices,nedges
  integer(INT8)  :: header(80)
  integer(INT16) :: attr
  integer(INT32) :: nfaces
  real(REAL32), allocatable :: V(:,:) ! allocate nvertices
  integer(kind=2), allocatable :: F(:,:),rgbv(:,:) ! allocate nfaces
  real(REAL32) :: normalvector(3),v1(3),v2(3),v3(3),vnorm
  character(100) line !these are for variable length input (color or not)

!! this will have a lot of extra random non ASCII stuff after the file name
!! need this because GCC11 isn't F2018 compliant with deferred length character with Bind C
!! ie. can't do CHARACTER(*,c_char), INTENT(IN) :: file_from_C_1 with BIND(C) with GCC11
!! declaring character(len=12), dimension(:), allocatable :: args with args(1) works too, but limited in length
!   Converting C char array to Fortran character.
    new_path = " "
    do i=1, 4096
        if ( INAME (i) == c_null_char ) then
            exit
        else
            new_path (i:i) = INAME (i)
        end if
    end do

  write(*,*) 'input off file from kernunos: ',trim(new_path)
  nblines=len(trim(new_path)) 
  allocate(character(nblines) :: file_from_C)
  file_from_C=trim(new_path)

  inquire(file=trim(new_path), exist=exists)
  if (exists) then
   unitno1 = get_new_fileunit()
   open(unitno1, file=trim(new_path), action="read", iostat=ierr)
   if (ierr .eq. 0) then 
!   READ HEADER       
    read(unitno1,*,IOSTAT=io) KH1
     if(io.GT.0) then
       write(*,*) 'ERROR ON INPUT'
       close(unitno1)
       return
     endif
     if (KH1.EQ.'OFF') then
!     vertices, faces, edges
      read(unitno1,*) nvertices,nfaces,nedges
      allocate(V(3,nvertices),F(3,nfaces),rgbv(3,nfaces))

!     read vertices list 
      do I=1,nvertices
       READ(unitno1,*) V(1,i),V(2,i),V(3,i) !these are the coordinates for each vertex
      end do 
!     read face list
      do I=1,nfaces
       read(unitno1,'(a)')line                 !read whole line as string'
       line=trim(line)//' 255 255 255'         !adds three 255's (white) to the string
       read(line,*) IH,F(:,i),rgbv(:,i)        !these are the vertex numbers for each face indexed from zero, 
                                               !ignores extra 255's if color info is already there
!      READ(unitno1,*) IH,F(:,i),rgbv(:,i)      !these are the vertex numbers for each face indexed from zero
       if (IH .NE. 3) then
        write(*,*) 'Error: Only reads triangular OFF files'
        close (unitno1)
        return
       endif
      end do 

!     FINISHED READING OFF FILE  
    else
        write(*,*) 'Could not read OFF signature'
        close(unitno1)
        return
    endif 

   close (unitno1)
   deallocate(file_from_C)

   write(*,*) 'Read OFF file with',nfaces,' faces and',nvertices,' vertices'
!  STL format does not have connectivity and each face carries 3 vertices,leading to duplicate vertices
   write(*,*) 'Writing STL file with',nfaces,' faces and',3*nfaces,' (duplicate) vertices'
    new_path = " "
    do i=1, 4096
        if ( ONAME (i) == c_null_char ) then
            exit
        else
            new_path (i:i) = ONAME (i)
        end if
    end do

  write(*,*) 'output stl file from kernunos: ',trim(new_path)
  nblines=len(trim(new_path)) 
  allocate(character(nblines) :: file_from_C)
  file_from_C=trim(new_path)

! file_idx will be zero if .bin.stl is not in the filename, ie only ONAME with .bin.stl in it will result in a binary file
  file_idx=index(file_from_C, ".bin.stl")
    
!  Convert to ASCII or binary STL
   header=0   ! binary STL header has no requirements
   unitno1 = get_new_fileunit()
   if( file_idx == 0) then  ! not binary, must be ASCII
    open(unitno1, file=trim(new_path), action="write", iostat=ierr)
   else
    open(unitno1, file=trim(new_path), access='stream', status='replace', &
       & action='write', iostat=io)
   endif

   if (ierr .eq. 0 .AND. io.eq.0) then
   if( file_idx == 0) then  ! not binary, must be ASCII      
     write(unitno1,*) 'solid Cornea'
   else
     write(unitno1, iostat=io) header, nfaces
   endif
!    write facets list; F() is indexed from zero
     attr=0  ! always for binary STL files unless we want 15bit rgb color c/w SolidView
!    bits 0 to 4 are the intensity level for blue (0 to 31),
!    bits 5 to 9 are the intensity level for green (0 to 31),
!    bits 10 to 14 are the intensity level for red (0 to 31),
!    bit 15 is 1 if the color is valid, or 0 if the color is not valid (as with normal STL files).
!    attr= b'0000001100000010'   ! 0000 0011 0000 0001 == blue 0, green 3, red 0, valid
     do I=1,nfaces
       do j=1,3
        v1(j)=V(j,F(1,i)+1)
        v2(j)=V(j,F(2,i)+1)
        v3(j)=V(j,F(3,i)+1)
       end do
       normalvector=surface_normal(v1,v2,v3)
       vnorm=sqrt(dot_product(normalvector,normalvector))
       attr=rgb2attr(rgbv(:,i))
     if( file_idx == 0) then  ! not binary, must be ASCII
       write(unitno1,*) 'facet normal ',normalvector/vnorm
       write(unitno1,*) '   outer loop'
       write(unitno1,*) '      vertex ',v1
       write(unitno1,*) '      vertex ',v2
       write(unitno1,*) '      vertex ',v3
       write(unitno1,*) '   endloop'
       write(unitno1,*) 'endfacet'
     else
       write(unitno1, iostat=io) normalvector,v1,v2,v3,attr
     endif
     end do
   if( file_idx == 0) then  ! not binary, must be ASCII
     write(unitno1,*) 'endsolid Cornea'             
     close(unitno1)
   else
     close(unitno1, iostat=io)
   endif

    else
     print*, "Error ",ierr,io," attempting to open file ",trim(new_path)
     return
    endif
   endif

   deallocate(V,F,rgbv)
   deallocate(file_from_C)

   else
    print*, "Error -- cannot find file: ",trim(new_path)
    return
   endif    
          
end subroutine ConvertOFFtoSTL_C




