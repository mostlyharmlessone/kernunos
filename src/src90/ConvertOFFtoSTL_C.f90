 SUBROUTINE ConvertOFFtoSTL_C(INAME,ONAME,deftype) bind(C,name='ConvertOFFtoSTL_C_')
! Reads OFF file created by WriteOFF and generates ASCII and binary STL files 
! default is ASCII without color, ONAME ending in ".bin.stl" will generate a binary STL file with a VisCam/Solidworks(deftype .eq.0) or Materials Magic(deftype .ne.0)color attribute.
! modified to be called from C/C++   
 !   https://www.khronos.org/opengl/wiki/Calculating_a_Surface_Normal
 !   https://en.wikipedia.org/wiki/STL_(file_format)
 !   https://en.wikipedia.org/wiki/OFF_(file_format)
!    https://fortran-lang.discourse.group/t/how-to-write-bytes-in-a-binary-file/763/7
!    https://stackoverflow.com/questions/41254019/reading-variable-length-data-in-fortran

  USE io_functions, ONLY  : get_new_fileunit
  USE special_fct, ONLY  : surface_normal,rgb2attr
  USE ISO_FORTRAN_ENV, ONLY : INT8,INT16,INT32,REAL32
  USE, INTRINSIC :: iso_c_binding, ONLY : c_float,c_int,c_char,c_null_char
  USE c_interfaces, ONLY : LogC
  IMPLICIT NONE
  CHARACTER(c_char), INTENT(INOUT), DIMENSION(4096) :: INAME,ONAME
  INTEGER(c_int), INTENT(IN) :: deftype
  CHARACTER(len=4096) :: new_path
  CHARACTER(:), ALLOCATABLE :: file_from_C
  INTEGER ::  nblines, file_idx
  LOGICAL :: exists
  CHARACTER(80) KH1
  CHARACTER(32) :: integerj,integerk
  INTEGER :: i,j,unitno1,ih,io,ierr,nvertices,nedges
  INTEGER  :: header(20)   !80-byte header
  INTEGER(INT8) :: onebyte
  INTEGER(INT16) :: attr
  INTEGER(INT32) :: nfaces,matlmagic
  REAL(REAL32), allocatable :: V(:,:) ! allocate nvertices
  INTEGER(kind=2), allocatable :: F(:,:),rgbv(:,:) ! allocate nfaces
  REAL(REAL32) :: normalvector(3),v1(3),v2(3),v3(3),vnorm
  CHARACTER(100) line !these are for variable length input (color or not)

!! this will have a lot of extra random non ASCII stuff after the file name
!! need this because GCC11 isn't F2018 compliant with deferred length character with Bind C
!! ie. can't do CHARACTER(*,c_char), INTENT(IN) :: file_from_C_1 with BIND(C) with GCC11
!! declaring CHARACTER(len=12), dimension(:), allocatable :: args with args(1) works too, but limited in length
!   Converting C char array to Fortran character.
    new_path = " "
    do i=1, 4096
        if ( INAME (i) == c_null_char ) then
            exit
        else
            new_path (i:i) = INAME (i)
        end if
    end do

  call LogC("input off file from kernunos: "//trim(new_path)//c_null_char)
  nblines=len(trim(new_path))
  allocate(CHARACTER(nblines) :: file_from_C)
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
   write(integerj, '(i0)') nfaces
   write(integerk, '(i0)') nvertices
   call LogC("Read OFF file with "//integerj//" faces and"//integerk//" vertices"//c_null_char)
!  STL format does not have connectivity and each face carries 3 vertices,leading to duplicate vertices
   write(integerk, '(i0)') 3*nfaces
   call LogC("Writing STL file with "//integerj//" faces and"//integerk//" (duplicate) vertices"//c_null_char)

    new_path = " "
    do i=1, 4096
        if ( ONAME (i) == c_null_char ) then
            exit
        else
            new_path (i:i) = ONAME (i)
        end if
    end do

  call LogC("output stl file from kernunos: "//trim(new_path)//c_null_char)
  nblines=len(trim(new_path)) 
  allocate(CHARACTER(nblines) :: file_from_C)
  file_from_C=trim(new_path)

! file_idx will be zero if .bin is not in the filename, ie only ONAME with .bin.stl or .bin in it will result in a binary file
  file_idx=index(file_from_C, ".bin")
    
!  Convert to ASCII or binary STL
   unitno1 = get_new_fileunit()
   if( file_idx == 0) then  ! not binary, must be ASCII
    open(unitno1, file=file_from_C, action="write", iostat=ierr)
   else
    if (index(file_from_C, ".stl") == 0) file_from_C = file_from_C // ".stl"  ! convert .bin to .bin.stl
    open(unitno1, file=file_from_C, access='stream', status='replace', &
       & action='write', iostat=io)
   endif

   if (ierr .eq. 0 .AND. io.eq.0) then
   if( file_idx == 0) then  ! not binary, must be ASCII      
     write(unitno1,*) 'solid Cornea'
   else
      header=0   !80-byte header
      if (deftype /= 1) then
       write(unitno1, iostat=io) header, nfaces ! binary STL header has no requirements if not Materials Magic
      else
      matlmagic=2**32-1
!     Materials Magic, COLOR=4 bytes RGBA,MATERIAL=12 bytes diffuse reflection,specular highlight ambient light colors
       write(unitno1, iostat=io) onebyte,'COLOR=',matlmagic,'MATERIAL=',matlmagic,matlmagic,matlmagic
       write(unitno1, iostat=io) 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'  !write 48 more bytes to bring header to 80 bytes
       write(unitno1, iostat=io) nfaces
      endif
   endif
!    write facets list; F() is indexed from zero
     do I=1,nfaces
      do j=1,3
       v1(j)=V(j,F(1,i)+1)
       v2(j)=V(j,F(2,i)+1)
       v3(j)=V(j,F(3,i)+1)
      end do
      normalvector=surface_normal(v1,v2,v3)
      vnorm=sqrt(dot_product(normalvector,normalvector))
      if (deftype == 0 .or. deftype == 1) then
       attr=rgb2attr(deftype,rgbv(:,i))
      else
       attr=0  ! always for binary STL files unless we want 15bit rgb color c/w VisCam/SolidView or Materials Magic
      endif
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
          
END SUBROUTINE ConvertOFFtoSTL_C




