!      Writes ASCII PLY files with color overlay
!      modified for assimp to be vertex colors, c/w Geom.f90 too
       subroutine WriteGeomPLY(b,donut,powmin,powmax,PLYNAME)
       use io_functions, only : get_new_fileunit
       use cornea_arrays
       use set_precision, ONLY : wp
       use special_fct, only : rgb2, rgb5
       use ISO_FORTRAN_ENV, only: INT8,INT16,INT32,REAL32
       use, intrinsic ::  ieee_arithmetic
       TYPE(wpJMatrix),INTENT(IN) :: b
       character(len=*), intent(in) :: PLYNAME
       real(wp), intent(IN) :: powmin,powmax
       logical, intent(IN) :: donut
       real(wp) :: X1,X2,X3
       real(REAL32) :: vert1,vert2,vert3,nrm1,nrm2,nrm3,normal
       real(wp) :: pow
       integer :: i,j,M1,N1,verts,faces,edges,unitno1,ierr
       integer(INT32) :: ivert1,ivert2,ivert3,ivert4,vertnum
       logical :: quad
       integer(int16) :: rgbv(3)  

       quad = .FALSE.
       if (donut .AND. quad) then
        write(*,*) 'WriteGeom: Cannot have closed disk with quadrilaterals'
        stop
       endif
!      RGB colors can follow after list of faces       
!      255 0 0 #red
!      0 255 0 #green
!      0 0 255 #blue
       N1=size(b%r,1)
       M1=size(b%r,2)
       unitno1 = get_new_fileunit()
       open(unitno1, file=trim(PLYNAME), action="write", iostat=ierr)
       
!     if no missing faces
      if (donut) then
       verts=M1*N1
       if (quad) then
        faces=(N1-1)*M1    !quadrilaterals
        edges=(2*N1-1)*(M1-1)  ! don't do the last set of edges
       else
        faces=2*(N1-1)*M1  !triangles
        edges=(3*N1-2)*(M1-1)  
       endif
      else 
!      closed 
       verts=M1*N1+1
       faces=(2*N1-1)*M1  !triangles
       edges=(3*N1-1)*(M1-1)
      endif
  
!      count the faces & edges, don't change the vertices or their numbering
       faces=0
       edges=0
       do i=1,M1-1
        do j=1,N1-1
        if  ( (j < b%MV(i)) .AND. (j < b%MV(i+1)) ) then           
          if (quad) then
           faces=faces+1
           edges=edges+2
          else
           faces=faces+2
           edges=edges+3
          endif                
         endif
        end do
       end do       
!     Last one is different
!      for i=M1
        do j=1,N1-1
         if  ( (j < b%MV(M1)) .AND. (j < b%MV(1)) ) then                     
          if (quad) then
           faces=faces+1
           edges=edges+2
          else
           faces=faces+2
           edges=edges+3
          endif
         endif
        end do 
!      add inner bunch if no donut, no boundary check necessary
       if (donut .eqv. .FALSE.) then
        faces=faces+M1
        edges=edges+M1
       endif

!       HEADER for ASCII PLY
        write(unitno1,'(g0)') 'ply' 
        write(unitno1,'(g0)') 'format ascii 1.0'
        write(unitno1,'(g0)') 'comment modified to conform with Open Asset Import Library - http://assimp.sf.net (v5.3.0)'
        write(unitno1,*) 'element vertex ',verts         
        write(unitno1,'(g0)') 'property float x'        
        write(unitno1,'(g0)') 'property float y'       
        write(unitno1,'(g0)') 'property float z' 
        write(unitno1,'(g0)') 'property float nx'        
        write(unitno1,'(g0)') 'property float ny'       
        write(unitno1,'(g0)') 'property float nz'       
        write(unitno1,'(g0)') 'property uchar red'
        write(unitno1,'(g0)') 'property uchar green'
        write(unitno1,'(g0)') 'property uchar blue'        
        write(unitno1,'(g0)') 'property uchar alpha'
        write(unitno1,*) 'element face ',faces 
        write(unitno1,'(g0)') 'property list uchar int vertex_index'
        write(unitno1,'(g0)') 'end_header' 

!      in this version, colors/powers by vertex
!      pow=b%Zp(I,J)
!      rgbv=rgb5(pow,powmin,powmax)
!      Write vertices as REAL32
!      There are "unreferenced vertices" this way, but it is much easier with vertex numbering
       if (donut .eqv. .FALSE.) then ! add one last vertex at origin
         vert1 = 0_REAL32       
         vert2 = 0_REAL32
         X3=b%Z0(1)
         pow=b%SAGC0(1)       ! not just X3 for future painting
         nrm1=0
         nrm2=0
         nrm3=1
         vert3 = real(X3,kind=REAL32)
         if (ieee_is_finite(vert3)) then
          ! ok
         else
          vert3 = 0 ! for out of bound values  
          pow = 0 ! for out of bound values     
         endif  
         rgbv=rgb5(pow,powmin,powmax)                   
         write(unitno1,*) vert1,vert2,vert3,nrm1,nrm2,nrm3,rgbv,255   
       endif

       do i=1,M1
        do j=1,N1 
          X1=b%THT(i)
          X2=b%R(j,i)
          X3=b%Z(j,i)
          pow=b%SAGC(j,i)     ! not just X3 for future painting
          nrm1=-abs(b%YPR(j,i))      !get rid of spurious sign
          nrm2=-b%YPTHETA(j,i)/X2    !polar coordinates
          normal=sqrt(nrm1*nrm1+nrm2*nrm2+1)
          nrm1=nrm1/normal
          nrm2=nrm2/normal
          nrm3=1/normal
          vert1 = real(ABS(X2)*COS(X1),kind=REAL32)
          vert2 = real(ABS(X2)*SIN(X1),kind=REAL32)
          vert3 = real(X3,kind=REAL32)
         if (ieee_is_finite(vert1) .AND. ieee_is_finite(vert2) .AND. ieee_is_finite(vert3)) then
          ! ok
         else
          vert1 = 0_REAL32  ! for out of bound values
          vert2 = 0_REAL32  ! for out of bound values
          vert3 = 0_REAL32  ! for out of bound values
          pow = 0 ! for out of bound values
         endif
         rgbv=rgb5(pow,powmin,powmax)           
         write(unitno1,*) vert1,vert2,vert3,nrm1,nrm2,nrm3,rgbv,255
        end do
       end do

!      Faces HAVE to be written/formatted as integers(INT32) 
        if (donut .eqv. .FALSE.) then  ! inner set of faces
         do i=1,M1-1 ! j=1 and the origin j=0
          ivert2=(i-1)*N1+1
          ivert3=i*N1+1
          ivert1=0  ! verts from above zero indexing, origin given last vertex number
          vertnum=3
          write(unitno1,*) vertnum,ivert1,ivert2,ivert3
         end do
!        Last face is different
         ivert2=(M1-1)*N1+1
         ivert3=1
         ivert1=0   ! verts from above zero indexing, origin given last vertex number
         vertnum=3
         write(unitno1,*) vertnum,ivert1,ivert2,ivert3
        endif 
       
       do i=1,M1-1
        do j=1,N1-1
         if (donut) then
          ivert1=(i-1)*N1+j-1
          ivert2=(i-1)*N1+j
          ivert3=i*N1+j
          ivert4=i*N1+j-1
         else
          ivert1=(i-1)*N1+j
          ivert2=(i-1)*N1+j+1
          ivert3=i*N1+j+1
          ivert4=i*N1+j
         endif
         if  ( (j < b%MV(i)) .AND. (j < b%MV(i+1)) ) then   
           if (quad) then
            vertnum=4
            write(unitno1,*) vertnum,ivert1,ivert2,ivert3,ivert4            
           else 
            vertnum=3
            write(unitno1,*) vertnum,ivert1,ivert2,ivert3
            write(unitno1,*) vertnum,ivert3,ivert4,ivert1                                                 
           endif
         endif
        end do
       end do  
       
!       Last one is different
!       i=M1 because "i+1"=M1, but second terms have 0 because "i" is (i-1)  
        do j=1,N1-1
         if (donut) then
          ivert1=(M1-1)*N1+j-1
          ivert2=(M1-1)*N1+j
          ivert3=j
          ivert4=j-1
         else
          ivert1=(M1-1)*N1+j
          ivert2=(M1-1)*N1+j+1
          ivert3=j+1
          ivert4=j
         endif
         if  ( (j < b%MV(M1)) .AND. (j < b%MV(1)) ) then  
           if (quad) then
            vertnum=4
            write(unitno1,*) vertnum,ivert1,ivert2,ivert3,ivert4  
           else
            vertnum=3
            write(unitno1,*) vertnum,ivert1,ivert2,ivert3  
            write(unitno1,*) vertnum,ivert3,ivert4,ivert1             
           endif
         endif
        end do
        
       close (unitno1)     
    
       end subroutine WriteGeomPLY
