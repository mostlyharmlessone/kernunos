!      Generates matrices for openGL

       subroutine Geom(flag, b, donut, powmin, powmax, elements, vertices, nV, nE)
       use cornea_arrays, ONLY : wpRadSlopeMatrix
       use set_precision, ONLY : wp
       use c_interfaces, ONLY : OpenGL_Show
       use special_fct, only : rgb2, rgb5
       use, intrinsic :: iso_c_binding, ONLY : c_float,c_int
       use, intrinsic ::  ieee_arithmetic
       use ISO_FORTRAN_ENV, only: stdin=>input_unit     ! for the pause read(stdin,*)
       TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
       real(wp), intent(IN) :: powmin,powmax 
       real(wp) :: X1,X2,X3
       real(wp) :: vert1,vert2,vert3       
       real(c_float) :: c_vert(3),c_rgbv(3)
       real(wp) :: pow
       integer :: i,j,k,M1,N1,verts,faces,edges
       integer(c_int) :: ivert1,ivert2,ivert3,ivert4
       integer(c_int), INTENT(INOUT) :: elements(*)                          ! faces x 3   
       real(c_float), INTENT(INOUT) :: vertices(*)                           ! vertices x 6 
       integer(c_int), INTENT(INOUT) :: flag, nE, nV                         ! call openGL or not
       logical, intent(IN) :: donut
       logical :: quad           
       N1=size(b%r,2)
       M1=size(b%r,1)

       quad = .FALSE.
       if (donut .AND. quad) then
        write(*,*) 'Geom: Cannot have closed disk with quadrilaterals'
        stop
       endif       
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
!     write(12,*) verts,faces,edges
  
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
!      Last one is different
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
        edges=edges+M1-1
       endif 
 
       write(*,*) 'Writing Geom for OpenGL'
!       RGB colors can follow after vertices       
!       255 0 0 #red
!       0 255 0 #green
!       0 0 255 #blue
!      Write vertices as c_float
!      Unreferenced vertices, but much easier numbering this way
!      vertices        
       k=1

       if (donut .eqv. .FALSE.) then ! add one last vertex at origin
         vert1 = 0      
         vert2 = 0
!         X3=b%ZpOrigin 
         X3=0.0        
         if (ieee_is_NaN(X3)) then
          vert3 = 0  ! for out of bound values
          pow = 0           ! for out of bound values          
         else
          vert3 = X3
 !         pow=b%ZpOrigin     ! not just X3 for future painting
          pow=40          
         endif                    
         c_vert=real((/vert1,vert2,vert3/),kind=4)  ! explicitly cast to kind=4 for consistent with c_float
         c_rgbv=rgb5(pow,powmin,powmax)/255.0  !openGL wants scale of 1.0 not 255        
         vertices(k:k+5)=(/c_vert,c_rgbv/)
         k=k+6      ! matrix index          
       endif

       do i=1,M1
        do j=1,N1
         X1=b%thta(i)
         X2=b%r(i,j)
         X3=b%Zp(i,j)   
         vert1 = ABS(X2)*COS(X1)   !explicitly make these c/w c_float
         vert2 = ABS(X2)*SIN(X1)
         if (ieee_is_NaN(X3)) then
          vert3 = 0  ! for out of bound values
          pow = 0           ! for out of bound values           
         else
          vert3 = X3
          pow=b%Zp(i,j)     ! not just X3 for future painting         
         endif
         c_vert=real((/vert1,vert2,vert3/),kind=4)  ! explicitly cast to kind=4 for consistent with c_float
         c_rgbv=rgb5(pow,powmin,powmax)/255.0  !openGL wants scale of 1.0 not 255        
         vertices(k:k+5)=(/c_vert,c_rgbv/)
         k=k+6      ! matrix index
        end do
       end do
        
       nV=k-1
       
!      faces  
       k=1 
        if (donut .eqv. .FALSE.) then  ! inner set of faces
         do i=1,M1-1 ! j=1 and the origin
          ivert2=(i-1)*N1+1
          ivert3=i*N1+1
          ivert1=0  ! verts from above zero indexing, origin given last vertex number
!         no boundary check on inner
          elements(k:k+2)=(/ivert1,ivert2,ivert3/)             
          k=k+3     ! matrix index
         end do
!        Last face is different
         ivert2=(M1-1)*N1+1
         ivert3=1
         ivert1=0   ! verts from above zero indexing, origin given last vertex number
!        no boundary check on inner
         elements(k:k+2)=(/ivert1,ivert2,ivert3/)             
         k=k+3     ! matrix index     
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
            elements(k:k+5)=(/ivert1,ivert2,ivert3,ivert3,ivert4,ivert1/) 
            k=k+6                                                   
          endif                                  
        end do
       end do       
!       Last set of faces is different
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
           elements(k:k+5)=(/ivert1,ivert2,ivert3,ivert3,ivert4,ivert1/)             
           k=k+6     ! matrix index
         endif
        end do 
       
        nE=k-1
        
       !write(*,*) "Enter/Return to Continue.."  
       !read(stdin,*)  ! the new pause needs use ISO_FORTRAN_ENV, only: stdin=>input_unit
       if (flag > 0) then
        write(*,*) 'Display in separate OpenGL window'                           
        call OpenGL_Show(vertices, elements, nV, nE)  ! glfw program incompatible with Jupiter/wxWidgets
       endif

       end subroutine Geom
