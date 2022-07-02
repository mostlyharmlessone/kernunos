!      Generates matrices for openGL

       subroutine Geom(b,powmin,powmax)
       use cornea_arrays
       use set_precision, ONLY : wp
       use c_interfaces, ONLY : OpenGL_Show
       use special_fct, only : rgb2, rgb5
       use, intrinsic :: iso_c_binding
       use ISO_FORTRAN_ENV, only: INT16,REAL32,stdin=>input_unit     
       TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
       real(wp), intent(IN) :: powmin,powmax 
       real(wp) :: X1,X2,X3
       real(REAL32) :: vert1,vert2,vert3       
       real(c_float) :: c_vert(3),c_rgbv(3)
       real(wp) :: pow
       integer :: i,j,k,M1,N1,verts,faces,edges
       integer(c_int) :: ivert1,ivert2,ivert3,ivert4
       logical :: donut
!      Can't use allocatable matrices to transfer to C, but use donut settings here
       integer(c_int) :: elements(0:6*(size(b%r,2)-1)*size(b%r,1)) ! faces x 3   index 0
       real(c_float) :: vertices(0:6*size(b%r,2)*size(b%r,1))      ! vertices x 6
       
       N1=size(b%r,2)
       M1=size(b%r,1)
       
      donut = .TRUE.
!     if no missing faces
      if (donut) then
        verts=M1*N1
        faces=2*(N1-1)*M1  !triangles
        edges=(3*N1-2)*(M1-1)  
      else 
!     closed 
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
         if (donut) then
           faces=faces+2
           edges=edges+3
         else
!        nothing here yet for .donut. .EQ. FALSE                 
         endif
        endif
        end do
       end do  
       
!       Last one is different
!       for i=M1
        do j=1,N1-1
        if  ( (j < b%MV(M1)) .AND. (j < b%MV(1)) ) then                     
         if (donut) then
           faces=faces+2
           edges=edges+3
         else
!        nothing here yet for .donut. .EQ. FALSE
         endif
        endif
        end do 

!       RGB colors can follow after vertices       
!       255 0 0 #red
!       0 255 0 #green
!       0 0 255 #blue
!      Write vertices as c_float
!      Unreferenced vertices, but much easier numbering this way
       k=-6 
       do i=1,M1
        do j=1,N1
!        faces
        if (i .LT. M1 .AND. j .LT. N1 ) then     
          ivert1=(i-1)*N1+j-1
          ivert2=(i-1)*N1+j
          ivert3=i*N1+j
          ivert4=i*N1+j-1
             
          if  ( (j < b%MV(i)) .AND. (j < b%MV(i+1)) ) then                    
           if (donut) then 
            k=k+6     ! matrix index
            elements(k:k+5)=(/ivert1,ivert2,ivert3,ivert3,ivert4,ivert1/)                                  
           else
!          nothing here yet for .donut. .EQ. FALSE                
           endif
          endif  
         endif !faces
         
!        vertices         
         X1=b%thta(i)
         X2=b%r(i,j)
         X3=b%Zp(i,j)   
         vert1 = ABS(X2)*COS(X1) 
         vert2 = ABS(X2)*SIN(X1) 
         vert3 = X3
         c_vert=(/vert1,vert2,vert3/)
         pow=b%Zp(I,J) 
         c_rgbv=rgb5(pow,powmin,powmax)        
         vertices(k:k+5)=(/c_vert,c_rgbv/) 
                                
        end do
       end do  
        write(*,*) k       
!       Last set of faces is different
!       i=M1 because "i+1"=M1, but second terms have 0 because "i" is (i-1)  
        do j=1,N1-1
         ivert1=(M1-1)*N1+j-1
         ivert2=(M1-1)*N1+j
         ivert3=j
         ivert4=j-1
         if  ( (j < b%MV(M1)) .AND. (j < b%MV(1)) ) then           
          if (donut) then
           k=k+6     ! matrix index
           elements(k:k+5)=(/ivert1,ivert2,ivert3,ivert3,ivert4,ivert1/)             
          else
!         nothing here yet for .donut. .EQ. FALSE
          endif
         endif
        end do  
        
        write(*,*) k
        write(*,*) 6*M1*N1
        read(stdin,*)   ! the new pause
        do k=1,6*M1*N1
         write(*,*) vertices(k)
        end do
        read(stdin,*)
             
       call OpenGL_Show(vertices, elements)
    
       end subroutine Geom
