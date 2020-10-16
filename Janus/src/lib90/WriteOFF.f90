       subroutine WriteOFF(b,KXNAME)
       USE cornea_arrays
       USE set_precision, ONLY : wp
       TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
       character(len=*), intent(in) :: KXNAME 
       real(wp):: X1,X2,X3,R,vert1,vert2,vert3,vert4
       integer :: i,j,M1,N1,vertices,faces,edges
       logical :: donut,quad

!       RGB colors can follow after list of faces       
!       255 0 0 #red
!       0 255 0 #green
!       0 0 255 #blue
       
       N1=size(b%r,2)
       M1=MM

       open (UNIT = 12, FILE = KXNAME)
       
       write(12,*) 'OFF'
       
      donut = .TRUE.
      quad = .FALSE.
!     if no missing faces
      if (donut) then
       vertices=M1*N1
       if (quad) then
        faces=(N1-1)*M1    !quadrilaterals
        edges=(2*N1-1)*M1  
       else
        faces=2*(N1-1)*M1  !triangles
        edges=(3*N1-2)*M1  
       endif
      else 
!      closed 
       vertices=M1*N1+1
       faces=(2*N1-1)*M1  !triangles
       edges=(3*N1-1)*M1  
      endif
!     write(12,*) vertices,faces,edges 
  
!      count the faces & edges, don't change the vertices or their numbering
       faces=0
       edges=0
       do i=1,M1-1
        do j=1,N1-1
         if ((ABS(b%Zp(i,j)) > 0) .AND. (ABS(b%Zp(i+1,j)) > 0) .AND. &
           & (ABS(b%Zp(i,j+1)) > 0) .AND. (ABS(b%Zp(i+1,j+1)) > 0)) then
         if (donut) then
          if (quad) then
           faces=faces+1
           edges=edges+2
          else
           faces=faces+2
           edges=edges+3
          endif
         else
!        nothing here yet                 
         endif
         endif
        end do
       end do  
       
!       Last one is different
!       i=M1, but second terms have i=1
        i=M1
        do j=1,N1-1
         if ((ABS(b%Zp(1,j)) > 0) .AND. (ABS(b%Zp(M1,j)) > 0) .AND. & 
           & (ABS(b%Zp(1,j+1)) > 0) .AND. (ABS(b%Zp(M1,j+1)) > 0)) then
         if (donut) then
          if (quad) then
           faces=faces+1
           edges=edges+2
          else
           faces=faces+2
           edges=edges+3
          endif
         else
!        nothing here yet
         endif
         endif
        end do 

       write(12,*) vertices,faces,edges
                                                                    
       do i=1,M1
        do j=1,N1
         X1=b%thta(i)
         X2=b%r(i,j)
         X3=b%Zp(i,j)   
         vert1 = ABS(X2)*COS(X1) 
         vert2 = ABS(X2)*SIN(X1) 
         vert3 = X3            
         write(12,*) vert1,vert2,vert3
        end do
       end do
        
       do i=1,M1-1
        do j=1,N1-1
         vert1=(i-1)*N1+j-1
         vert2=(i-1)*N1+j
         vert3=i*N1+j
         vert4=i*N1+j-1
         if ((ABS(b%Zp(i,j)) > 0) .AND. (ABS(b%Zp(i+1,j)) > 0) .AND. &
           & (ABS(b%Zp(i,j+1)) > 0) .AND. (ABS(b%Zp(i+1,j+1)) > 0)) then
         if (donut) then
          if (quad) then
           write(12,*) '4',vert1,vert2,vert3,vert4              
          else
           write(12,*) '3',vert1,vert2,vert3                     
           write(12,*) '3',vert3,vert4,vert1                                  
          endif
         else
!        nothing here yet                 
         endif
         endif
        end do
       end do  
       
!       Last one is different
!       i=M1, but second terms have i=1
        i=M1
        do j=1,N1-1
         vert1=(i-1)*N1+j-1
         vert2=(i-1)*N1+j
         vert3=N1+j
         vert4=N1+j-1
         if ((ABS(b%Zp(1,j)) > 0) .AND. (ABS(b%Zp(M1,j)) > 0) .AND. & 
           & (ABS(b%Zp(1,j+1)) > 0) .AND. (ABS(b%Zp(M1,j+1)) > 0)) then
         if (donut) then
          if (quad) then
           write(12,*) '4',vert1,vert2,vert3,vert4    
          else
           write(12,*) '3',vert1,vert2,vert3               
           write(12,*) '3',vert3,vert4,vert1
          endif
         else
!        nothing here yet
         endif
         endif
        end do          
       	                            
       close (12)       
    
       end subroutine WriteOFF
