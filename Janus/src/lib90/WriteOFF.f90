       subroutine WriteOFF(b,KXNAME)
       USE cornea_arrays
       USE set_precision, ONLY : wp
       TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
       character(len=*), intent(in) :: KXNAME 
       real(wp):: X1,X2,X3,vert1,vert2,vert3,vert4
       integer :: i,j,M1,N1,vertices,faces,edges
       integer :: ivert1,ivert2,ivert3,ivert4
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
        edges=(2*N1-1)*(M1-1)  ! don't do the last set of edges
       else
        faces=2*(N1-1)*M1  !triangles
        edges=(3*N1-2)*(M1-1)  
       endif
      else 
!      closed 
       vertices=M1*N1+1
       faces=(2*N1-1)*M1  !triangles
       edges=(3*N1-1)*(M1-1)  
      endif
!     write(12,*) vertices,faces,edges 
  
!      count the faces & edges, don't change the vertices or their numbering
       faces=0
       edges=0
       do i=1,M1-1
        do j=1,N1-1
        if  ( (j < b%MV(i)) .AND. (j < b%MV(i+1)) ) then           
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
!       for i=M1
        do j=1,N1-1
        if  ( (j < b%MV(M1)) .AND. (j < b%MV(1)) ) then                     
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

!      these HAVE to be written/formatted as integers for igl 
        
       do i=1,M1-1
        do j=1,N1-1
         ivert1=(i-1)*N1+j-1
         ivert2=(i-1)*N1+j
         ivert3=i*N1+j
         ivert4=i*N1+j-1
         if  ( (j < b%MV(i)) .AND. (j < b%MV(i+1)) ) then                      
          if (donut) then
           if (quad) then
            write(12,*) '4',ivert1,ivert2,ivert3,ivert4              
           else
            write(12,*) '3',ivert1,ivert2,ivert3                     
            write(12,*) '3',ivert3,ivert4,ivert1                                  
          endif
         else
!        nothing here yet                 
         endif
         endif
        end do
       end do  
       
!       Last one is different
!       i=M1 because "i+1"=M1, but second terms have 0 because "i" is (i-1)  
        do j=1,N1-1
         ivert1=(M1-1)*N1+j-1
         ivert2=(M1-1)*N1+j
         ivert3=j
         ivert4=j-1
         if  ( (j < b%MV(M1)) .AND. (j < b%MV(1)) ) then           
          if (donut) then
           if (quad) then
            write(12,*) '4',ivert1,ivert2,ivert3,ivert4    
           else
            write(12,*) '3',ivert1,ivert2,ivert3               
            write(12,*) '3',ivert3,ivert4,ivert1
           endif
         else
!        nothing here yet
         endif
         endif
        end do          
       	                            
       close (12)       
    
       end subroutine WriteOFF
