!      Writes OFF file with color overlay
!      powers/colors on faces
       subroutine WriteGeomOFF(flag,b,donut,powmin,powmax,OFFNAME)
       use io_functions, only : get_new_fileunit
       use cornea_arrays
       use set_precision, ONLY : wp
       use special_fct, only : colormap
       use ISO_FORTRAN_ENV, only: INT8,INT16,INT32,REAL32
       use, intrinsic :: iso_c_binding, ONLY : c_float,c_int      
       use, intrinsic ::  ieee_arithmetic
       TYPE(wpJMatrix),INTENT(IN) :: b
       character(len=*), intent(in) :: OFFNAME
       real(wp), intent(IN) :: powmin,powmax
       logical, intent(IN) :: donut
       integer(c_int), INTENT(INOUT) :: flag       
       real(wp) :: X1,X2,X3
       real(REAL32) :: vert1,vert2,vert3
       real(wp) :: pow_vert1,pow_vert2,pow_vert3,pow_vert4,pow_face4,pow_face3_1,pow_face3_2
       integer :: i,j,M1,N1,verts,faces,edges,unitno3,ierr,map,fct
       integer(INT32) :: ivert1,ivert2,ivert3,ivert4,vertnum
       logical :: quad
       integer(int16) :: rgbv(3)

       map=mod((flag-mod(flag,100))/100,100)
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
       unitno3 = get_new_fileunit()
       open(unitno3, file=trim(OFFNAME), action="write", iostat=ierr)
       
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

!      HEADER for OFF 
       write(unitno3,'(g0)') 'OFF'
       write(unitno3,*) verts,faces,edges

!      Write vertices as REAL32
!      There are "unreferenced vertices" this way, but it is much easier with vertex numbering
       if (donut .eqv. .FALSE.) then ! add one last vertex at origin
         vert1 = 0_REAL32       
         vert2 = 0_REAL32
         X3=b%Z0(1)
         vert3 = real(X3,kind=REAL32)
         if (ieee_is_finite(vert3)) then
          ! ok
         else
          vert3 = 0 ! for out of bound values       
         endif                     
         write(unitno3,*) vert1,vert2,vert3   
       endif

       do i=1,M1
        do j=1,N1 
          X1=b%THT(i)
          X2=b%R(j,i)
          X3=b%Z(j,i)
          vert1 = real(ABS(X2)*COS(X1),kind=REAL32)
          vert2 = real(ABS(X2)*SIN(X1),kind=REAL32)
          vert3 = real(X3,kind=REAL32)
         if (ieee_is_finite(vert1) .AND. ieee_is_finite(vert2) .AND. ieee_is_finite(vert3)) then
          ! ok
         else
          vert1 = 0_REAL32  ! for out of bound values
          vert2 = 0_REAL32  ! for out of bound values
          vert3 = 0_REAL32  ! for out of bound values
         endif           
         write(unitno3,*) vert1,vert2,vert3
        end do
       end do

!      Faces HAVE to be written/formatted as integers(INT32) 
        if (donut .eqv. .FALSE.) then  ! inner set of faces
         do i=1,M1-1 ! j=1 and the origin j=0
          ivert2=(i-1)*N1+1
          ivert3=i*N1+1
          ivert1=0  ! verts from above zero indexing, origin given last vertex number
          fct=mod(((flag-mod(flag,10000))/10000),100)
          if (fct .lt. 16 .and. fct .gt. 0) then
             pow_vert1=b%ZC(1,i,fct)
             pow_vert2=b%ZC(1,i+1,fct)
             pow_vert3=b%ZC0(1,fct)
          else
          SELECT CASE (fct)
            CASE (0)
              pow_vert1=b%SAGC(1,i)
              pow_vert2=b%SAGC(1,i+1)
              pow_vert3=b%SAGC0(1)
            CASE (16)
              pow_vert1=b%INSTC(1,i)
              pow_vert2=b%INSTC(1,i+1)
              pow_vert3=b%INSTC0(1)
            CASE (17)
              pow_vert1=b%INSTC2(1,i)
              pow_vert2=b%INSTC2(1,i+1)
              pow_vert3=b%INSTC20(1)
            CASE (18)
              pow_vert1=b%MEANC(1,i)
              pow_vert2=b%MEANC(1,i+1)
              pow_vert3=b%MEANC0(1)
            CASE (19)
              pow_vert1=b%MONGEA(1,i)
              pow_vert2=b%MONGEA(1,i+1)
              pow_vert3=b%MONGEA0(1)
            CASE (20)
              pow_vert1=b%Z(1,i)
              pow_vert2=b%Z(1,i+1)
              pow_vert3=b%Z0(1)
            CASE (21)
              pow_vert1=b%OBSC(1,i)
              pow_vert2=b%OBSC(1,i+1)
              pow_vert3=b%OBSC0(1)
            CASE DEFAULT
              pow_vert1=b%SAGC(1,i)
              pow_vert2=b%SAGC(1,i+1)
              pow_vert3=b%SAGC0(1)
         END SELECT
         endif
          pow_face3_1=(pow_vert1+pow_vert2+pow_vert3)/3
          if (ieee_is_finite(pow_face3_1) ) then  !.and. (powmax-powmin) > eps
           vertnum=3
           rgbv=colormap(pow_face3_1,powmin,powmax,map)
          else
           write(*,*) 'WriteGeom: Error in central values'
           stop
          endif
          write(unitno3,*) vertnum,ivert1,ivert2,ivert3,rgbv
         end do
!        Last face is different
         ivert2=(M1-1)*N1+1
         ivert3=1
         ivert1=0   ! verts from above zero indexing, origin given last vertex number
         fct=mod(((flag-mod(flag,10000))/10000),100)
         if (fct .lt. 16 .and. fct .gt. 0) then
            pow_vert1=b%ZC(1,M1,fct)
            pow_vert2=b%ZC(1,1,fct)
            pow_vert3=b%ZC0(1,fct)
         else
         SELECT CASE (fct)
           CASE (0)
             pow_vert1=b%SAGC(1,M1)
             pow_vert2=b%SAGC(1,1)
             pow_vert3=b%SAGC0(1)
           CASE (16)
             pow_vert1=b%INSTC(1,M1)
             pow_vert2=b%INSTC(1,1)
             pow_vert3=b%INSTC0(1)
           CASE (17)
             pow_vert1=b%INSTC2(1,M1)
             pow_vert2=b%INSTC2(1,1)
             pow_vert3=b%INSTC20(1)
           CASE (18)
             pow_vert1=b%MEANC(1,M1)
             pow_vert2=b%MEANC(1,1)
             pow_vert3=b%MEANC0(1)
           CASE (19)
             pow_vert1=b%MONGEA(1,M1)
             pow_vert2=b%MONGEA(1,1)
             pow_vert3=b%MONGEA0(1)
           CASE (20)
             pow_vert1=b%Z(1,M1)
             pow_vert2=b%Z(1,1)
             pow_vert3=b%Z0(1)
           CASE (21)
             pow_vert1=b%OBSC(1,M1)
             pow_vert2=b%OBSC(1,1)
             pow_vert3=b%OBSC0(1)
           CASE DEFAULT
             pow_vert1=b%SAGC(1,M1)
             pow_vert2=b%SAGC(1,1)
             pow_vert3=b%SAGC0(1)
        END SELECT
        endif
         pow_face3_1=(pow_vert1+pow_vert2+pow_vert3)/3
         vertnum=3
         rgbv=colormap(pow_face3_1,powmin,powmax,map)
         write(unitno3,*) vertnum,ivert1,ivert2,ivert3,rgbv
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
!        powers go by vertices, but colors need by face
!        rgbv=colormap(pow,powmin,powmax,map)
         fct=mod(((flag-mod(flag,10000))/10000),100)
         if (fct .lt. 16 .and. fct .gt. 0) then
           pow_vert1=b%ZC(j+1,i,fct)
           pow_vert2=b%ZC(j+1,i+1,fct)
           pow_vert3=b%ZC(j,i,fct)
           pow_vert4=b%ZC(j,i+1,fct)
         else
         SELECT CASE (fct)
           CASE (0)
             pow_vert1=b%SAGC(j+1,i)
             pow_vert2=b%SAGC(j+1,i+1)
             pow_vert3=b%SAGC(j,i)
             pow_vert4=b%SAGC(j,i+1)
           CASE (16)
             pow_vert1=b%INSTC(j+1,i)
             pow_vert2=b%INSTC(j+1,i+1)
             pow_vert3=b%INSTC(j,i)
             pow_vert4=b%INSTC(j,i+1)
           CASE (17)
             pow_vert1=b%INSTC2(j+1,i)
             pow_vert2=b%INSTC2(j+1,i+1)
             pow_vert3=b%INSTC2(j,i)
             pow_vert4=b%INSTC2(j,i+1)
           CASE (18)
             pow_vert1=b%MEANC(j+1,i)
             pow_vert2=b%MEANC(j+1,i+1)
             pow_vert3=b%MEANC(j,i)
             pow_vert4=b%MEANC(j,i+1)
           CASE (19)
             pow_vert1=b%MONGEA(j+1,i)
             pow_vert2=b%MONGEA(j+1,i+1)
             pow_vert3=b%MONGEA(j,i)
             pow_vert4=b%MONGEA(j,i+1)
           CASE (20)
             pow_vert1=b%Z(j+1,i)
             pow_vert2=b%Z(j+1,i+1)
             pow_vert3=b%Z(j,i)
             pow_vert4=b%Z(j,i+1)
           CASE (21)
             pow_vert1=b%OBSC(j+1,i)
             pow_vert2=b%OBSC(j+1,i+1)
             pow_vert3=b%OBSC(j,i)
             pow_vert4=b%OBSC(j,i+1)
           CASE DEFAULT
             pow_vert1=b%SAGC(j+1,i)
             pow_vert2=b%SAGC(j+1,i+1)
             pow_vert3=b%SAGC(j,i)
             pow_vert4=b%SAGC(j,i+1)
        END SELECT
        endif
         pow_face4=(pow_vert1+pow_vert2+pow_vert3+pow_vert4)/4
         pow_face3_1=(pow_vert1+pow_vert2+pow_vert3)/3
         pow_face3_2=(pow_vert1+pow_vert3+pow_vert4)/3
           if (quad) then
            vertnum=4
            rgbv=colormap(pow_face4,powmin,powmax,map)
            write(unitno3,*) vertnum,ivert1,ivert2,ivert3,ivert4,rgbv             
           else 
            vertnum=3
            rgbv=colormap(pow_face3_1,powmin,powmax,map)
            write(unitno3,*) vertnum,ivert1,ivert2,ivert3,rgbv
            rgbv=colormap(pow_face3_2,powmin,powmax,map)
            write(unitno3,*) vertnum,ivert3,ivert4,ivert1,rgbv
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
!        powers go by vertices, but colors need by face
!        rgbv=colormap(pow,powmin,powmax,map)
         fct=mod(((flag-mod(flag,10000))/10000),100)
         if (fct .lt. 16 .and. fct .gt. 0) then
           pow_vert1=b%ZC(j+1,M1,fct)
           pow_vert2=b%ZC(j+1,M1-1,fct)
           pow_vert3=b%ZC(j,M1,fct)
           pow_vert4=b%ZC(j,M1-1,fct)
         else
         SELECT CASE (fct)
           CASE (0)
             pow_vert1=b%SAGC(j+1,M1)
             pow_vert2=b%SAGC(j+1,M1-1)
             pow_vert3=b%SAGC(j,M1)
             pow_vert4=b%SAGC(j,M1-1)
           CASE (16)
             pow_vert1=b%INSTC(j+1,M1)
             pow_vert2=b%INSTC(j+1,M1-1)
             pow_vert3=b%INSTC(j,M1)
             pow_vert4=b%INSTC(j,M1-1)
           CASE (17)
             pow_vert1=b%INSTC2(j+1,M1)
             pow_vert2=b%INSTC2(j+1,M1-1)
             pow_vert3=b%INSTC2(j,M1)
             pow_vert4=b%INSTC2(j,M1-1)
           CASE (18)
             pow_vert1=b%MEANC(j+1,M1)
             pow_vert2=b%MEANC(j+1,M1-1)
             pow_vert3=b%MEANC(j,M1)
             pow_vert4=b%MEANC(j,M1-1)
           CASE (19)
             pow_vert1=b%MONGEA(j+1,M1)
             pow_vert2=b%MONGEA(j+1,M1-1)
             pow_vert3=b%MONGEA(j,M1)
             pow_vert4=b%MONGEA(j,M1-1)
           CASE (20)
             pow_vert1=b%Z(j+1,M1)
             pow_vert2=b%Z(j+1,M1-1)
             pow_vert3=b%Z(j,M1)
             pow_vert4=b%Z(j,M1-1)
           CASE (21)
             pow_vert1=b%OBSC(j+1,M1)
             pow_vert2=b%OBSC(j+1,M1-1)
             pow_vert3=b%OBSC(j,M1)
             pow_vert4=b%OBSC(j,M1-1)
           CASE DEFAULT
             pow_vert1=b%SAGC(j+1,M1)
             pow_vert2=b%SAGC(j+1,M1-1)
             pow_vert3=b%SAGC(j,M1)
             pow_vert4=b%SAGC(j,M1-1)
        END SELECT
        endif
         pow_face4=(pow_vert1+pow_vert2+pow_vert3+pow_vert4)/4
         pow_face3_1=(pow_vert1+pow_vert2+pow_vert3)/3
         pow_face3_2=(pow_vert1+pow_vert3+pow_vert4)/3
           if (quad) then
            vertnum=4
            rgbv=colormap(pow_face4,powmin,powmax,map)
            write(unitno3,*) vertnum,ivert1,ivert2,ivert3,ivert4,rgbv   
           else
            vertnum=3
            rgbv=colormap(pow_face3_1,powmin,powmax,map)
            write(unitno3,*) vertnum,ivert1,ivert2,ivert3,rgbv 
            rgbv=colormap(pow_face3_2,powmin,powmax,map)
            write(unitno3,*) vertnum,ivert3,ivert4,ivert1,rgbv
           endif
         endif
        end do
        
       close (unitno3)      
    
       end subroutine WriteGeomOFF
