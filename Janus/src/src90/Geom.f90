!      Generates matrices for openGL

       subroutine Geom(flag, b, donut, powmin, powmax, elements, vertices, nV, nE)
       use cornea_arrays, ONLY : wpJMatrix
       use set_precision, ONLY : wp
       use special_fct, only : colormap
       use, intrinsic :: iso_c_binding, ONLY : c_float,c_int
       use, intrinsic ::  ieee_arithmetic
       use ISO_FORTRAN_ENV, only: stdin=>input_unit     ! for the pause read(stdin,*)
       IMPLICIT NONE
       TYPE(wpJMatrix),INTENT(IN) :: b      
       real(wp), intent(INOUT) :: powmin,powmax
       real(wp) :: X1,X2,X3
       real(wp) :: vert1,vert2,vert3,nrm1,nrm2,nrm3,normal
       real(c_float) :: c_vert(3),c_rgbv(3),c_norm(3)
       real(wp) :: pow
       integer :: i,j,k,kk,M1,N1,verts,faces,edges,map,fct
       integer(c_int) :: ivert1,ivert2,ivert3,ivert4
       integer(c_int), INTENT(INOUT) :: elements(*)                          ! faces x 3   
       real(c_float), INTENT(INOUT) :: vertices(*)                           ! vertices x 6 
       integer(c_int), INTENT(INOUT) :: flag, nE, nV
       logical, intent(IN) :: donut
       logical :: quad

       N1=size(b%r,1)
       M1=size(b%r,2)
       map=mod((flag-mod(flag,100))/100,100)

       quad = .FALSE.
       if (donut .AND. quad) then
        write(*,*) 'Geom: Cannot have closed disk with quadrilaterals'
        return
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
       kk=1
       if (donut .eqv. .FALSE.) then ! add one last vertex at origin
         vert1 = 0      
         vert2 = 0
         X3=-b%Z0(1)          ! flip it upside down
         fct=mod(((flag-mod(flag,10000))/10000),100)
         if (fct .lt. 16 .and. fct .gt. 0) then
              pow=b%ZC0(1,fct)
         else
         SELECT CASE (fct)
           CASE (0)
              pow=b%SAGC0(1)
           CASE (16)
              pow=b%INSTC0(1)
           CASE (17)
              pow=b%INSTC20(1)
           CASE (18)
              pow=b%MEANC0(1)
           CASE (19)
              pow=b%MONGEA0(1)
           CASE (20)
              pow=b%Z0(1)
           CASE (21)
              pow=b%Warp0(1)
           CASE DEFAULT
              pow=b%SAGC0(1)
        END SELECT
        endif
         vert3 = real(X3,kind=4)
         nrm1=0
         nrm2=0
         nrm3=1
         if (ieee_is_finite(vert3) .and. ieee_is_finite(pow)) then         
          c_rgbv=colormap(pow,powmin,powmax,map)/255.0  !openGL wants scale of 1.0 not 255
         else
          vert3 = 0         ! for out of bound values
          c_rgbv = (/1,1,1/)
         endif   
         c_norm=real((/nrm1,nrm2,nrm3/),kind=4)  ! explicitly cast to kind=4 for consistent with c_float                          
         c_vert=real((/vert1,vert2,vert3/),kind=4)  ! explicitly cast to kind=4 for consistent with c_float
         vertices(k:k+8)=(/c_vert,c_norm,c_rgbv/)
         k=k+9      ! matrix index        
       endif

       do i=1,M1
        do j=1,N1
         X1=b%THT(i)         ! in radians
         X2=b%R(j,i)
         X3=-b%Z(j,i)         ! flip it
         fct=mod(((flag-mod(flag,10000))/10000),100)
         if  ( j <= b%MV(i)) then
         if (fct .lt. 16 .and. fct .gt. 0) then
              pow=b%ZC(j,i,fct)
         else
         SELECT CASE (fct)
           CASE (0)
              pow=b%SAGC(j,i)
           CASE (16)
               pow=b%INSTC(j,i)
           CASE (17)
              pow=b%INSTC2(j,i)
           CASE (18)
              pow=b%MEANC(j,i)
           CASE (19)
              pow=b%MONGEA(j,i)
           CASE (20)
              pow=b%Z(j,i)
           CASE (21)
              pow=b%Warp(j,i)
           CASE DEFAULT
              pow=b%SAGC(j,i)
        END SELECT
        endif
        else
           pow = powmax  ! outside range
         endif
         nrm1=-abs(b%YPR(j,i))      !get rid of spurious sign
         nrm2=-b%YPTHETA(j,i)/X2    !polar coordinates
         normal=sqrt(nrm1*nrm1+nrm2*nrm2+1)
         if (normal .ne. 0) then
          nrm1=nrm1/normal
          nrm2=nrm2/normal
          nrm3=1/normal
         else
          nrm1 = 0         ! for out of bound values
          nrm2 = 0         ! for out of bound values
          nrm3 = 1         ! for out of bound values
         endif
         vert1 = real(ABS(X2)*COS(X1),kind=4)   !explicitly make these c/w c_float
         vert2 = real(ABS(X2)*SIN(X1),kind=4)
         vert3 = real(X3,kind=4)  
         if (ieee_is_finite(vert3) .and. ieee_is_finite(vert2) .and. &
             ieee_is_finite(vert1) .and. ieee_is_finite(pow) .and. ieee_is_finite(nrm3)) then
          c_rgbv=colormap(pow,powmin,powmax,map)/255.0  !openGL wants scale of 1.0 not 255
         else
          vert1 = 0        ! for out of bound values
          vert2 = 0        ! for out of bound values
          vert3 = 0        ! for out of bound values
          nrm1 = 0         ! for out of bound values
          nrm2 = 0         ! for out of bound values
          nrm3 = 1         ! for out of bound values
          c_rgbv = (/1,1,1/)
         endif
         c_norm=real((/nrm1,nrm2,nrm3/),kind=4)  ! explicitly cast to kind=4 for consistent with c_float
         c_vert=real((/vert1,vert2,vert3/),kind=4)  ! explicitly cast to kind=4 for consistent with c_float
         vertices(k:k+8)=(/c_vert,c_norm,c_rgbv/)
         k=k+9      ! matrix index
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

       end subroutine Geom
