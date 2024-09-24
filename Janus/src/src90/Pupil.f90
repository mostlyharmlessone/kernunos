!      Generates matrices for openGL, pupil version

       subroutine Pupil(b, dist, pupil_elements, pupil_vertices, pupil_nV, pupil_nE)
       use cornea_arrays, ONLY : wpJMatrix
       use set_precision, ONLY : wp
       use, intrinsic :: iso_c_binding, ONLY : c_float,c_int
       use, intrinsic ::  ieee_arithmetic
       use ISO_FORTRAN_ENV, only: stdin=>input_unit     ! for the pause read(stdin,*)
       TYPE(wpJMatrix),INTENT(IN) :: b
       integer(c_int), INTENT(INOUT) :: pupil_elements(*)                          ! faces x 3
       real(c_float), INTENT(INOUT) :: pupil_vertices(*), dist                     ! vertices x 6
       integer(c_int), INTENT(INOUT) :: pupil_nE, pupil_nV
       real(wp) :: X1,X2,X3
       real(wp) :: vert1,vert2,vert3
       real(c_float) :: c_vert(3),c_rgbv(3),c_norm(3)
       integer :: i,j,k,M1,verts
       integer(c_int) :: ivert1,ivert2,ivert3


       M1=size(b%r,2)

       c_rgbv = (/0,0,0/)
       c_norm = (/0,0,1/)

!      vertices  
       k=1        
       vert1 = real(b%Pupil_Center(1),kind=4)   !explicitly make these c/w c_float
       vert2 = real(b%Pupil_Center(2),kind=4)
       vert3 = real(dist,kind=4)
       c_vert=real((/vert1,vert2,vert3/),kind=4)  ! explicitly cast to kind=4 for consistent with c_float
       pupil_vertices(k:k+8)=(/c_vert,c_norm,c_rgbv/)
       k=k+9      ! matrix index
       do i=1,M1
         X1=b%THT(i)         ! in radians
         X2=b%PU(i)          ! pupil radius
         X3=dist             ! pupil position in Z        
         vert1 = real(ABS(X2)*COS(X1),kind=4)   !explicitly make these c/w c_float
         vert2 = real(ABS(X2)*SIN(X1),kind=4)
         vert3 = real(X3,kind=4)
         c_vert=real((/vert1,vert2,vert3/),kind=4)  ! explicitly cast to kind=4 for consistent with c_float
         pupil_vertices(k:k+8)=(/c_vert,c_norm,c_rgbv/)
         k=k+9      ! matrix index
       end do
       pupil_nV=k-1
       
!      elements
       k=1 
         do i=1,M1-1 ! j=1 and the origin
          ivert2=(i-1)+1
          ivert3=i+1
          ivert1=0  ! verts from above zero indexing, origin given last vertex number
!         no boundary check on inner
          pupil_elements(k:k+2)=(/ivert1,ivert2,ivert3/)
          k=k+3     ! matrix index
         end do
!        Last face is different
         ivert2=(M1-1)+1
         ivert3=1
         ivert1=0   ! verts from above zero indexing, origin given last vertex number
!        no boundary check on inner
         pupil_elements(k:k+2)=(/ivert1,ivert2,ivert3/)
         k=k+3     ! matrix index     
      
        pupil_nE=k-1

       end subroutine Pupil
