    MODULE sparseAssign
!  Modified to add CSR sparse matrices
      USE set_precision, ONLY : dkind
      USE sparseTypes, ONLY: dpTriplet, dpTripletList, dpHBSparseMatrix, &
          dpCSRSparseMatrix, slu_dpHBSparseMatrix, setExpansionFactor, getExpansionFactor
      IMPLICIT NONE

!...
! Define procedure names for assignment 
      INTERFACE ASSIGNMENT (=)
! This defines the basic operations for building a list of
! triplets from individual triplets and for clearing a 
! dpTripletList variable by reclaiming all the allocated space.
 !TYPE(dpTripletList) = TYPE(dpTriplet)
        MODULE PROCEDURE a_triplet
 !TYPE(dpTripletList) = TYPE(dpTriplet)(:)
        MODULE PROCEDURE list_of_triplets
!TYPE(dpTriplet) = TYPE(dpTripletList)(:)
        MODULE PROCEDURE list_of_triplets_eq_triplets
 !TYPE(dpTripletList) =  INTEGER (0)
        MODULE PROCEDURE clear_triplets
      END INTERFACE

      INTERFACE ASSIGNMENT (=)
! Define procedure names for assignment to TYPE(dpHBSparseMatrix)
! Convert a dpTripletList to HB sparse matrix form
! TYPE(dpHBSparseMatrix) = TYPE(dpTripletList)
        MODULE PROCEDURE dhbc_eq_list_of_triplets
! Convert a CSR sparse to a HB sparse
! TYPE(dpHBSparseMatrix) = TYPE(dpCSRSparseMatrix)
        MODULE PROCEDURE dhbc_eq_dcsr
! Clear an HB sparse matrix and reclaim all allocated space
! TYPE(dpHBSparseMatrix) = INTEGER (0)
        MODULE PROCEDURE clear_dhbc
      END INTERFACE

      INTERFACE ASSIGNMENT (=)
! Define procedure name for assignment to TYPE(dpTriplet)(:)
! Convert a sparse matrix in HB format to a list of dpTriplest
! TYPE(dpTriplet)(:) = TYPE(dpHBSparseMatrix)
       MODULE PROCEDURE list_of_triplets_eq_dhbc
! Convert a sparse matrix in CSR format to a list of dpTriplest
! TYPE(dpTriplet)(:) = TYPE(dpCSRSparseMatrix)
       MODULE PROCEDURE list_of_triplets_eq_dcsr
     END INTERFACE

     INTERFACE ASSIGNMENT (=)
! Define procedure names for assignment to TYPE(dpCSRSparseMatrix)
! Convert a dpTripletList to CSR sparse matrix format
! TYPE(dpCSRSparseMatrix) = TYPE(dpTripletList)
      MODULE PROCEDURE dcsr_eq_list_of_triplets
! Clear an CSR sparse matrix and reclaim all allocated space
! TYPE(dpCSRSparseMatrix) = INTEGER (0)
      MODULE PROCEDURE clear_dcsr
     END INTERFACE

     INTERFACE ASSIGNMENT (=)
! Convert a sparse matrix in CSR format to a dense matrix
! REAL(:,:)=TYPE(dpCSRSparseMatrix)
      MODULE PROCEDURE matrix_eq_dcsr
! Convert a dense matrix to a sparse matrix in CSR format
! TYPE(dpCSRSparseMatrix)=REAL(:,:)
      MODULE PROCEDURE dcsr_eq_matrix

     END INTERFACE

    INTERFACE
     SUBROUTINE csrcoo ( nrow, job, a, ja, ia, nnz, ao, ir, jc, ierr )
     INTEGER ( kind = 4 ) nrow
     INTEGER ( kind = 4 ) nnz
     REAL ( kind = 8 ) a(*)
     REAL ( kind = 8 ) ao(*)
     INTEGER ( kind = 4 ) ia(*)
     INTEGER ( kind = 4 ) ierr
     INTEGER ( kind = 4 ) ir(*)
     INTEGER ( kind = 4 ) ja(*)
     INTEGER ( kind = 4 ) jc(*)
     INTEGER ( kind = 4 ) job
     END SUBROUTINE
    END INTERFACE

      REAL (dkind), PRIVATE :: zero = 0.0e0_dkind
    CONTAINS

      SUBROUTINE a_triplet(sparse,triplet)
        IMPLICIT NONE
        TYPE (dpTripletList), INTENT (INOUT) :: sparse
        TYPE (dpTriplet), INTENT (IN) :: triplet
! Make a single triplet an equivalent array of size 1.
! Use the array based assignment routine for the logic.       
        CALL list_of_triplets(sparse,(/triplet/))
      END SUBROUTINE a_triplet

      SUBROUTINE clear_dhbc(dhbc,iflag)
! The overloaded assignment dhbc = 0 clears
! the contents of dhbc.   
        IMPLICIT NONE
        TYPE (dpHBSparseMatrix), INTENT (INOUT) :: dhbc
        INTEGER, INTENT (IN) :: iflag
! Note that only IFLAG=0 clears the list.       
! Deallocate all space -- we don't bother to check if this
! fails as there is nothing very sensible we can do except
! abort which may well not be what the user wants
        IF (iflag==0) THEN
          IF (ALLOCATED(dhbc%colStartIndices)) &
             DEALLOCATE (dhbc%rowIndices,dhbc%colStartIndices,dhbc%values)
          dhbc%noOfRows = 0
          dhbc%noOfColumns = 0
          dhbc%errFlag = 0
        END IF
      END SUBROUTINE clear_dhbc

  SUBROUTINE clear_dcsr(dcsr,iflag)
! The overloaded assignment dcsr = 0 clears
! the contents of dcsr.
  IMPLICIT NONE
  TYPE (dpCSRSparseMatrix), INTENT (INOUT) :: dcsr
  INTEGER, INTENT (IN) :: iflag
! Note that only IFLAG=0 clears the list.
! Deallocate all space -- we don't bother to check if this
! fails as there is nothing very sensible we can do except
! abort which may well not be what the user wants
  IF (iflag==0) THEN
    IF (ALLOCATED(dcsr%ia)) &
       DEALLOCATE (dcsr%a,dcsr%ia,dcsr%ja)
    dcsr%noOfRows = 0
    dcsr%noOfColumns = 0
    dcsr%nnz = 0
  END IF
 END SUBROUTINE clear_dcsr

      SUBROUTINE clear_triplets(sparse,iflag)
! The overloaded assignment TYPE(dpTripletList) = 0 clears the contents
! of sparse, reclaims allocated storage and sets lastTriplet=0.   
        IMPLICIT NONE
        TYPE (dpTripletList), INTENT (INOUT) :: sparse
        INTEGER, INTENT (IN) :: iflag
! Note that only iflag=0 clears the list.       
        IF (iflag==0) THEN
          IF (ALLOCATED(sparse%values)) &
            DEALLOCATE (sparse%rows,sparse%columns,sparse%values)
          sparse%lastTriplet = 0
          sparse%errFlag = 0
        END IF
      END SUBROUTINE clear_triplets

      SUBROUTINE dhbc_eq_list_of_triplets(dhbc,sparse)
      USE sparseSort, ONLY : sparseData
      USE sortElements, ONLY : qsort
! This routine handles the overloaded assignment
! TYPE(dpHBSparseMatrix)=TYPE(dpTripletList)
! It builds an MROWS by NCOLS sparse matrix using
! the Harwell-Boeing format.  Triplets in the list
! TYPES(dpTripletList) are accumulated (summed) if there are
! repeated entries of row indices. 
        IMPLICIT NONE
        TYPE (dpHBSparseMatrix), INTENT (INOUT) :: dhbc
        TYPE (dpTripletList), INTENT (IN), TARGET :: sparse
        TYPE(sparseData) :: spData
! Local working variables:
        INTEGER :: i, ioerr, j, last, ncols, mrows
        INTEGER :: ii, mc, nz
        LOGICAL :: accumulate
        INTEGER, ALLOCATABLE, TARGET :: ind(:), itemp(:)
        INTEGER, ALLOCATABLE :: ip(:)
        REAL (dkind), ALLOCATABLE :: column(:), values(:)
        last = sparse%lastTriplet
! Take care of the empty case, LAST == 0.
! This case implies that the sparse matrix is 0.
        IF (last==0) THEN
          dhbc%noOfColumns = 0
          dhbc%noOfRows = 0
          IF ( .NOT. ALLOCATED(dhbc%rowIndices)) THEN
            ALLOCATE (dhbc%colStartIndices(1), STAT=ioerr)
            IF (ioerr/=0) THEN
              dhbc%errFlag = ioerr
              RETURN
            ENDIF
          END IF
! Initialize start of column indices
          dhbc%colStartIndices(1) = 1
          RETURN
        END IF
! Get working space to process and accumulate set of triplets.       
        ALLOCATE (ind(last),itemp(last),values(last),STAT=ioerr)
        IF (ioerr/=0) THEN
          dhbc%errFlag = ioerr
          RETURN
        END IF
! Sort the column indices.  Then extract the size of the matrix.
! The max column index=NCOLS.  The max row index=MROWS.
! Zero values are ignored for purposes of determining the size.       
        ind(1:last) = [(i, i=1,last)]
        spData%left = 1
        spData%right = last
        spData%colPtr => sparse%columns(1:last)
        spData%indexPtr => ind(1:last)
        CALL qsort(spData)
! The matrix dimensions are determined by the largest
! values of row and columns indices that appear in
! the right-hand side or input list.
! Rearrange the row subscripts and the values.       
        values = sparse%values(ind)
        itemp = sparse%rows(ind)
        ncols = max(0,sparse%columns(ind(last)))
        mrows = max(0,maxval(itemp))
        dhbc%noOfColumns = ncols
        dhbc%noOfRows = mrows
! This is the pointer list to starts and end (less one) of the 
! separate columns in the matrix. Initialize to zero.
        ALLOCATE (ip(ncols+1), STAT=ioerr)
        IF (ioerr/=0) THEN
          dhbc%errFlag = ioerr
          RETURN
        END IF
        ip = 0
! Make a preliminary run through the matrix and separate
! the columns.     
        DO j = 1, last
! Count the number of elements in each column.       
          ip(sparse%columns(ind(j))+1) = ip(sparse%columns(ind(j))+1) + 1
        END DO
! Process the data in each column of the matrix.
! Within each column sort the row indices.  If there
! are repeats then expand the sums into a full column
! of size DHBC %noOfRows.  Contract the column and record
! the non-zero values.
        ii = 0
        nz = 0
        DO j = 1, ncols
! Get the number of elements in this column.  There
! may be repeats. A scan is done to see and if there
! are repeats and then an expand/sum/contract step
! is made for this column.
          mc = ip(j+1)
          IF (mc==0) CYCLE
! Sort the row indices within a column.
          ind(1:mc) = [(i, i=1,mc)]
          spData%left =  1
          spData%right =  mc
          spData%colPtr => itemp(ii+1:ii+mc)
          spData%indexPtr => ind(1:mc)
          CALL qsort(spData)
! Move the row indices so they are sorted.                    
          itemp(ii+1:ii+mc) = itemp(ii+ind(1:mc))
! Move the corresponding values for those rows.          
          values(ii+1:ii+mc) = values(ii+ind(1:mc))
          accumulate = .FALSE.
! See if there are any repeats of row indices.  If there are
! then add the associated values and replace the repeated 
! indices by a single value.         
SCAN:     DO i = 1, mc - 1
! This assigns the value .TRUE. the first time
! a row index is repeated.          
            accumulate = (itemp(ii+i)==itemp(ii+i+1))
            IF (accumulate) EXIT SCAN
          END DO SCAN
! If there are repeats then get working space for 
! expand/sum/contract buffer.  Then accumulate.
          IF (accumulate) THEN
            IF ( .NOT. ALLOCATED(column)) THEN
              ALLOCATE (column(mrows), STAT=ioerr)
              IF (ioerr/=0) THEN
                dhbc%errFlag = ioerr
                RETURN
              END IF
            END IF
! Clear out the expanded column and accumulate repeated values.
            IF (mrows>0) column = zero
            DO i = 1, mc
              column(itemp(ii+i)) = column(itemp(ii+i)) + values(ii+i)
            END DO
! Compress the column and move its final values.  This step
! changes the value of MC (number of entries) for this column.
            mc = 0
            DO i = 1, mrows
              IF (column(i)/=zero) THEN
                nz = nz + 1
! Save the row index and its accumulated value.                 
                itemp(nz) = i
                values(nz) = column(i)
                mc = mc + 1
              END IF
            END DO
          ELSE 
            IF (mc>0) THEN
              itemp(nz+1:nz+mc) = itemp(ii+1:ii+mc)
              values(nz+1:nz+mc) = values(ii+1:ii+mc)
              nz = nz + mc
            END IF
          END IF 
! This is the new number of non-zero values in this column.
          ii = ii + ip(j+1)
          ip(j+1) = mc
        END DO
! Define the pointers for the columns of the Harwell-
! Boeing format.  
        DO j = 1, ncols
          ip(j+1) = ip(j) + ip(j+1)
          ip(j) = ip(j) + 1
        END DO
        ip(ncols+1) = ip(ncols+1) + 1
! Move the local allocated arrays into place so they 
! become the components of the derived type.  Because of 
! accumulation the sizes of components for row indices and
! values may be longer than required.  But this step avoids
! creating new allocated temporary arrays that require
! additional space.
        CALL move_alloc(from=itemp,to=dhbc%rowIndices)
        CALL move_alloc(from=ip,to=dhbc%colStartIndices)
        CALL move_alloc(from=values,to=dhbc%values)
! Tidy mind !
        DEALLOCATE(ind)
        IF (accumulate) THEN
          DEALLOCATE(column)
        END IF
      END SUBROUTINE dhbc_eq_list_of_triplets

      SUBROUTINE list_of_triplets(sparse,triplets)
! Accumulate a list of triplets.  If space runs out, new
! arrays are allocated that (attempt to) hold all the data. 
! Non-positive subscripts are ignored.  
! This routine handles the overloaded assignment
! TYPE(dpTripletList)(:) = TYPE(dptriplets);
        IMPLICIT NONE
        TYPE (dpTripletList), INTENT (INOUT) :: sparse
        TYPE (dpTriplet), INTENT (IN) :: triplets(:)
        INTEGER, ALLOCATABLE :: tempInt(:)
        REAL (dkind), ALLOCATABLE :: tempReal(:)
        INTEGER :: istat, j, k, last, m

        REAL(dkind) :: expFactor

        expFactor = getExpansionFactor(sparse)

! ALlocation error flag
        istat = 0
! This dummy loop is a container for the logic of the 
! list building.   
BLOCK:  DO
! If the contents of dpTripletList are not allocated
! then allocate it with size = max(K,expansionFactor * size(triplets))
          k = size(triplets)
          last = sparse%lastTriplet
! If triplets set is empty, return immediately.
          IF (k<=0) EXIT BLOCK
          IF ( .NOT. ALLOCATED(sparse%values)) THEN
! We are forcing expansion Factor to be > one
            m = int(k*expFactor)
! Allocate enough space, plus a bit more, for the
! first set of triplets.          
            ALLOCATE (sparse%rows(m),sparse%columns(m),sparse%values(m), &
              STAT=istat)
! Exit if allocate did not suceed
            IF (istat/=0) EXIT BLOCK
! Join the triplets to the end of the list.               
! Note the number of triplets. 
            last = 0
            sparse%lastTriplet = k
            sparse%rows(1:k) = triplets(1:k)%rowIndex
            sparse%columns(1:k) = triplets(1:k)%columnIndex
            sparse%values(1:k) = triplets(1:k)%value
            EXIT BLOCK
          ELSE
            j = size(sparse%values)
            IF (k+last>j) THEN
! Space to hold the incoming set of triplets is
! too small.  Expand the amount available.
              m = max(k+last,int(j*expFactor))
! Allocate space separately to minimize temporary memory use
              ALLOCATE (tempInt(m),STAT=istat)
! Check if allocation did not succeed. 
              IF (istat/=0) EXIT BLOCK
! Transfer the current list of triplets to the newly
! allocated space. Row indices first
              tempInt(1:last) = sparse%rows(1:last)
              CALL move_alloc(from=tempInt,to=sparse%rows)
! Then columns
              ALLOCATE (tempInt(m),STAT=istat)
              IF (istat/=0) EXIT BLOCK
              tempInt(1:last) = sparse%columns(1:last)
              CALL move_alloc(from=tempInt,to=sparse%columns)

! Finally the values
              ALLOCATE (tempReal(m),STAT=istat)
              IF (istat/=0) EXIT BLOCK
              tempReal(1:last) = sparse%values(1:last)
              CALL move_alloc(from=tempReal,to=sparse%values)
            END IF
          END IF
! If we arrive here we need to copy in the new triplet data
          sparse%lastTriplet = last + k
          sparse%rows(last+1:last+k) = triplets(1:k)%rowIndex
          sparse%columns(last+1:last+k) = triplets(1:k)%columnIndex
          sparse%values(last+1:last+k) = triplets(1:k)%value
          EXIT BLOCK
        END DO BLOCK

! This flag is updated to alert if there was any
! memory allocation problem.        
        sparse%errFlag = ior(sparse%errFlag,istat)
      END SUBROUTINE list_of_triplets

      SUBROUTINE list_of_triplets_eq_dhbc(triplets,dhbc)
! This routine handles the overloaded assignment
! TYPE(dpTriplet)(:) = TYPE(dpHBSparseMatrix)

! It assigns an array of triplets using the contents
! of a Harwell-Boeing format sparse matrix.  Zero values
! are not returned. 
        IMPLICIT NONE
        TYPE (dpTriplet), INTENT (INOUT), ALLOCATABLE :: triplets(:)
        TYPE (dpHBSparseMatrix), INTENT (IN) :: dhbc
        INTEGER :: i, icount, istat, j, k, l, m, n
! Get matrix size.
        n = dhbc%noOfColumns
! Allocate just enough space to hold the entries
! of the Harwell-Boeing format sparse matrix.       
        k = max(0,dhbc%colStartIndices(n+1)-1)
        ALLOCATE(triplets(k), STAT=istat)
        IF(istat /= 0) THEN
          WRITE(*,*) 'Allocation failure in assignment triplets(:)=dhbc_sparse.'
          RETURN
        END IF
        l = 0
        icount = 0
        DO j = 1, n
! Get the number of entries in column J of DHBC.
          m = dhbc%colStartIndices(j+1) - dhbc%colStartIndices(j)
          DO i = 1, m
! Copy row index, column index and value to make a triplet.          
            l = l + 1
            icount = icount + 1
            triplets(l) = dpTriplet(dhbc%rowIndices(icount),j,dhbc%values(icount))
          END DO
        END DO

      END SUBROUTINE list_of_triplets_eq_dhbc

SUBROUTINE list_of_triplets_eq_triplets(triplets,sparse)
! This routine handles the overloaded assignment
! TYPE(dpTriplet)(:) = TYPE(dptripletList; opposite of SUBROUTINE list_of_triplets(sparse,triplets)
 IMPLICIT NONE
 TYPE (dpTriplet), INTENT (INOUT) :: triplets(:)
 TYPE (dpTripletList), INTENT (IN) :: sparse
 INTEGER :: nnz,i
 nnz=sparse%lastTriplet
 do i=1,nnz
  triplets(i)=dpTriplet(sparse%rows(i),sparse%columns(i),sparse%values(i))
 end do
END SUBROUTINE list_of_triplets_eq_triplets

SUBROUTINE list_of_triplets_eq_dcsr(trip,dcsr)
USE sparsekit, ONLY: csrcoo
use sparseUtils
! This routine handles the overloaded assignment
! TYPE(dpTriplet)(:) = TYPE(dpCSRSparseMatrix)
! by providing a modern interface/replacement to sparsekit.f90 CSRCOO
! It assigns an array of triplets using the contents
! of a Compressed Sparse Row format sparse matrix.  Zero values
! are not returned.
  IMPLICIT NONE
  TYPE (dpTriplet), INTENT (OUT), ALLOCATABLE :: trip(:)
  TYPE (dpCSRSparseMatrix), INTENT (IN) :: dcsr
  INTEGER :: nrow, nnz, ierr, k, k1, k2, i
! Get matrix size.
  nrow = dcsr%noOfRows ; ierr = 0
! Allocate just enough space to hold the entries
! of the Compressed Sparse Row format sparse matrix.
  ALLOCATE(trip(dcsr%nnz), STAT=ierr)
  IF(ierr /= 0) THEN
    WRITE(*,*) 'Allocation failure in assignment triplets(:)=dcsr_sparse.'
    RETURN
  END IF
! uses sparsekit.f90 but makes array temps
!  call csrcoo( nrow, 3, dcsr%a, dcsr%ja, dcsr%ia, dcsr%nnz, triplets%value, triplets%rowIndex, triplets%columnIndex, ierr )
! copied from csrcoo
  nnz = dcsr%nnz
  if ( (nrow+1) .gt. size(dcsr%ia)) then
   write(*,*) 'FATAL Error on input list_of_triplets_eq_dcsr/csrcoo: unexpected size',(nrow+1),size(dcsr%ia)
   stop
  endif
  if ((dcsr%ia(nrow+1)-1) .ne. nnz) then
   write(*,*) 'FATAL Error on input list_of_triplets_eq_dcsr/csrcoo: unexpected nnz',dcsr%ia(nrow+1)-1,nnz
   stop
  endif
  trip(1:nnz)%value = dcsr%a(1:nnz)
  trip(1:nnz)%columnIndex = dcsr%ja(1:nnz)
!  Copy backward.
  do i = nrow, 1, -1
   k1 = dcsr%ia(i+1) - 1
   k2 = dcsr%ia(i)
   do k = k1, k2, -1
    trip(k)%rowIndex = i
   end do
 end do
 do i=1, dcsr%nnz
  if (trip(i)%rowIndex .lt. 1 .or. trip(i)%rowIndex .gt. dcsr%nnz) then
   write(*,*) 'FATAL Error on output list_of_triplets_eq_dcsr/csrcoo: bad rowIndex'
   stop
  endif
 end do
END SUBROUTINE list_of_triplets_eq_dcsr


SUBROUTINE dcsr_eq_list_of_triplets(dcsr,sparse)
USE sparsekit, ONLY: coocsr
! This routine handles the overloaded assignment
! TYPE(dpCSRSparseMatrix)=TYPE(dpTripletList)
! by providing an interface to sparsekit.f90 COOCSR
! It builds a Compressed Sparse Row matrix from a list of triplets.
  IMPLICIT NONE
  TYPE (dpCSRSparseMatrix), INTENT (INOUT) :: dcsr
  TYPE (dpTripletList), INTENT (IN), TARGET :: sparse
  INTEGER :: istat, ioerr, i
  INTEGER, ALLOCATABLE :: ir(:)
  INTEGER, ALLOCATABLE, TARGET :: ind(:),itemp(:)
! need to find nrow first for list of triplets
  dcsr%nnz = sparse%lastTriplet
! Take care of the empty case, LAST == 0.
! This case implies that the sparse matrix is 0.
  IF (dcsr%nnz==0) THEN
   dcsr%noOfRows = 0
   IF ( .NOT. ALLOCATED(dcsr%ia)) THEN
    ALLOCATE (dcsr%ia(1), STAT=ioerr)
    IF (ioerr/=0) THEN
      dcsr%errFlag = ioerr
      RETURN
    ENDIF
   END IF
! Initialize start of column indices
   dcsr%ja(1) = 1
   RETURN
  END IF
  ALLOCATE (ind(dcsr%nnz),itemp(dcsr%nnz),STAT=ioerr)
  IF (ioerr/=0) THEN
   dcsr%errFlag = ioerr
   RETURN
  END IF
! The max row index => dcsr%noOfRows.
  ind(1:dcsr%nnz) = [(i, i=1,dcsr%nnz)]
  itemp = sparse%rows(ind)
  dcsr%noOfRows = max(0,maxval(itemp))
! The max column index => dcsr%noOfColumns.
  itemp = sparse%columns(ind)
  dcsr%noOfColumns = max(0,maxval(itemp))
  ! Allocate just enough space to hold the entries of the CSR matrix
  ALLOCATE (ir(dcsr%nnz),dcsr%ia(dcsr%nnz),dcsr%a(dcsr%nnz), dcsr%ja(dcsr%nnz),STAT=istat)
  IF(istat /= 0) THEN
    WRITE(*,*) 'Allocation failure in assignment dcsr_sparse(:)=triplets.'
    RETURN
  END IF
! coocsr destroys ir, make working copy of triplets%rows
  ir(1:dcsr%nnz)=sparse%rows(1:dcsr%nnz)
  call coocsr( dcsr%noOfRows, dcsr%nnz, sparse%values(1:dcsr%nnz), ir(1:dcsr%nnz), sparse%columns(1:dcsr%nnz), dcsr%a, dcsr%ja, dcsr%ia )
  DEALLOCATE(ind,itemp,ir)

END SUBROUTINE dcsr_eq_list_of_triplets

SUBROUTINE dhbc_eq_dcsr(dhbc,dcsr)
! This routine handles the overloaded assignment
! TYPE(dpHBSparseMatrix) = TYPE(dpCSRSparseMatrix)
! using the above overloaded assignments
  IMPLICIT NONE
  TYPE (dpHBSparseMatrix), INTENT (INOUT) :: dhbc
  TYPE (dpCSRSparseMatrix), INTENT (IN) :: dcsr
  TYPE (dpTriplet), ALLOCATABLE :: triplets(:)
  TYPE (dpTripletList) :: s
  triplets = dcsr
  s = triplets
  dhbc = s
END SUBROUTINE dhbc_eq_dcsr

SUBROUTINE matrix_eq_dcsr(matrix,dcsr)
USE sparsekit, ONLY: csrdns
! This routine handles the overloaded assignment
! REAL(:,:)=TYPE(dpCSRSparseMatrix)
! by providing an interface to sparsekit.f90 CSRDNS (modified)
  IMPLICIT NONE
  REAL (dkind), INTENT(INOUT), ALLOCATABLE :: matrix(:,:)
  TYPE (dpCSRSparseMatrix), INTENT (IN) :: dcsr
  INTEGER :: ierr
  ALLOCATE (matrix(dcsr%noOfRows,dcsr%noOfColumns))
  call csrdns ( dcsr%noOfRows, dcsr%noOfColumns, dcsr%a, dcsr%ja, dcsr%ia, matrix, ierr )
  IF (ierr/=0) THEN
   write(*,*) 'Error in csrdns'
   RETURN
  ENDIF
END SUBROUTINE matrix_eq_dcsr

SUBROUTINE dcsr_eq_matrix(dcsr,matrix)
USE sparsekit, ONLY: dnscsr
! This routine handles the overloaded assignment
! TYPE(dpCSRSparseMatrix)=REAL(:,:)
! by providing an interface to sparsekit.f90 CSRDNS (modified)
  IMPLICIT NONE
  TYPE (dpCSRSparseMatrix), INTENT (INOUT) :: dcsr
  REAL (dkind), INTENT(IN) :: matrix(:,:)
  INTEGER :: i, j
  dcsr%nnz = 0
  do i=1,size(matrix,1)
   do j=1,size(matrix,2)
    if (abs(matrix(j,i)) > 0) dcsr%nnz=dcsr%nnz+1
   end do
  end do
  ALLOCATE(dcsr%a(1:dcsr%nnz), dcsr%ja(1:dcsr%nnz), dcsr%ia(1:dcsr%nnz))
  dcsr%noOfRows=size(matrix,1)
  dcsr%noOfColumns=size(matrix,2)
  call dnscsr ( size(matrix,1), size(matrix,2), dcsr%nnz, matrix, size(matrix,1), dcsr%a(1:dcsr%nnz), dcsr%ja(1:dcsr%nnz), dcsr%ia(1:dcsr%nnz), dcsr%errFlag )
  IF (dcsr%errFlag /= 0) THEN
   write(*,*) 'Error in dnscsr'
   RETURN
  ENDIF
END SUBROUTINE dcsr_eq_matrix

    END MODULE sparseAssign
