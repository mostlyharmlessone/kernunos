    MODULE sparseStructure

      USE set_precision, ONLY : dkind
      IMPLICIT NONE

! Define double precision triplets with three fields; the row
! and column index and value of each non-zero element.
      TYPE dpTriplet
        INTEGER :: rowIndex, columnIndex
        REAL (dkind) :: value
      END TYPE dpTriplet

      TYPE dpTripletList
! Define an expansion factor to allow extra free space to 
! be allocated for efficiency reasons. The mutator function
! setExpansionFactor needs to be used to change this value
! to ensure that it is always at least one.
        REAL (dkind), PRIVATE :: expansionFactor = 1.2E0_dkind
! lastTriplet is the array index to the current end of data
        INTEGER :: lastTriplet = 0
! Status of the list; an allocation error has occurred if
! errFlag has been set non-zero
        INTEGER :: errFlag = 0
! Arrays to store triplet data
        INTEGER, ALLOCATABLE :: rows(:), columns(:)
        REAL (dkind), ALLOCATABLE :: values(:)
      END TYPE dpTripletList

! The Harwell-Boeing column-oriented sparse format:
! It is defined using overloaded assignment:
! TYPE(dpHBSparseMatrix) = TYPE(dpTripletList).  This will be used
! after the triplets list has been accumulated in a
! variable of TYPE(dpTripletList).

      TYPE dpHBSparseMatrix
! The number of rows and columns in the matrix.
! These values are defined, in the overloaded 
! assignment, as the maximum indices noted for 
! any triplet.
        INTEGER :: noOfRows = 0
        INTEGER :: noOfColumns = 0
! This flag is non-zero if there is any allocation error
! during creation of a Harwell-Boeing matrix with overloaded
! assignment.    
        INTEGER :: errFlag = 0

! Row indices of non-zero values.
        INTEGER, ALLOCATABLE :: rowIndices(:)
! Pointers for the start of non-zeros in a column.  
        INTEGER, ALLOCATABLE :: colStartIndices(:)
! Values of the non-zero matrix elements. Note that
! accumulation may occur during overloaded assignment.
        REAL (dkind), ALLOCATABLE :: values(:)
      END TYPE dpHBSparseMatrix

!...
! Define procedure names for assignment 
      INTERFACE ASSIGNMENT (=)
! TYPE(dpTripletList)= TYPE(dpTriplet) or 
! TYPE(dpTripletList)= TYPE(dpTriplet)(:), i.e. a set of triplets.
! TYPE(dpTripletList)=  0 (integer, 0) clears the list

! This is a basic operation of building a sparse list, 
! given coordinates (or elements of sums) of a matrix.
        MODULE PROCEDURE a_triplet, list_of_triplets, clear_triplets

! Define procedure names for assignment
! TYPE(dpHBSparseMatrix) = TYPE(dpTripletList)
        MODULE PROCEDURE dhbc_eq_list_of_triplets, clear_dhbc

! Define procedure name for assignment
! TYPE(dpTriplet)(:) = TYPE(dpHBSparseMatrix)
        MODULE PROCEDURE list_of_triplets_eq_dhbc
      END INTERFACE

      REAL (dkind) :: zero = 0._dkind
    CONTAINS

      SUBROUTINE setExpansionFactor(tList, newFactor)
! Mutator routine to change the expansion factor associated with
! a tripletList. This is used in the building of a list of triples
! from individual sets of triplets
!
! The factor will not be assigned a value less than or equal to one
      TYPE(dpTripletList), INTENT (INOUT) :: tList
      REAL(dkind), INTENT(IN) :: newFactor
      REAL(dkind), PARAMETER :: one = 1.0E0_dkind

      IF (newFactor > one) THEN
        tList%expansionFactor = newFactor
      END IF
      
      END SUBROUTINE setExpansionFactor

      FUNCTION getExpansionFactor(tList) RESULT(factor)
! Mutator routine to change the expansion factor associated with
! a tripletList. This is used in the building of a list of triples
! from individual sets of triplets
!
! The factor will not be assigned a value less than or equal to one
      TYPE(dpTripletList), INTENT (INOUT) :: tList
      REAL(dkind) :: factor

      factor = tList%expansionFactor
      
      END FUNCTION getExpansionFactor

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
        IF (iflag==0) THEN
          IF (allocated(dhbc%colStartIndices)) &
             DEALLOCATE (dhbc%rowIndices,dhbc%colStartIndices,dhbc%values)
          dhbc%noOfRows = 0
          dhbc%noOfColumns = 0
          dhbc%errFlag = 0
        END IF
      END SUBROUTINE clear_dhbc

      SUBROUTINE clear_triplets(sparse,iflag)
! The overloaded assignment TYPE(SPARSE) = 0 clears
! the contents of SPARSE and sets lastTriplet=0.   
        IMPLICIT NONE
        TYPE (dpTripletList), INTENT (INOUT) :: sparse
        INTEGER, INTENT (IN) :: iflag
! Note that only IFLAG=0 clears the list.       
        IF (iflag==0) THEN
          IF (allocated(sparse%values)) DEALLOCATE (sparse%rows,sparse%columns, &
            sparse%values)
          sparse%lastTriplet = 0
          sparse%errFlag = 0
        END IF
      END SUBROUTINE clear_triplets

      SUBROUTINE dhbc_eq_list_of_triplets(dhbc,sparse)
! This routine handles the overloaded assignment
! TYPE(dpHBSparseMatrix)=TYPE(dpTripletList)

! It builds an MROWS by NCOLS sparse matrix using
! the Harwell-Boeing format.  Triplets in the list
! TYPES(dpTripletList) are accumulated (summed) if there are
! repeated entries of row indices. 
        IMPLICIT NONE
        TYPE (dpHBSparseMatrix), INTENT (INOUT) :: dhbc
        TYPE (dpTripletList), INTENT (IN) :: sparse

! Local working variables:
        INTEGER i, ioerr, j, last, ncols, mrows
        INTEGER ii, mc, nz
        LOGICAL accumulate

        INTEGER, ALLOCATABLE :: ind(:), itemp(:), ip(:)
        REAL (dkind), ALLOCATABLE :: column(:), values(:)

        last = sparse%lastTriplet
! Take care of the empty case, LAST == 0.
! This case implies that the sparse matrix is 0.
        IF (last==0) THEN
          dhbc%noOfColumns = 0
          dhbc%noOfRows = 0
          IF ( .NOT. allocated(dhbc%rowIndices)) &
               ALLOCATE (dhbc%colStartIndices(1))
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
        CALL qsortd(sparse%columns,ind,last)

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
! This is the pointer list to starts and end( less one) of the 
! separate columns in the matrix.       
        ALLOCATE (ip(ncols+1))
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
          CALL qsortd(itemp(ii+1:ii+mc),ind,mc)
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
            IF ( .NOT. allocated(column)) ALLOCATE (column(mrows))

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
          ELSE !IF(ACCUMULATE)
            IF (mc>0) THEN
              itemp(nz+1:nz+mc) = itemp(ii+1:ii+mc)
              values(nz+1:nz+mc) = values(ii+1:ii+mc)
              nz = nz + mc
            END IF
          END IF !IF(ACCUMULATE)

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

      CONTAINS
        SUBROUTINE qsortd(x,ind,n)
! Code converted using TO_F90 by Alan Miller
! Date: 2002-12-18  Time: 11:55:47

          IMPLICIT NONE
!INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(12, 60)

!REAL (dp), INTENT(IN)  :: x(:)
          INTEGER, INTENT (IN) :: x(:)
          INTEGER, INTENT (OUT) :: ind(:)
          INTEGER, INTENT (IN) :: n

!***************************************************************************

!                                                         ROBERT RENKA
!                                                 OAK RIDGE NATL. LAB.

!   THIS SUBROUTINE USES AN ORDER N*LOG(N) QUICK SORT TO SORT A REAL (dp)
! ARRAY X INTO INCREASING ORDER.  THE ALGORITHM IS AS FOLLOWS.  IND IS
! INITIALIZED TO THE ORDERED SEQUENCE OF INDICES 1,...,N, AND ALL INTERCHANGES
! ARE APPLIED TO IND.  X IS DIVIDED INTO TWO PORTIONS BY PICKING A CENTRAL
! ELEMENT T.  THE FIRST AND LAST ELEMENTS ARE COMPARED WITH T, AND
! INTERCHANGES ARE APPLIED AS NECESSARY SO THAT THE THREE VALUES ARE IN
! ASCENDING ORDER.  INTERCHANGES ARE THEN APPLIED SO THAT ALL ELEMENTS
! GREATER THAN T ARE IN THE UPPER PORTION OF THE ARRAY AND ALL ELEMENTS
! LESS THAN T ARE IN THE LOWER PORTION.  THE UPPER AND LOWER INDICES OF ONE
! OF THE PORTIONS ARE SAVED IN LOCAL ARRAYS, AND THE PROCESS IS REPEATED
! ITERATIVELY ON THE OTHER PORTION.  WHEN A PORTION IS COMPLETELY SORTED,
! THE PROCESS BEGINS AGAIN BY RETRIEVING THE INDICES BOUNDING ANOTHER
! UNSORTED PORTION.

! INPUT PARAMETERS -   N - LENGTH OF THE ARRAY X.

!                      X - VECTOR OF LENGTH N TO BE SORTED.

!                    IND - VECTOR OF LENGTH >= N.

! N AND X ARE NOT ALTERED BY THIS ROUTINE.

! OUTPUT PARAMETER - IND - SEQUENCE OF INDICES 1,...,N PERMUTED IN THE SAME
!                          FASHION AS X WOULD BE.  THUS, THE ORDERING ON
!                          X IS DEFINED BY Y(I) = X(IND(I)).

!*********************************************************************

! NOTE -- IU AND IL MUST BE DIMENSIONED >= LOG(N) WHERE LOG HAS BASE 2.

!*********************************************************************

          INTEGER :: iu(64), il(64)
          INTEGER :: m, i, j, k, l, ij, it, itt, indx
          REAL :: r
!REAL (DP) :: T
          INTEGER :: t, lselect

! LOCAL PARAMETERS -

! IU,IL =  TEMPORARY STORAGE FOR THE UPPER AND LOWER
!            INDICES OF PORTIONS OF THE ARRAY X
! M =      INDEX FOR IU AND IL
! I,J =    LOWER AND UPPER INDICES OF A PORTION OF X
! K,L =    INDICES IN THE RANGE I,...,J
! IJ =     RANDOMLY CHOSEN INDEX BETWEEN I AND J
! IT,ITT = TEMPORARY STORAGE FOR INTERCHANGES IN IND
! INDX =   TEMPORARY INDEX FOR X
! R =      PSEUDO RANDOM NUMBER FOR GENERATING IJ
! T =      CENTRAL ELEMENT OF X

          IF (n<=0) RETURN

! INITIALIZE IND, M, I, J, R and LSELECT
          lselect = 1
          DO i = 1, n
            ind(i) = i
          END DO
          m = 1
          i = 1
          j = n
          r = .375

! TOP OF LOOP
!    20 CONTINUE
LOOP_MAIN: DO
            SELECT CASE (lselect)
            CASE (1)
              IF (i>=j) THEN
                lselect = 70
                CYCLE LOOP_MAIN
              END IF
!    GO TO 70
              IF (r<=.5898437) THEN
                r = r + .0390625
              ELSE
                r = r - .21875
              END IF
              lselect = 30
              CYCLE LOOP_MAIN

! INITIALIZE K
!    30 CONTINUE
            CASE (30)
              k = i

! SELECT A CENTRAL ELEMENT OF X AND SAVE IT IN T

              ij = i + r*(j-i)
              it = ind(ij)
              t = x(it)

! IF THE FIRST ELEMENT OF THE ARRAY IS GREATER THAN T,
!   INTERCHANGE IT WITH T

              indx = ind(i)
              IF (x(indx)>t) THEN
                ind(ij) = indx
                ind(i) = it
                it = indx
                t = x(it)
              END IF

! INITIALIZE L

              l = j

! IF THE LAST ELEMENT OF THE ARRAY IS LESS THAN T,
!   INTERCHANGE IT WITH T

              indx = ind(j)
              IF (x(indx)>=t) THEN
                lselect = 50
                CYCLE LOOP_MAIN
              END IF
!    GO TO 50
              lselect = 3
              CYCLE LOOP_MAIN
            CASE (3)
              ind(ij) = indx
              ind(j) = it
              it = indx
              t = x(it)

! IF THE FIRST ELEMENT OF THE ARRAY IS GREATER THAN T,
!   INTERCHANGE IT WITH T

              indx = ind(i)
              IF (x(indx)<=t) THEN
                lselect = 50
                CYCLE LOOP_MAIN
              END IF
!GO TO 50
              lselect = 4
              CYCLE LOOP_MAIN

            CASE (4)
              ind(ij) = indx
              ind(i) = it
              it = indx
              t = x(it)
!    GO TO 50
              lselect = 50
              CYCLE LOOP_MAIN

! INTERCHANGE ELEMENTS K AND L
            CASE (40)
!    40 CONTINUE
              itt = ind(l)
              ind(l) = ind(k)
              ind(k) = itt
              lselect = 50
              CYCLE LOOP_MAIN

! FIND AN ELEMENT IN THE UPPER PART OF THE ARRAY WHICH IS
!   NOT LARGER THAN T

!    50 CONTINUE
            CASE (50)
              l = l - 1
              indx = ind(l)
              IF (x(indx)>t) CYCLE LOOP_MAIN
!    GO TO 50
              lselect = 60
              CYCLE LOOP_MAIN
! FIND AN ELEMENT IN THE LOWER PART OF THE ARRAY WHICH IS NOT SMALLER THAN T

!    60 CONTINUE
            CASE (60)
              k = k + 1
              indx = ind(k)
              IF (x(indx)<t) CYCLE LOOP_MAIN
! GO TO 60

! IF K <= L, INTERCHANGE ELEMENTS K AND L

              IF (k<=l) THEN
                lselect = 40
                CYCLE LOOP_MAIN
              END IF
!GO TO 40

! SAVE THE UPPER AND LOWER SUBSCRIPTS OF THE PORTION OF THE
!   ARRAY YET TO BE SORTED
              lselect = 80
              IF (l-i>j-k) THEN
                il(m) = i
                iu(m) = l
                i = k
                m = m + 1
!      GO TO 80
                CYCLE LOOP_MAIN
              END IF

              il(m) = k
              iu(m) = j
              j = l
              m = m + 1
!    GO TO 80
              CYCLE LOOP_MAIN

! BEGIN AGAIN ON ANOTHER UNSORTED PORTION OF THE ARRAY

!    70 CONTINUE
            CASE (70)
              m = m - 1
              IF (m==0) RETURN
              i = il(m)
              j = iu(m)
              lselect = 80
              CYCLE LOOP_MAIN

!    80 CONTINUE
            CASE (80)
              IF (j-i>=11) THEN
                lselect = 30
                CYCLE LOOP_MAIN
              END IF
!    GO TO 30
!    IF (I == 1) GO TO 20
              lselect = 1
              IF (i==1) CYCLE LOOP_MAIN
              lselect = 2
              CYCLE LOOP_MAIN
            CASE (2)
              i = i - 1

! SORT ELEMENTS I+1,...,J.  NOTE THAT 1 <= I < J AND J-I < 11.

LOOP_90:      DO
                i = i + 1
                IF (i==j) THEN
                  lselect = 70
                  CYCLE LOOP_MAIN
                END IF
!GO TO 70
                indx = ind(i+1)
                t = x(indx)
                it = indx
                indx = ind(i)
                IF (x(indx)<=t) CYCLE LOOP_90
                k = i

LOOP_100:       DO
                  ind(k+1) = ind(k)
                  k = k - 1
                  indx = ind(k)
                  IF (t<x(indx)) CYCLE LOOP_100
                  EXIT LOOP_100
                END DO LOOP_100

                ind(k+1) = it
              END DO LOOP_90
            END SELECT

          END DO LOOP_MAIN
        END SUBROUTINE qsortd

      END SUBROUTINE dhbc_eq_list_of_triplets

      SUBROUTINE list_of_triplets(sparse,triplets)
! Accumulate a list of triplets.  If space runs out, new
! arrays are allocated that (attempt to) hold all the data. 
! Non-positive subscripts are ignored.  
        IMPLICIT NONE
        TYPE (dpTripletList), INTENT (INOUT) :: sparse
        TYPE (dpTriplet), INTENT (IN) :: triplets(:)
        INTEGER, ALLOCATABLE :: tempInt(:)
        REAL (dkind), ALLOCATABLE :: tempReal(:)
        INTEGER istat, j, k, last, m

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
          IF ( .NOT. allocated(sparse%values)) THEN
! We are forcing expansion Factor to be > one
            m = int(k*sparse%expansionFactor)
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
              m = max(k+last,int(j*sparse%expansionFactor))
! Allocate spaec separately to minimize temporary memory use
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
! Define local variables for copying data.
        TYPE (dpTriplet), ALLOCATABLE :: local(:), temp(:)
        INTEGER i, icount, istat, j, k, l, m, n
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

! print routines for each of the derived types 
! useful to check constructions and for debugging
      SUBROUTINE printDpTriplet(triplet)
      TYPE(dpTriplet), INTENT(IN) :: triplet

      WRITE(*,'(''Row and Column Indices: '',I7,'','',I7,'' Value: '',E16.8)') &
         triplet%rowIndex,  triplet%columnIndex,  triplet%value

      END SUBROUTINE printDpTriplet

      SUBROUTINE printDpTripletList(tripletList)
      TYPE(dpTripletList), INTENT(IN) :: tripletList
      INTEGER :: i

      WRITE(*, '(''Number of items in list: '',I7)') tripletList%lastTriplet
      WRITE(*, '(''Error flag: '',i5,''  Exapnsion factor: '',f7.3)') &
           tripletList%errFlag, tripletList%expansionFactor 

      IF (tripletList%lastTriplet /= 0) THEN
        WRITE(*,'(I7,'': ('',I7,'','',I7,''): '',E16.8)') &
           (i, tripletList%rows(i), tripletList%columns(i), &
            tripletList%values(i), i = 1, tripletList%lastTriplet)
      ELSE
        WRITE(*, '(''List empty'')')
      END IF

      END SUBROUTINE printDpTripletList

      SUBROUTINE printDpHBSparseMatrix(h)
      TYPE(dpHBSparseMatrix), INTENT(IN) :: h

      INTEGER :: col, row, rowStart, rowEnd

      WRITE(*, '(''Number of rows and columns: '',I6,'', '',I6)') &
         h%noOfRows, h%noOfColumns
      WRITE(*, '(''Error flag: '',i5)') h%errFlag
      DO col = 1, h%noOfColumns
        rowStart = h%colStartIndices(col)
        rowEnd = h%colStartIndices(col+1)
        IF (rowStart == rowEnd) THEN
          WRITE(*, '(''Column '',I6,'' all zero'')') col
        ELSE
          WRITE(*, '(''Non-zeros in column '',I6)') col
          DO row = rowStart, rowEnd-1
            WRITE(*, '(I6, E14.6)') h%rowIndices(row), h%values(row) 
          END DO 
        END IF
      END DO

      END SUBROUTINE printDpHBSparseMatrix

    END MODULE sparseStructure
