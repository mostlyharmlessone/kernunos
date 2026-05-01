MODULE sparseUtils
!  Modified to add CSR sparse matrices
  USE set_precision, ONLY: dkind
  USE sparseTypes, ONLY: dpTriplet, dpTripletList, dpHBSparseMatrix, &
                         dpCSRSparseMatrix,getExpansionFactor

  CONTAINS
! print routines for each of the derived types 
! useful to check constructions and for debugging

      SUBROUTINE printDpTriplet(triplet)
      TYPE(dpTriplet), INTENT(IN) :: triplet
      WRITE(*,'(''Row and Column Indices: '',I7,'','',I7,'' Value: '',E16.8)') &
         triplet%rowIndex,  triplet%columnIndex,  triplet%value
      END SUBROUTINE printDpTriplet

      SUBROUTINE printDpTripletList(tripletList)
      TYPE(dpTripletList), INTENT(INOUT) :: tripletList
      INTEGER :: i
      REAL(dkind) :: expFactor
      expFactor = getExpansionFactor(tripletList)
      WRITE(*, '(''Number of items in list: '',I7)') tripletList%lastTriplet
      WRITE(*, '(''Error flag: '',i5,''  Expansion factor: '',f7.3)') &
           tripletList%errFlag, expFactor 
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

      SUBROUTINE printDpmatrix(matrix)
       REAL (dkind), INTENT(IN) :: matrix(:,:)
       integer :: row
       do row=1,Size(matrix,1)
        WRITE(*,*) matrix(row,1:Size(matrix,2))
       end do
      END SUBROUTINE printDpmatrix

      SUBROUTINE printDpCSRSparseMatrix(h)
      TYPE(dpCSRSparseMatrix), INTENT(IN) :: h
!      INTEGER :: i
      WRITE(*, '(''Number of rows and columns: '',I6,'', '',I6)') &
         h%noOfRows, h%noOfColumns
!      do i=1,h%nnz
!       WRITE(*, '(I6, E14.6)') h%ja(i),h%a(i)
!      end do
!      do i=1,h%noOfRows+1
!       WRITE(*, '(I6)') h%ia(i)
!      end do
       WRITE(*,*) 'Number of non-zero numbers: ',h%nnz
       WRITE(*,*) h%a(1:h%nnz)
       WRITE(*,*) h%ja(1:h%nnz)
       WRITE(*,*) h%ia(1:h%noOfRows+1)
      END SUBROUTINE printDpCSRSparseMatrix

END MODULE sparseUtils
