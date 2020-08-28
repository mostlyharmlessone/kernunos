MODULE sparseOps
  USE set_precision, ONLY : dkind
  USE sparseTypes, ONLY: dpTriplet, dpTripletList, &
      dpHBSparseMatrix
  USE sparseAssign, ONLY: ASSIGNMENT(=)

  IMPLICIT NONE

  REAL(dkind), PRIVATE :: zero = 0.0e0_dkind

! Define procedure name for unary defined operation (.t.),
! the transpose of a Harwell-Boeing matrices.
      INTERFACE OPERATOR (.t.)
        MODULE PROCEDURE transpose_dhbc
      END INTERFACE

! Define the procedure name for the user-defined operation +,
! the sum of two Harwell-Boeing matrices.
      INTERFACE OPERATOR (+)
        MODULE PROCEDURE sum_dhbc_plus_dhbc
      END INTERFACE

! Define procedure names for defined operation (.p.), the product of a
! Harwell-Boeing matrix and dense array, i.e. y = H * x, or y = x * H.
! An error occurs if SIZE(x) is incompatible with the dimensions of H.
! The * operator is not overloaded to avoid confusing it with element 
! by element multiplication.
      INTERFACE OPERATOR (.p.)
        MODULE PROCEDURE sparse_matrix_times_vector, &
          vector_times_sparse_matrix, sparse_matrix_times_matrix, &
          matrix_times_sparse_matrix
      END INTERFACE

! The (*) operation can be replaced by (.p.) and
! will obtain the same results, for dense vectors.
! For dense matrices use the (.p.) operation.  
      INTERFACE OPERATOR (*)
        MODULE PROCEDURE sparse_matrix_times_vector, &
          vector_times_sparse_matrix
      END INTERFACE

     CONTAINS

      FUNCTION matrix_times_sparse_matrix(x,h) RESULT (y)
! This routine handles the overloaded operation (*)
! that stands for a dense matrix times a sparse-matrix.
! The result is a dense matrix y = x^T*H.  The number of
! rows for x must be at least the number of rows in H.  The
! result y will be the number of columns in x by
! the number of columns in H.
        IMPLICIT NONE
        TYPE (dpHBSparseMatrix), INTENT (IN) :: h
        REAL (dkind), INTENT (IN) :: x(:,:)
        REAL (dkind) :: y(SIZE(x,2),h%noOfColumns)
! Local variables
        INTEGER :: j, k, n

        n = h%noOfColumns
        k = SIZE(x,2)

        DO j = 1, k
          y(j,1:n) = x(:,j)*h
        END DO

      END FUNCTION matrix_times_sparse_matrix

      FUNCTION sparse_matrix_times_matrix(h,x) RESULT (y)
! This routine handles the overloaded operation (*)
! that stands for a sparse-matrix times a dense matrix.
! The result is y = H*x.  The first dimension of x must
! be at least the number of columns in H.  The result y
! will be the number of rows in H by the number of
! columns in x.
        IMPLICIT NONE
        TYPE (dpHBSparseMatrix), INTENT (IN) :: h
        REAL (dkind), INTENT (IN) :: x(:,:)
        REAL (dkind) :: y(h%noOfRows,SIZE(x,2))
! Local variables
        INTEGER :: j, n

        n = SIZE(x,2)
! Compute the columns of the dense matrix
! pre-multiplied by the column vectors of the sparse
! matrix. Note the use of the overloaded (*) for
! vectors.  This allows an easy implementation for 
! dense matrices.
        DO j = 1, n
          y(:,j) = h*x(:,j)
        END DO
      END FUNCTION sparse_matrix_times_matrix

      FUNCTION sparse_matrix_times_vector(h,x) RESULT (y)
! This routine handles the overloaded operation (*)
! that stands for a sparse-matrix times a dense vector.
! The result is y = H*x.  The size of x must be at least
! the number of columns in H.  The result y will be the
! number of rows in H.
        IMPLICIT NONE
        TYPE (dpHBSparseMatrix), INTENT (IN) :: h
        REAL (dkind), INTENT (IN) :: x(:)
        REAL (dkind) :: y(h%noOfRows)
! Local variables
        INTEGER :: i, j, n

        y = zero
        n = h%noOfColumns
        j = 0
! Compute the sum of components x_j of the dense vector
! pre-multiplied by the column vectors of the sparse matrix. 
        DO i = 1, h%colStartIndices(n+1) - 1
          DO WHILE (i>=h%colStartIndices(j+1))
            j = j + 1
          END DO
! This allows vectors of shorter size than the number
! of sparse matrix columns to be used.    
          IF (j>SIZE(x)) CYCLE
! Skip computation with zero values in the dense vector.   
          IF (x(j)==zero) CYCLE
          y(h%rowIndices(i)) = y(h%rowIndices(i)) + h%values(i)*x(j)
        END DO
      END FUNCTION sparse_matrix_times_vector

      FUNCTION sum_dhbc_plus_dhbc(a,b) RESULT (c)
! This function computes the sum of two Harwell-Boeing
! format sparse matrices, C = A + B.
! The dimensions of the result are determined by the
! maximum of the dimensions of the separate factors.
        IMPLICIT NONE
        TYPE (dpHBSparseMatrix), INTENT (IN) :: a, b
        TYPE (dpHBSparseMatrix) :: c
! Local variables
! Local working arrays of triplets
        TYPE (dpTriplet), ALLOCATABLE :: aa(:), bb(:)
! Local list of triplets
        TYPE (dpTripletList) :: cc
! Convert Harwell-Boeing terms to lists of triplets
        aa = a
        bb = b
! Accumulate the two lists of triplets
        cc = aa
        cc = bb
! Convert results to Harwell-Boeing form, the sum A+B.
        c = cc
! The local arrays AA,BB and list CC are deallocated
! when the function returns.             
      END FUNCTION sum_dhbc_plus_dhbc

      FUNCTION vector_times_sparse_matrix(x,h) RESULT (y)
! This routine handles the overloaded operation .p.
! that stands for a dense vector times a sparse-matrix.
! The result is y = x^T*H.  The result y will be the
! number of columns in H.
        IMPLICIT NONE
        TYPE (dpHBSparseMatrix), INTENT (IN) :: h
        REAL (dkind), INTENT (IN) :: x(:)
        REAL (dkind) :: y(h%noOfColumns)
! Local variables
        INTEGER :: i, j, n

        y = zero
        n = h%noOfColumns
        j = 0
! Compute the inner products of the columns of H
! with the corresponding components of x. 
        DO i = 1, h%colStartIndices(n+1) - 1
          DO WHILE (i>=h%colStartIndices(j+1))
            j = j + 1
          END DO
! Skip accumulating inner product if dense vector has
! a zero entry.
          IF (h%rowIndices(i)>SIZE(x)) CYCLE
          IF (x(h%rowIndices(i))==zero) CYCLE
          y(j) = y(j) + h%values(i)*x(h%rowIndices(i))
        END DO

      END FUNCTION vector_times_sparse_matrix

      FUNCTION transpose_dhbc(b) RESULT (a)
! This functions constructs the transpose of a
! Harwell-Boeing sparse matrix, A=B^T.  It supports the
! overloaded operation .t. B.

        IMPLICIT NONE
        TYPE (dpHBSparseMatrix), INTENT (IN) :: b
        TYPE (dpHBSparseMatrix) :: a

! Local working arrays of triplets
        TYPE (dpTriplet), ALLOCATABLE :: bb(:)
! Local list of triplets
        TYPE (dpTripletList) :: aa
! Local integers for indexing and swapping indices
        INTEGER :: itemp, k
! Convert Harwell-Boeing terms to list of triplets
        bb = b
! Interchange row and column indices to get a list
! of triplets corresponding to the transpose
        DO k = 1, SIZE(bb)
          itemp = bb(k)%rowIndex
          bb(k)%rowIndex = bb(k)%columnIndex
          bb(k)%columnIndex = itemp
        END DO
! Convert array of triplets to a list
        aa = bb
! Convert list to output Harwell-Boeing matrix form
        a = aa
      END FUNCTION transpose_dhbc
END MODULE sparseOps

