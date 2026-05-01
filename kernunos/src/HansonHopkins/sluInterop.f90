    MODULE sluinterop
      USE, INTRINSIC :: iso_c_binding
      USE set_precision, ONLY : dkind
      USE sparsetypes, ONLY : dphbsparsematrix, superlu_options_t, &
          slu_dpHBSparseMatrix, mem_usage
      IMPLICIT NONE


! Define procedure names for assignment
! TYPE(slu_dpHBSparseMatrix) = TYPE(dpHBSparseMatrix)
! TYPE(slu_dpHBSparseMatrix) = 0
     INTERFACE ASSIGNMENT(=)
        MODULE PROCEDURE slu_dhbc_eq_dhbc, clear_slu_dhbc
     END INTERFACE

! The .ip. and .pi. operations are for solving
! sparse systems using the SuperLU package.  
     INTERFACE OPERATOR (.ip.)
         MODULE PROCEDURE solve_ext_sparse_matrix_dense_rhsv,&
         solve_sparse_matrix_dense_rhsv
     END INTERFACE 

     INTERFACE OPERATOR (.pi.)
         MODULE PROCEDURE solve_dense_rhsv_ext_sparse_matrix,&
         solve_dense_rhsv_sparse_matrix
     END INTERFACE

! This defines the interfaces to C codes used for the support of 
! defined operations .ip. and .pi.
      INTERFACE
        SUBROUTINE set_default_options(options) BIND(C, &
            NAME='set_default_options')
          IMPORT superlu_options_t
          TYPE (superlu_options_t), INTENT (INOUT) :: options
        END SUBROUTINE

        SUBROUTINE factor_superlu(iopt,n,nnz,values,ir,ip,options, &
            drop_tol, f_factors,info) &
            BIND(C,NAME='FACTOR_with_SuperLU')

!   This usage allows changes to arguments due to the 
!   LU factorization.

!   FACTOR_with_SuperLU(int *iopt, int *n, int *nnz, 
!         double *values, int *rowind, int *colptr,
!         superlu_options_t *options, double *drop_tol,
!         fptr *f_factors, int *info) 

          USE, INTRINSIC :: iso_c_binding
          IMPORT superlu_options_t, dkind
          IMPLICIT NONE

          INTEGER (c_int), INTENT (IN) :: iopt, n, nnz
          INTEGER (c_int), INTENT (INOUT) :: info
          TYPE (superlu_options_t), INTENT (INOUT) :: options
          REAL (c_double), INTENT (INOUT) :: values(*), drop_tol
          INTEGER (c_int), INTENT (INOUT) :: ir(*), ip(*)
          TYPE (c_ptr), INTENT (INOUT) :: f_factors
        END SUBROUTINE

        SUBROUTINE solve_superlu(iopt,n,b,ldb,nrhs,options,trans, &
            f_factors,info) BIND(C,NAME='SOLVE_with_SuperLU')

!   This usage assumes static arguments due to any solve step.

!   SOLVE_with_SuperLU(int *iopt, int *n, int *nnz, 
!         double *b, int *ldb, int *nrhs, 
!         superlu_options_t *options, trans_t *trans, 
!         fptr *f_factors, int *info) 

          USE, INTRINSIC :: iso_c_binding
          IMPORT superlu_options_t, dkind
          IMPLICIT NONE

          INTEGER (c_int), INTENT (IN) :: iopt, n, ldb, nrhs, trans
          INTEGER (c_int), INTENT (OUT) :: info
          TYPE (superlu_options_t), INTENT (IN) :: options
          TYPE (c_ptr), INTENT (IN) :: f_factors
          REAL (c_double), INTENT (INOUT) :: b(ldb,*)
        END SUBROUTINE

        SUBROUTINE clear_superlu(iopt,options,f_factors,info) &
            BIND(C, NAME='CLEAR_SuperLU')

!   This usage allows changes to arguments due to memory release.

!    CLEAR_SuperLU(int *iopt, superlu_options_t *options, 
!         fptr *f_factors, int *info) 

          USE, INTRINSIC :: iso_c_binding
          IMPORT superlu_options_t, dkind
          IMPLICIT NONE

          INTEGER (c_int), INTENT (IN) :: iopt
          INTEGER (c_int), INTENT (OUT) :: info
          TYPE (superlu_options_t), INTENT (INOUT) :: options
          TYPE (c_ptr), INTENT (INOUT) :: f_factors
        END SUBROUTINE

      END INTERFACE

    CONTAINS
      FUNCTION solve_dense_rhsv_sparse_matrix(b,h) RESULT (y)
! A new factorization is computed each time the operator is invoked.
! Support for Y = B .pi. H. 
        TYPE (dphbsparsematrix), INTENT (IN) :: h
        TYPE (slu_dpHBSparseMatrix) :: g
        REAL (dkind), INTENT (IN) :: b(*)
        REAL (dkind) :: y(h%noofcolumns)
        INTEGER :: trans
! Ascend H to become a component of G.  
        g = h
! Call a routine that does the computations.  There will
! be changes to components of G that result from computing
! a factorization.
        y(1:h%noofcolumns) = b(1:h%noofcolumns)
        trans = 1
        CALL solve_sparse_system(g,y,trans)
! This releases memory but further solves take
! refactoring.        
        g = 0
      END FUNCTION

      FUNCTION solve_sparse_matrix_dense_rhsv(h,b) RESULT (y)
! A new factorization is computed each time the operator is invoked.
! Support for Y = H .ip. B.    
        TYPE (dphbsparsematrix), INTENT (IN) :: h
        TYPE (slu_dpHBSparseMatrix) :: g
        REAL (dkind), INTENT (IN) :: b(*)
        REAL (dkind) :: y(h%noofcolumns)
        INTEGER :: trans
! Ascend H to become a component of G.  
        g = h
! Call a routine that does the computations.  There will
! be changes to components of G that result from computing
! a factorization.
        y(1:h%noofcolumns) = b(1:h%noofcolumns)
        trans = 0
        CALL solve_sparse_system(g,y,trans)
! This releases memory but further solves take
! refactoring.        
        g = 0
      END FUNCTION

      FUNCTION solve_dense_rhsv_ext_sparse_matrix(b,g) RESULT (y)
! The LU factorization is done prior to this use with
! the overloaded assignment G = H.
! The updates are of the type that saves the sparse LU
! factorization so that further instances of using the
! .pi. operations are efficient. 
        TYPE (slu_dpHBSparseMatrix), INTENT (IN) :: g
        REAL (dkind), INTENT (IN) :: b(*)
        REAL (dkind) :: y(g%hbMatrix%noofcolumns)
        INTEGER :: trans
! Call a routine that does the computations.
        y(1:g%hbMatrix%noofcolumns) = b(1:g%hbMatrix%noofcolumns)
        trans = 1
        CALL solve_sparse_system(g,y,trans)
      END FUNCTION

      FUNCTION solve_ext_sparse_matrix_dense_rhsv(g,b) RESULT (y)
! The LU factorization is done prior to this use with
! the overloaded assignment G = H.
! The updates are of the type that saves the sparse LU
! factorization so that further instances of using the
! .ip. operations are efficient.
        TYPE (slu_dpHBSparseMatrix), INTENT (IN) :: g
        REAL (dkind), INTENT (IN) :: b(*)
        REAL (dkind) :: y(g%hbMatrix%noofcolumns)
        INTEGER :: trans
! Call a routine that does the computations.
        y(1:g%hbMatrix%noofcolumns) = b(1:g%hbMatrix%noofcolumns)
        trans = 0
        CALL solve_sparse_system(g,y,trans)
      END FUNCTION

      SUBROUTINE factor_sparse_system(g)
        TYPE (slu_dpHBSparseMatrix), INTENT (INOUT) :: g

        INTEGER :: i, info, k, n, nnz
        INTEGER :: any_rows(g%hbMatrix%noofcolumns)

! Get the system size (must be same as number of rows)
! and the number of non-zero values. 

        n = g%hbMatrix%noofcolumns
        IF (n/=g%hbMatrix%noofrows) THEN
! This is an error flag - the matrix is not square.           
          g%info = -1
          RETURN
        END IF
        nnz = g%hbMatrix%colstartindices(n+1) - 1
! If the matrix is factored, K=2.  Otherwise K=1.
! This results in a factorization followed by solves.
! The value K is set to 1 when the factorization completes.           
        k = g%options%fact + 1
! Scan the matrix and check for the structural defects
! of missing rows or columns.
        IF (k==1) THEN
          any_rows = 0
          DO i = 1, nnz
            any_rows(g%hbMatrix%rowindices(i)) = 1
          END DO
          IF (any(any_rows==0)) THEN
! This is an error flag - the matrix is missing at least one row.           
            g%info = -2
            RETURN
          END IF
          DO i = 1, n - 1
            IF (g%hbMatrix%colstartindices(i)==g%hbMatrix%colstartindices(i+1)) THEN
! This is an error flag - the matrix is missing at least one col.           
              g%info = -3
              RETURN
            END IF
          END DO
        END IF
! First factor, then solves are available.   
        CALL factor_superlu(k,n,nnz,g%hbMatrix%values,g%hbMatrix%rowindices, &
          g%hbMatrix%colstartindices,g%options,g%dropTol,g%factorData,info)
! If the factorization succeeded then note that it does not
! have to be done again.               
        IF (info==0) g%options%fact = 1
! If a zero pivot was found INFO > 0 gives the first column.           
        g%info = info
      END SUBROUTINE factor_sparse_system

      SUBROUTINE solve_sparse_system(g,b,trans)
        TYPE (slu_dpHBSparseMatrix), INTENT (IN) :: g
        INTEGER, INTENT (IN) :: trans
        REAL (dkind), INTENT (INOUT) :: b(*)
        INTEGER :: info, n, nrhs, ldb
! Get the system size (must be same as number of rows).

        n = g%hbMatrix%noofcolumns
        nrhs = 1
        ldb = n
! Factorization is available, now solve.      
        CALL solve_superlu(g%options%fact+1,n,b,ldb,nrhs,g%options,trans, &
          g%factorData,info)
! Can't save INFO as a component of G.  That is forced
! by INTENT(IN) on G. This is a Fortran requirement.
      END SUBROUTINE

SUBROUTINE fortran_print(info,n,nnzl,nnzu,memuse) BIND(C, &
    NAME='fortran_print')
! This routine is called from the C wrapper.  It summarizes
! the results and memory usage of factoring the sparse
! Harwell-Boeing square matrix.
  USE, INTRINSIC :: iso_c_binding
  USE sparsetypes, ONLY : mem_usage
  IMPLICIT NONE

  TYPE (mem_usage), INTENT (IN) :: memuse
  INTEGER (c_int), INTENT(IN) :: info, n, nnzl, nnzu

! If INFO==0 the factorization was a success.  The value
! if positive, gives the column where a zero pivot was found.
  IF (info==0) THEN
    WRITE (*,'(A,I10)') 'The SuperLU factorization completed with N =', &
      n
  ELSE
    WRITE (*,'(A,I10/A,I10)') &
      'The SuperLU factorization DID NOT complete with N =', n, &
      'The first column where a zero pivot occurred = ', info
    RETURN
  END IF
  WRITE (*,'(/A,3I10)') 'The nonzero values of L, U and their sum =', &
    nnzl, nnzu, nnzl + nnzu
  WRITE (*,'(/A,1PE12.6/A,1PE12.6/)') &
    'Memory Usage (KBytes) for LU step   = ', memuse%for_lu*1.E-3, &
    'Total Usage  (KBytes) for all terms = ', memuse%total_needed*1.E-3
END SUBROUTINE

      SUBROUTINE slu_dhbc_eq_dhbc(g,h)
        IMPLICIT NONE
        TYPE (slu_dpHBSparseMatrix), INTENT (INOUT) :: g
        TYPE (dphbsparsematrix), INTENT (IN) :: h
        INTEGER :: itemp
! Copy the Harwell-Boeing part into place.       
        g%hbMatrix = h
! Get default options for use in SuperLU solve steps.
! Give special treatment to the PrintStat parameter.
! This may have been turned on in user code.
        itemp = g%options%printstat
        CALL set_default_options(g%options)
        g%options%printstat = itemp
! Set iterative refinement to be a default.
        g%options%iterrefine = 1
! Flag that the LU factorization is not available.           
        g%options%fact = 0
! Default setting of the LU factorization flag.
        g%info = 0
        CALL factor_sparse_system(g)
      END SUBROUTINE

      SUBROUTINE clear_slu_dhbc(g,i)
        IMPLICIT NONE
        TYPE (slu_dpHBSparseMatrix), INTENT (INOUT) :: g
        INTEGER, INTENT (IN) :: i

        INTEGER :: info, iopt

! Only the specific value I==0 results in clearing
! the storage used by factoring the matrix.

        IF (i==0) THEN
          iopt = 3
! Unload storage in the C code. The name used here is
! to inform the compiler of changes to the arguments. 
! No factorization is performed, only memory release.          
          CALL clear_superlu(iopt,g%options,g%factorData,info)
! Flag that the LU factorization is not available.           
          g%options%fact = 0
          g%info = info
        END IF
      END SUBROUTINE

    END MODULE sluinterop
