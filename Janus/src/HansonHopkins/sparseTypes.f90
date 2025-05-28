    MODULE sparseTypes

      USE set_precision, ONLY : dkind
      USE iso_c_binding
      IMPLICIT NONE

      REAL(dkind), PARAMETER, PRIVATE :: zero = 0.0e0_dkind, & 
                                defaultExpansionF = 1.2E0_dkind

! Define double precision triplets with three fields; the row
! and column index and value of each non-zero element.
      TYPE dpTriplet
        INTEGER :: rowIndex, columnIndex
        REAL (dkind) :: value
      END TYPE dpTriplet

! Define a "list" of dpTriplets using arrays to allow 
! efficient access and sorting. This is also known as a Coordinate List format (COO)
      TYPE dpTripletList
! Define an expansion factor to allow extra free space to 
! be allocated for efficiency reasons. The mutator function
! setExpansionFactor needs to be used to change this value
! to ensure that it is always at least one.
        REAL (dkind), PRIVATE :: expansionFactor = defaultExpansionF
! lastTriplet is the array index to the current end of data
        INTEGER :: lastTriplet = 0
! Status of the list; an allocation error has occurred if
! errFlag has been set to a non-zero value
        INTEGER :: errFlag = 0
! Arrays to store triplet data
        INTEGER, ALLOCATABLE :: rows(:), columns(:)
        REAL (dkind), ALLOCATABLE :: values(:)
      END TYPE dpTripletList

! Define double precision Compressed Sparse Row format matrix as defined in sparsekit.f90
! and see https://en.wikipedia.org/wiki/Sparse_matrix, known as CSR or Yale format
! for convenience, the no of columns is defined as well, though it is not part of the format
! It is defined using overloaded assignment:
! TYPE(dpCSRSparseMatrix) = TYPE(dpTripletList).  This will be used
! after the triplets list has been accumulated in a
! variable of TYPE(dpTripletList).
      TYPE dpCSRSparseMatrix
! The number of rows in the matrix, number of columns in the matrix, and number of values.
! These values are defined, in the overloaded
! assignment, as the maximum indices noted for
! any triplet.
      INTEGER :: noOfRows = 0
      INTEGER :: noOfColumns = 0
      INTEGER :: nnz = 0
! This flag is non-zero if there is any allocation error
! during creation of a Compressed Sparse Row matrix with overloaded
! assignment.
       INTEGER :: errFlag = 0
! values of the non-zero elements
       real ( kind = 8 ), allocatable :: a(:)
! column index
       integer ( kind = 4 ), allocatable :: ja(:)
! row index
       integer ( kind = 4 ), allocatable :: ia(:)
      END TYPE dpCSRSparseMatrix

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

! This derived type repeats the structure (using the
! same name) that is a parameter for the SuperLU codes.
      TYPE, BIND(C) :: superlu_options_t
          INTEGER (c_int) :: Fact
          INTEGER (c_int) :: Equil 
          INTEGER (c_int) :: ColPerm
          INTEGER (c_int) :: Trans 
          INTEGER (c_int) :: IterRefine
          REAL (c_double) :: DiagPivotThresh
          INTEGER (c_int) :: SymmetricMode
          INTEGER (c_int) :: PivotGrowth
          INTEGER (c_int) :: ConditionNumber
          INTEGER (c_int) :: RowPerm
          INTEGER (c_int) :: ILU_DropRule
          REAL (c_double) :: ILU_DropTol
          REAL (c_double) :: ILU_FillFactor
          INTEGER (c_int) :: ILU_Norm
          REAL (c_double) :: ILU_FillTol
          INTEGER (c_int) :: ILU_MILU;
          REAL (c_double) :: ILU_MILU_Dim
          INTEGER (c_int) :: ParSymbFact
          INTEGER (c_int) :: ReplaceTinyPivot
          INTEGER (c_int) :: SolveInitialized
          INTEGER (c_int) :: RefineInitialized
          INTEGER (c_int) :: PrintStat=0
      END TYPE superlu_options_t
!typedef struct {
!    fact_t        Fact;
!    yes_no_t      Equil;
!    colperm_t     ColPerm;
!    trans_t       Trans;
!    IterRefine_t  IterRefine;
!    double        DiagPivotThresh;
!    yes_no_t      SymmetricMode;
!    yes_no_t      PivotGrowth;
!    yes_no_t      ConditionNumber;
!    rowperm_t     RowPerm;
!    int          ILU_DropRule;
!    double       ILU_DropTol;    /* threshold for dropping */
!    double       ILU_FillFactor; /* gamma in the secondary dropping */
!    norm_t       ILU_Norm;       /* infinity-norm, 1-norm, or 2-norm */
!    double       ILU_FillTol;    /* threshold for zero pivot perturbation */
!    milu_t       ILU_MILU;
!    double       ILU_MILU_Dim;   /* Dimension of PDE (if available) */
!    yes_no_t      ParSymbFact;
!    yes_no_t      ReplaceTinyPivot; /* used in SuperLU_DIST */
!    yes_no_t      SolveInitialized;
!    yes_no_t      RefineInitialized;
!    yes_no_t      PrintStat;
!} superlu_options_t;
      TYPE slu_dpHBSparseMatrix
! This is the sparse matrix involved in the solve step.
          TYPE(dpHBSparseMatrix) :: hbMatrix
! This is the options used in the solve step.    
          TYPE(superlu_options_t) :: options
! This is the drop tolerance used
! in the SuperLU factorization.  It is initialized
! to the value 0.      
          REAL(dkind) :: dropTol = zero
! This is the C pointer, where the factored matrix 
! and its related data are stored.      
          TYPE(c_ptr) :: factorData
! This is the flag value resulting from a factorization
! by the SuperLU package.
          INTEGER :: info = 0    
      END TYPE slu_dpHBSparseMatrix

! This structure is passed back from the C program.  Note that
! the components have fixed single precision types for two components.        
      TYPE, BIND (C) :: mem_usage
        REAL (c_float) :: for_lu
        REAL (c_float) :: total_needed
      END TYPE mem_usage

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

END MODULE sparseTypes
