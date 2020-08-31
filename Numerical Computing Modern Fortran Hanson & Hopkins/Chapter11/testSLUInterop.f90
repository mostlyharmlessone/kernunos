    PROGRAM testsluinterop
      USE set_precision, ONLY : dkind
      USE sparsetypes, ONLY: dpTripletList, dpTriplet, &
          dpHBSparseMatrix, slu_dpHBSparsematrix
      USE sparseAssign, ONLY: ASSIGNMENT(=)
      USE sluInterop, ONLY: OPERATOR(.ip.), OPERATOR(.pi.), &
          ASSIGNMENT(=)
      USE sparseOps, ONLY: OPERATOR(.p.)

      IMPLICIT NONE

      REAL (dkind), ALLOCATABLE :: x(:), y(:), z(:)
      REAL (dkind) :: relerra, relerrb, dnrm2
      INTEGER, PARAMETER :: ntests = 2
      REAL (dkind), PARAMETER :: zero = 0.0e0_dkind
      INTEGER :: i, n, test_number
      INTEGER :: where_failed(ntests,2)
      TYPE (dpTripletList) :: u
      TYPE (dpHBSparseMatrix) :: h
      TYPE (slu_dpHBSparsematrix) :: gg


      LOGICAL :: passed(ntests)
      CHARACTER (len=80) :: test_desc(ntests)
      CHARACTER (len=10) :: endStatus

! Clear the Harwell-Boeing matrix.    
      h = 0

! Define a new 5 by 5 sparse matrix.  This is used
! as an example at the Matrix Market, 
! http://math.nist.gov/MatrixMarket/formats.html

      u = dpTriplet(1,1,1.000E+00_dkind)
      u = dpTriplet(2,2,1.050E+01_dkind)
      u = dpTriplet(3,3,1.500E-02_dkind)
      u = dpTriplet(1,4,6.000E+00_dkind)
      u = dpTriplet(4,2,2.505E+02_dkind)
      u = dpTriplet(4,4,-2.800E+02_dkind)
      u = dpTriplet(4,5,3.332E+01_dkind)
      u = dpTriplet(5,5,1.200E+01_dkind)

! Convert matrix to Harwell-Boeing format.
      h = u

! Make this part of the enhanced Harwell-Boeing type.
! For square systems and LU factorization is computed.

! Turn on print summary status after an LU factorization.
      gg%options%printstat = 1
      gg = h

! Start with a known vector.  Generate a right-hand side:
      n = 5
      ALLOCATE (x(n),y(n),z(n))
      x = [ (REAL(i,dkind),i=1,n) ]
! Compute the right-hand side.     
      y = h .p. x
! Solve the sytem with the generated right-hand side.
! It should be the same as the X(:) values but will
! not be due to rounding errors and conditioning.
      z = gg .ip. y
      IF (gg%info==0) z = z - x
      relerra = dnrm2(n,z,1)/dnrm2(n,x,1)
      test_number = 1
! Test if solution to system with generated right-
! hand side agrees with vector used in the product.
      test_desc(test_number) = &
        ' H-B Matrix (.ip.) b(:) Agrees with known solution.'
      passed(test_number) = (relerra <= EPSILON(zero)*10.0E0_dkind)
! If the test did not pass, record where it failed.
      IF ( .NOT. passed(test_number)) THEN
        where_failed(test_number,1:2) = 5
      END IF
! Compute a right-hand side for the transposed matrix.     
      y = x .p. h
! Turn off print status after an LU factorization.
      gg%options%printstat = 0
! Solve the transposed system.     
      z = y .pi. gg
      IF (gg%info==0) z = z - x
      relerrb = dnrm2(n,z,1)/dnrm2(n,x,1)
      test_number = 2
! Test if solution to system with generated right-
! hand side agrees with vector used in the product.
      test_desc(test_number) = &
        ' b(:) (.pi.) H-B Matrix Agrees with known solution.'
      passed(test_number) = (relerrb <= EPSILON(zero)*10.0E0_dkind)
! If the test did not pass, record where it failed.
      IF ( .NOT. passed(test_number)) THEN
        where_failed(test_number,1:2) = 5
      END IF
! Clear storage for the sparse matrices.                    
      h = 0
      gg = 0
! Summarize the results of the tests.  Print out first
! case of failures, if they occur.

      WRITE (*,'(A,3x,A,8x,A)') 'Test', 'Status', 'Description'
      DO i = 1, ntests
      endStatus = 'Passed'
        IF ( .NOT. passed(i)) THEN
          endStatus = 'FAILED'
          WRITE (*,'(A,3x,2I5)') 'First Noted at Values (M,N) =', &
            where_failed(i,:)
        END IF
        WRITE (*,'(I3,4x,A,3x,A)') i, endStatus, test_desc(i)
      END DO

    END PROGRAM testsluinterop
