    PROGRAM testSparseStruct
      USE set_precision, ONLY : dkind
      USE sparseTypes, ONLY : dpTriplet, dpTripletList, dpHBSparsematrix, &
        getExpansionFactor, setExpansionFactor

      USE sparseAssign, ONLY : ASSIGNMENT (=)
      USE sparseOps, ONLY : OPERATOR (.t.), OPERATOR (.p.),&
                            OPERATOR(+), ASSIGNMENT (=)
      IMPLICIT NONE
! Test the overloaded assignment associated with the Harwell-Boeing
! sparse matrix derived types.  The sparse operations are compared to
! dense matrix equivalents.  Results are compared and should agree to
! within rounding errors.

      INTEGER, PARAMETER :: mmax = 100, nmax = 100, ntests = 7
      REAL (dkind), PARAMETER :: zero = 0.0E0_dkind, two = 2.0E0_dkind, &
        three=3.0E0_dkind, defaultfactor = 1.2E0_dkind

      REAL (dkind) :: threshold = 0.5E0_dkind, factor
      INTEGER :: errno, i, j, k, l, m, n, test_number
      CHARACTER (len=10) :: status

      REAL (dkind), ALLOCATABLE :: a(:,:), b(:,:), c(:,:), d(:,:)
      REAL (dkind), ALLOCATABLE :: w(:), x(:), y(:), z(:)
      INTEGER :: where_failed(ntests,2)
      LOGICAL :: passed(ntests)
      CHARACTER (len=80) :: test_desc(ntests)

      TYPE (dpTripletList) :: u, v
      TYPE (dpTriplet), ALLOCATABLE :: t(:)
      TYPE (dpHBSparsematrix) :: f, g, h

      passed = .TRUE.
      where_failed = 0

! Test accessor and mutator for getting the expansion factor
      test_number = 1
      test_desc(test_number) = ' set and getExpansion factor tests'

      factor = getExpansionFactor(u)
      passed(test_number) = (factor == defaultFactor) 
      IF (factor /= defaultFactor) THEN
        errno = 1
      END IF

      CALL setExpansionFactor(u,two)
      factor = getExpansionFactor(u)
      passed(test_number) = (factor == two)
      IF (factor/= two) THEN
        errno = 10*errno + 2
      END IF

! If the test did not pass, record errno  (different from other
! tests in this package)
      IF ( .NOT. passed(test_number)) THEN
        where_failed(test_number,1) = errno
      END IF


      DO n = 10, nmax, 10
        DO m = 10, mmax, 10
          ALLOCATE (a(m,n),b(m,n),c(m,n),d(n,m),STAT=errno)
          IF (errno /= 0) THEN
            WRITE(*, '(''Allocation of array space fails'')')
            STOP
          END IF
! Get random numbers in A and B.  Set some
! entries to ZERO (=0) to emulate sparse matrices.
          CALL random_number(a)
          WHERE (a > threshold) a = zero
!  Build separate lists for the non-zero entries in g and h.
          DO j = 1, n
            DO i = 1, m
              u = dpTriplet(i,j,a(i,j))
            END DO
          END DO
          CALL random_number(b)
          WHERE (b < threshold) b = zero
!  Build separate lists for the non-zero entries in g and h.
          DO j = 1, n
            DO i = 1, m
              v = dpTriplet(i,j,b(i,j))
            END DO
          END DO

! Construct Harwell-Boeing matrices from
! the two separate lists.   
          g = u
          h = v

! Recover the non-zero values from G as a list of Triplets.
          t = g

! The list of non-zero values should agree exactly with
! the non-zero values of A.
          k = size(t)
          c = zero
! Construct a dense matrix from the list of Triplets.    
          DO l = 1, k
            c(t(l)%rowindex,t(l)%columnindex) = t(l) %value
          END DO

! Test if a Harwell-Boeing matrix agrees with an
! equivalent dense matrix.  These should agree exactly.  
          test_number = 2
          test_desc(test_number) = &
            ' HB Matrix Agrees with Dense Equivalent Matrix.'
          passed(test_number) = passed(test_number) .AND. (sum(a-c)==zero)

! If the test did not pass, record M,N where it failed.
          IF ( .NOT. passed(test_number) .AND. sum(where_failed(test_number, &
              :))==0) THEN
            where_failed(test_number,1) = m
            where_failed(test_number,2) = n
          END IF

! Deallocate array space in preparation for next test
! Explicitly
          DEALLOCATE (t,STAT=errno)
          IF (errno /= 0) THEN
            WRITE(*, '(''Failed to deallocate t matrix prior to test 2'','// &
                     ''' Errno = '', i5)') errno
          END IF

! and Implicitly
          u = 0
          g = 0

! Test 2 is to test the accumulation feature. Generate a matrix
! and add it twice to the list and hence the HB matrix
! Get random numbers in A and B.  Set some
! entries to ZERO (=0) to emulate sparse matrices.
          CALL random_number(a)
          WHERE (a>threshold) a = zero
!  Build separate lists for the non-zero entries in A and B.
          DO j = 1, n
            DO i = 1, m
              u = dpTriplet(i,j,a(i,j))
              u = dpTriplet(i,j,two*a(i,j))
            END DO
          END DO
! Construct Harwell-Boeing matrices from
! the two separate lists.   
          g = u

! Recover the non-zero values from G as a list of triplets.
          t = g

! The list of non-zero values should agree exactly with
! the non-zero values of A.
          k = size(t)
          c = zero
! Construct a dense matrix from the list of triplets.    
          DO l = 1, k
            c(t(l)%rowindex,t(l)%columnindex) = t(l) %value
          END DO

! Test if a Harwell-Boeing matrix agrees with an
! equivalent dense matrix.  These should agree exactly.  
          test_number = 3
          test_desc(test_number) = ' HB Matrix Accumulation test'
          passed(test_number) = &
            (sum(three*a-c)<=m*n*epsilon(zero))

! If the test did not pass, record M,N where it failed.
          IF ( .NOT. passed(test_number) .AND. sum(where_failed(test_number, &
              :))==0) THEN
            where_failed(test_number,1) = m
            where_failed(test_number,2) = n
          END IF

          DEALLOCATE (t,STAT=errno)
          IF (errno /= 0) THEN
            WRITE(*, '(''Failed to deallocate t matrix prior to test 3'','// &
                     ''' Errno = '', i5)') errno
          END IF

          a = three*a
! Compute the transpose of a Harwell-Boeing matrix.
          f = .t. g
! Recover the non-zero values from the transpose as a list of triplets.
          t = f
! The list of non-zero values should agree exactly with
! the non-zero values of A.
          k = size(t)
          d = zero
! Construct a dense matrix from the list of triplets.    
          DO l = 1, k
            d(t(l)%rowindex,t(l)%columnindex) = t(l) %value
          END DO
! Test if transpose of Harwell-Boeing sparse matrix 
! agrees with transpose of an identical dense matrix.    
          test_number = 4
          test_desc(test_number) = &
            ' HB Matrix (.t.) Agrees with Dense Matrix Transpose.'
          passed(test_number) = &
            (sum(d-transpose(a))==0)
! If the test did not pass, record M,N where it failed.
          IF ( .NOT. passed(test_number) .AND. sum(where_failed(test_number, &
              :))==0) THEN
            where_failed(test_number,1) = m
            where_failed(test_number,2) = n
          END IF
! Test matrix-vector products:
          ALLOCATE (x(n),y(m),z(m),w(n))
          CALL random_number(x)
! Make dense vector also sparse but no special storage.        
          WHERE (x>0.1_dkind) x = zero
          y = matmul(a,x)
          z = y - (g.p.x)
! Test if defined operation G .p. x agrees
! with dense matmul(A,x).  There may be rounding errors.
          test_number = 5
          test_desc(test_number) = &
            ' HB Matrix * Vector (.p.) Agrees with Dense Matrix * Vector'
          passed(test_number) =  &
            (abs(sum(z))<=epsilon(zero)*sum(y))
! If the test did not pass, record M,N where it failed.
          IF ( .NOT. passed(test_number) .AND. sum(where_failed(test_number, &
              :))==0) THEN
            where_failed(test_number,1) = m
            where_failed(test_number,2) = n
          END IF

          CALL random_number(y)
! Make dense vector also sparse but no special storage.    
          WHERE (y>0.1_dkind) y = zero
          x = matmul(y,a)
          w = x - (y.p.g)
! Test if defined operation g .p. x agrees
! with dense matmul(A,x).  There may be rounding errors.
          test_number = 6
          test_desc(test_number) = &
            ' Vector * HB Matrix (.p.) Agrees with Vector * Dense Matrix'
          passed(test_number) =  &
            (abs(sum(w))<=epsilon(zero)*sum(x))
! If the test did not pass, record M,N where it failed.
          IF ( .NOT. passed(test_number) .AND. sum(where_failed(test_number, &
              :))==0) THEN
            where_failed(test_number,1) = m
            where_failed(test_number,2) = n
          END IF
! Add g+h, but as a sparse matrix.  This should agree
! with the result as a dense matrix.  There will be rounding
! error differences.
          test_number = 7
          test_desc(test_number) = &
            ' HB Matrix + HB Matrix Agrees with Dense Matrix Sum.'
          f=g+h 
! Recover the non-zero values from the sum as a list of triplets.
          deallocate(t)
          t=f 
          k=size(t)
          c=zero
! Construct a dense matrix from the list of triplets resulting from g+h.    
          do l=1,k
             c(t(l) % rowindex,t(l) % columnindex) = t(l) % value
          end do
! Test if sum of Harwell-Boeing sparse matrices 
! agrees with sum of identical dense matrices.
! There may be rounding errors.
    passed(test_number)= &
    (abs(sum(a+b-c)) <= epsilon(zero)*sum(a+b))
! If the test did not pass, record M,N where it failed.
    if(.not.passed(test_number).and.sum(where_failed(test_number,:))==0) then
        where_failed(test_number,1)=m
        where_failed(test_number,2)=n 
    end if
! Deallocation occurs for each value of M, N.
! This is inefficient but adequate for testing purposes.         
          DEALLOCATE (a,b,c,d,t,w,x,y,z, STAT=errno)
          IF (errno /= 0) THEN
            WRITE(*, '(''Failed to deallocate at end of tests'','// &
                     ''' Errno = '', i5)') errno
          END IF
          u = 0
          v = 0
          f = 0
          g = 0
          h = 0
        END DO
      END DO

! Summarize the results of the tests.  Print out first
! case of failures, if they occur.

      WRITE (*,'(A,3x,A,8x,A)') 'Test', 'Status', 'Description'
! Treat the first case differently as it's testing the
! set and get Expansion Factor routines`
      status = 'Passed'
      IF (.NOT.passed(1)) THEN
        status = 'FAILED'
        SELECT CASE (where_failed(1,1))
        CASE(1) 
          WRITE (*,'(A)') 'getExpansionFactor call failed'
        CASE(2)
          WRITE (*,'(A)') 'setExpansionFactor call failed'
        CASE(12)
          WRITE (*,'(A)') 'Both get and setExpansionFactor calls failed'
        END SELECT 
      END IF
      WRITE (*,'(I3,4x,A,3x,A)') 1, status, test_desc(1)

! All the other possible error messages have the same format
      DO i = 2, ntests
        status = 'Passed'
        IF ( .NOT. passed(i)) THEN
          status = 'FAILED'
          WRITE (*,'(A,3x,2I5)') 'First Noted at Values (M,N) =', &
            where_failed(i,:)
        END IF
        WRITE (*,'(I3,4x,A,3x,A)') i, status, test_desc(i)
      END DO

    END PROGRAM testSparseStruct
