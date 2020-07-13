    PROGRAM drive_piecewise_linear
! Write the coefficient matrix for a least squares problem
! that comes from piece-wise linear fitting of data with a
! continuous function.  The breakpoints are equally spaced.  

! There are N unknowns in the problem.  There are M data values.
! The N unknowns are the values of the linear functions at the
! ends of the each breakpoint interval.

! The breakpoints are equally spaced.  The M data value are pairs
! (t_i, t_i ** 2).  The t_i are random on (0,1).

! The matrix  B=[A : I_M]
!               [0 : A^T] is first defined as a list of triplets.
! This matrix is assembled using overloaded assignment.
! Then B is converted to Harwell-Boeing format using overloaded
! assignment between derived types.  The sparse matrix B has
! dimension (M+N) by (M+N).

      USE sparseOps
      USE blas123, ONLY : dnrm2
      IMPLICIT NONE

! Set problem size:
      INTEGER, PARAMETER :: n = 2000
! Define array for the knots:
      REAL (dkind) :: a(n)
      REAL (dkind), ALLOCATABLE :: rhs(:), t(:), x(:), r(:)
! See if the interval has any points.  This flags
! when an interval is noted.
      INTEGER, ALLOCATABLE :: iseed(:)
      INTEGER :: m, saw_points(n-1)
! Define what will be the collection of matrix triplets.
      TYPE (dpTripletList) s
! Define the Harwell-Boeing derived type that holds the
! processed triplets.
      TYPE (dpHBSparseMatrix) b
! Define the ascended type that holds the Harwell-Boeing
! matrix and factorization quantities.
      TYPE (slu_dpHBSparseMatrix) g

! Define local variables:
      REAL (dkind) :: delta, u, v, resid_error
      INTEGER i, j, k, sz

      delta = 1.D0/real(n-1,dkind)
! Define the array of breakpoints:
      a(1) = 0.D0
      a(n) = 1.D0

! Define the knots or inner breakpoints:
      DO i = 2, n - 1
        a(i) = a(i-1) + delta
      END DO

! Generate sufficient random values so that each interval
! has at least one value.  Do not store the values.
      saw_points = 0
      m = 0
      CALL random_seed(size=sz)
      ALLOCATE (iseed(sz))
      CALL random_seed(get=iseed)
      DO WHILE (any(saw_points==0))
        CALL random_number(u)
! This direct computation of the interval containing
! the data can be off (low) by 1.  So if the value
! of the basis function is > 1, move to the next interval.
        k = max(1,min(n-1,floor(real(n*u,dkind))))
        DO
          v = (u-a(k))/delta
          IF (v<=1.D0 .OR. k==n-1) EXIT
! Move to the next interval.        
          k = k + 1
        END DO
        saw_points(k) = 1
        m = m + 1
      END DO
! Set random number seed so the same sequence results.
      CALL random_seed(put=iseed)

! Allocate local working space -
      ALLOCATE (t(m),x(n+m),r(n+m),rhs(n+m))

! Generate the random values and write the matrix entries.
! Record the function values in RHS(*).
      DO j = 1, m
        CALL random_number(t(j))
        rhs(j) = t(j)**2
! This direct computation of the interval containing
! the data can be off (low) by 1.  So if the value
! of the basis function is > 1, move to the next interval.
        k = max(1,min(n-1,floor(real(n*t(j),dkind))))
        DO
          v = (t(j)-a(k))/delta
          IF (v<=1.D0 .OR. k==n-1) EXIT
! Move to the next interval.        
          k = k + 1
        END DO
! Gather up the list of the sparse matrix triplets (S) that
! will define B.  The next assignments (S =) are accumulation
! steps of the list of matrix entries.

! Write adjacent columns of the A matrix, in NW corner of B:    
        s = dpTriplet(j,k,v)
        s = dpTriplet(j,k+1,1.D0-v)
! Write adjacent rows of the A^T matrix, in SE corner of B:
        s = dpTriplet(k+m,n+j,v)
        s = dpTriplet(k+m+1,n+j,1.D0-v)
! Write row of identity matrix I_M, in NE corner of B:
        s = dpTriplet(j,n+j,1.D0)
      END DO
! Use overloaded assigment to convert from a list
! of triplets. Create a Harwell-Boeing matrix
! representation for B.  This assignment (B =)
! converts the S list to a sparse matrix format.
      b = s

! Clear out space occupied by S.  This assignment
! deallocates the space used accumulating the
! list, S.
      s = 0

! Define the rest of the right-hand side.
      rhs(m+1:n+m) = 0.D0

! Solve for the coefficients of the piece-wise linear spline.
! This defined operation works with a Harwell-Boeing matrix
! and ascends B to be a component of an extended type, G.
! Default settings of pivoting rules and other parameters
! are used.
      x = b .ip. rhs
! Compute the residual.  The first M components of R
! are the same as the negative values of X(N+1:N+M).    
      r = b .p. x(1:n)
      r = r - rhs
      r(1:m) = r(1:m) + x(n+1:n+m)
      resid_error = dnrm2(m,r,1)/dnrm2(m,x(n+1),1)
      WRITE (*,'(A)') ' Data Fitting of y(t)=t**2, (0,1).'
      WRITE (*,'(A,2I10)') &
        ' The number of breakpoints (N) and data points (M) ', n, m
      WRITE (*,'(/A/(A,1PG12.5))') ' Relative Error (1) (Vector Norm)', &
        ' with solution and computed residual =', resid_error

! Ascend B, the Harwell-Boeing matrix, to the extended type G.
! The advantage of using .ip. on the extended type is that a
! factorization does not have to be recomputed after it is
! once available.  Options can also be reset from defaults.
      g%options%printstat = 1
      g = b
      x = g .ip. rhs
      x = 0.D0

! The factorization is available so only back solves now needed.     
      x = g .ip. rhs
      r = b .p. x(1:n)
      r = r - rhs
      r(1:m) = r(1:m) + x(n+1:n+m)
      resid_error = dnrm2(m,r,1)/dnrm2(m,x(n+1:),1)
      WRITE (*,'(A)') ' Repeated Data Fitting of y(t)=t**2, (0,1) - No Factorization -.'
      WRITE (*,'(A,2I10)') &
        ' The number of breakpoints (N) and data points (M) ', n, m
      WRITE (*,'(/A/(A,1PG12.5))') ' Relative Error (2) (Vector Norm)', &
        ' with solution and computed residual =', resid_error
! Free storage and clear matrix -
      g = 0
      b = 0
    END PROGRAM
