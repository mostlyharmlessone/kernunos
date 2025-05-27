    PROGRAM drivePiecewiseCubic

! Generate the coefficient matrix for a constrained least squares problem
! that comes from piece-wise cubic spline fitting of data with a
! continuous function.  The breakpoints (knots) are equally spaced.

! There are N unknowns in the problem and M data values.
! The N unknowns are the z values and z" second derivatives of the functions at the
! ends of the each breakpoint interval (the knots).

! The M data value are pairs (t_i, y(t_i)) where the t_i
! are random on (0,1).

! the design matric A (N x M) is such that A*z=y with constraint C^T*b=d as NxN constraints
! on the continuity of the first derivatives at the knots
! then B*[z,r]=[y,0]

! The matrix  B=[A^TA : C]
!               [C^T :  0] is first defined as a list of triplets.
! This matrix is assembled using overloaded assignment.
! B is then converted to Harwell-Boeing format using overloaded
! assignment between derived types.  The sparse matrix B has
! dimension (M+N) by (M+N).

      USE set_precision, ONLY: dkind
      USE sparseTypes, ONLY: dpTriplet, dpTripletList, dpCSRSparseMatrix, &
          dpHBSparseMatrix, slu_dpHBSparseMatrix
      USE sparseOps, ONLY: OPERATOR(.p.)
      USE sluInterop, ONLY: OPERATOR(.ip.), ASSIGNMENT(=) 
      USE sparseAssign, ONLY: ASSIGNMENT(=)
      USE lapackinterface, ONLY: dnrm2

      IMPLICIT NONE

! Real constants
      REAL(dkind), PARAMETER :: one=1.0E0_dkind, zero=0.0E0_dkind
! Set problem size:
      INTEGER, PARAMETER :: n=2000 ! Could make this an input value
! Define arrays for knots, data points, etc
      REAL(dkind), ALLOCATABLE :: a(:), rhs(:), t(:), x(:), r(:), z(:)
! iseed is used to store the seed used for the Fortran intrinsic
!       random number generator
! saw_points is used to ensure that every interval in the partition
!       contains at least one point
      INTEGER, ALLOCATABLE :: iseed(:), saw_points(:)
      INTEGER :: m, findInterval
! Define what will be the collection of matrix triplets.
      TYPE (dpTripletList) :: s
! Define the Harwell-Boeing derived type that holds the
! processed triplets.
      TYPE (dpHBSparseMatrix) :: b
! Define the ascended type that holds the Harwell-Boeing
! matrix and factorization quantities.
      TYPE (slu_dpHBSparseMatrix) :: g

! Define local variables
      REAL (dkind) :: delta, u, resid_error
      INTEGER :: errno, i, j, k, sz

      ALLOCATE (a(n), saw_points(n-1), STAT=errno)
      IF (errno /= 0) THEN
        print *, "Allocate fails with errno: ", errno
      END IF
      delta = one/real(n-1,dkind)
! Define the array of breakpoints
      a(1) = zero
      a(n) = one
! Define the knots or inner breakpoints
      DO i = 2, n - 1
        a(i) = a(i-1) + delta
      END DO

! Generate sufficient random values so that each interval
! has at least one value.  We do not store these data values
! here as we don't know the total number of data points required
! yet
      saw_points = 0
      m = 0
! Store the seed so we can regenerate the data points later
      CALL random_seed(size=sz)
      ALLOCATE (iseed(sz), STAT=errno)
      IF (errno /= 0) THEN
        print *, "Allocate fails with errno: ", errno
      END IF
      CALL random_seed(get=iseed)
      DO WHILE (any(saw_points==0))
        CALL random_number(u)
        k = findInterval(u, n, a)
        saw_points(k) = 1
        m = m + 1
      END DO
! Set random number seed so the same sequence results.
      CALL random_seed(put=iseed)

! Allocate local working space
      ALLOCATE (t(m),x(n+m),z(n+m),r(n+m),rhs(n+m), STAT=errno)
      IF (errno /= 0) THEN
        print *, "Allocate fails with errno: ", errno
      END IF
! Generate the random values and write the matrix entries.
! Record the function values in RHS(*).
      DO j = 1, m
        CALL random_number(t(j))
        z(j) = t(j)**2
      end do

      rhs(1)=0 ; rhs(m)=0 ; s = dpTriplet(1,n+1,one) ; s = dpTriplet(m,n+m,one)
      DO j = 2, m-1

!     ?replace with bsearch or vice-versa, ugly hack for k=1
        k = findInterval(t(j), n, a)
        if (k .eq. 1) k=2 !cycle doesn't work here

        rhs(j)=(z(j+1)-z(j))/(a(k+1)-t(j))-(z(j)-z(j-1))/(t(j)-a(k-1))

! Gather up the list of the sparse matrix triplets (S) that
! will define B.  The next assignments (S =) are accumulation
! steps of the list of matrix entries.
! Write adjacent columns of the A matrix, in NW corner of B:    

        s = dpTriplet(j,k-1,(t(j)-a(k-1))/6.0)
        s = dpTriplet(j,k,(a(k+1)-a(k-1))/3.0)
        s = dpTriplet(j,k+1,(a(k+1)-t(j))/6.0)

! Write adjacent rows of the A^T matrix, in SE corner of B:

        s = dpTriplet(k+m-1,n+j,(t(j)-a(k-1))/6.0)
        s = dpTriplet(k+m,n+j,(a(k+1)-a(k-1))/3.0)
        s = dpTriplet(k+m+1,n+j,(a(k+1)-t(j))/6.0)

! Write row of identity matrix I_M, in NE corner of B:
        s = dpTriplet(j,n+j,one)
      END DO
! Use overloaded assigment to convert from a list of
! triplets. Create a Harwell-Boeing matrix representation for B.
! This assignment (B =) converts the S list to a sparse matrix format.
      b = s
! Clear out space occupied by S.  This assignment deallocates
! the space used accumulating the list, S.
      s = 0
! Define the rest of the right-hand side.
      rhs(m+1:n+m) = zero
! Solve for the coefficients of the piece-wise cubic spline.
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
      x = zero

! The factorization is available so only back solves now needed.     
      x = g .ip. rhs
      r = b .p. x(1:n)
      r = r - rhs
      r(1:m) = r(1:m) + x(n+1:n+m)
      resid_error = dnrm2(m,r,1)/dnrm2(m,x(n+1:),1)

!      call SplineEval(0,x,y,y2,n,u,f,fp,fpp,fppp)
      !  x->a(k) (n values), y->z(j) (m values), y2->z2(k) (n values) at knots

      WRITE (*,'(A)') ' Repeated Data Fitting of y(t)=t**2, (0,1) - No Factorization -.'
      WRITE (*,'(A,2I10)') &
        ' The number of breakpoints (N) and data points (M) ', n, m
      WRITE (*,'(/A/(A,1PG12.5))') ' Relative Error (2) (Vector Norm)', &
        ' with solution and computed residual =', resid_error
! Free storage and clear matrix
      g = 0
      b = 0
    END PROGRAM

    FUNCTION findInterval(u, n, a) RESULT(k)
    USE set_precision, ONLY: dkind
    INTEGER, INTENT(IN) :: n
    REAL(dkind), INTENT(IN) :: u, a(n)
    REAL(dkind) :: v, delta
    REAL(dkind), PARAMETER :: one = 1.0E0_dkind
    INTEGER :: k

! This direct computation of the interval containing
! the data can be off (low) by 1.  So if the value
! of the basis function is > 1, move to the next interval.
    delta = one/real(n-1,dkind)
    k = max(1,min(n-1,floor(real(n*u,dkind))))
    DO
      v = (u-a(k))/delta
      IF (v<=one .OR. k==n-1) EXIT
! Move to the next interval.        
      k = k + 1
    END DO

    END FUNCTION findInterval
