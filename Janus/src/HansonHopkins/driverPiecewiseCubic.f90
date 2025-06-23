    PROGRAM drivePiecewiseCubic

! Generate the coefficient matrix for a equality-constrained least squares problem
! that comes from piece-wise cubic spline fitting of data with a
! continuous function.  The breakpoints (knots) are equally spaced.

! There are 2N unknowns at N knots in the problem and M data values.
! The 2N unknowns are the N z values and N z" second derivatives of the functions at the
! ends of the each breakpoint interval (the knots).

! The M data value are pairs (t_i, y(t_i)) where the t_i
! are random on (0,1).

! the design matric A (Mx2N) is for the LSQ solution to A*y=b (M data points) with constraint C*(z,z")=d as (2N x N) constraints
! on the continuity of the first derivatives at the knots, h are the Lagrange multipliers
! then y == (z sub j,z" sub j) alternating.

! The matrix  B=[A^TA : C^T][ y ]  = [A^T*b] 2N
!               [ C   :  0 ][ h ]    [  d  ] N
!                 2N     N

! The matrix B has dimension (3N x 3N) but has the possibly ill-conditioned Gram product ATA.

! The extension of H&H to avoid the normal equations is using the (M+3N)x(M+3N) system

! The matrix  B= [A : I_M :  0 ][ y ]   [ b ] M
!                [0 : A^T : C^T][ r ] = [ 0 ] 2N
!                [C :  0  :  0 ][ h ]   [ d ] N
!                 2N   M     N
! which adds the constraint C*y=d, with h Lagrange multipliers
! which can be rearranged for symmetry as (Amy Tabb referencing Matrix Computations, Gene H. Golub and Charles F. Van Loan. 4th edition, 2013 ISBN 9781421407944.)

! The matrix  B= [0 : A^T : C^T ][ y ]   [ 0 ] 2N
!                [A : I_M :  0  ][ r ] = [ b ] M
!                [C :  0  :  0  ][ h ]   [ d ] N
!                 2N    M     N

! The matrix B has dimension (2N+M+N x 2N+M+N)

      USE set_precision, ONLY: dkind
      USE sparseTypes, ONLY: dpTriplet, dpTripletList, dpCSRSparseMatrix, &
          dpHBSparseMatrix, slu_dpHBSparseMatrix
      USE sparseOps, ONLY: OPERATOR(.p.), OPERATOR(.t.)
      USE sluInterop, ONLY: OPERATOR(.ip.), ASSIGNMENT(=) 
      USE sparseAssign, ONLY: ASSIGNMENT(=)
      USE lapackinterface, ONLY: dnrm2
      USE sparseUtils

      IMPLICIT NONE

! Real constants
      REAL(dkind), PARAMETER :: one=1.0E0_dkind, zero=0.0E0_dkind
! Set problem size:
      INTEGER, PARAMETER :: n=10 !n=2000 ! Could make this an input value
! Define arrays for knots, data points, etc
      REAL(dkind), ALLOCATABLE :: a(:), rhs(:), t(:), x(:), r(:), y(:)
! iseed is used to store the seed used for the Fortran intrinsic
!       random number generator
! saw_points is used to ensure that every interval in the partition
!       contains at least one point
      INTEGER, ALLOCATABLE :: iseed(:), saw_points(:)
      INTEGER :: m, findInterval
! Define what will be the collection of matrix triplets.
      TYPE (dpTripletList) :: s
! Define what will be the collection of matrix triplets for the design matrix
      TYPE (dpTripletList) :: acbd
! Define what will be the transposed collection of matrix triplets for the design matrix
      TYPE (dpTripletList) :: acbdT
! Define what will be the CSR version for the design matrix
      TYPE (dpCSRSparseMatrix) :: acbd_csr
! Define what will be the transposed CSR version for the design matrix
      TYPE (dpCSRSparseMatrix) :: acbdT_csr
! Define what will be the CSR version of A^TA for the design matrix
      TYPE (dpCSRSparseMatrix) :: ata_csr
! Define what will be the collection of matrix triplets.
      TYPE (dpTriplet), ALLOCATABLE :: trip_array(:)

! Define the Harwell-Boeing derived type that holds the
! processed triplets.
      TYPE (dpHBSparseMatrix) :: b
! Define the ascended type that holds the Harwell-Boeing
! matrix and factorization quantities.
      TYPE (slu_dpHBSparseMatrix) :: g

! Define local variables
      REAL (dkind) :: delta, v, u, resid_error
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


!! copied from Linear
! Allocate local working space
      ALLOCATE (t(m),x(n+m),r(n+m),rhs(n+m), STAT=errno)
      IF (errno /= 0) THEN
        print *, "Allocate fails with errno: ", errno
      END IF
! Generate the random values and write the matrix entries.
! Record the function values in RHS(*).
      DO j = 1, m
        CALL random_number(t(j))
        rhs(j) = t(j)**2
        k = findInterval(t(j), n, a)
        v = (t(j)-a(k))/delta
! Gather up the list of the sparse matrix triplets (S) that
! will define B.  The next assignments (S =) are accumulation
! steps of the list of matrix entries.
! Write adjacent columns of the A matrix, in NW corner of B:
        s = dpTriplet(j,k,v)
        s = dpTriplet(j,k+1,one-v)
! copy for test of CSRSparse; only need A
        s_test = dpTriplet(j,k,v)
        s_test = dpTriplet(j,k+1,one-v)
! Write adjacent rows of the A^T matrix, in SE corner of B:
        s = dpTriplet(k+m,n+j,v)
        s = dpTriplet(k+m+1,n+j,one-v)
! Write row of identity matrix I_M, in NE corner of B:
        s = dpTriplet(j,n+j,one)
      END DO

! Define the rest of the right-hand side.
      rhs(m+1:n+m) = zero





! Design matrix 2NxM equations

      DO j = 1, m

        k = findInterval(t(j), n, a)
! from linear version, needs work
        rhs(j) = t(j)**2
        k = findInterval(t(j), n, a)
        v = (t(j)-a(k))/delta
! Write adjacent columns of the A matrix, in NW corner of B:
        acbd = dpTriplet(j,k,v)
        acbd = dpTriplet(j,k+1,one-v)
! Write adjacent rows of the A^T matrix, in SE corner of B:
        acbdT = dpTriplet(k+m,n+j,v)
        acbdT = dpTriplet(k+m+1,n+j,one-v)
! can test with acbdT should be identical to (.t. acbd) if

      END DO

! Use overloaded assigment to convert from a list of
! triplets. Create a Compressed Sparse Row matrix representation for A, A^T.
        acbd_csr = acbd
        acbdT_csr = acbdT
! Create A^TA by multiplication of CSR sparse matrices
        ata_csr = acbdT_csr .p. acbd_csr
! Create A^T*y
        rhs(1:2*n) = acbdT_csr .p. y(1:m)
! Create triplets corresponding to A^TA
        trip_array = ata_csr
! Create tripletlist
        s = trip_array

! Constraints, need to rewrite in terms of z, z" at knots NxN equations
  s = dpTriplet(1,n+1,one) ; s = dpTriplet(m,n+m,one)

DO j = 2, m-1

!     ?replace with bsearch or vice-versa, ugly hack for k=1 for periodic version
  k = findInterval(t(j), n, a)
  if (k .eq. 1) k=2 !cycle doesn't work here

!  constraint "d" = 0, no addition to rhs(:)

! Gather up the list of the sparse matrix triplets (S) that
! will define C^T.  The next assignments (S =) are accumulation
! steps of the list of matrix entries.
! Write adjacent columns of the C^T matrix, in NW corner of B:

  s = dpTriplet(j,k-1,(t(j)-a(k-1))/6.0)
  s = dpTriplet(j,k,(a(k+1)-a(k-1))/3.0)
  s = dpTriplet(j,k+1,(a(k+1)-t(j))/6.0)

! Write adjacent rows of the C matrix, in SE corner of B:

  s = dpTriplet(k+m-1,n+j,(t(j)-a(k-1))/6.0)
  s = dpTriplet(k+m,n+j,(a(k+1)-a(k-1))/3.0)
  s = dpTriplet(k+m+1,n+j,(a(k+1)-t(j))/6.0)

END DO




! Use overloaded assigment to convert from a list of
! triplets. Create a Harwell-Boeing matrix representation for B.
! This assignment (B =) converts the S list to a sparse matrix format.
      b = s
! Clear out space occupied by S.  This assignment deallocates
! the space used accumulating the list, S.
      s = 0
! Define the rest of the right-hand side.
      rhs(2*n+1:3*n) = zero
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
