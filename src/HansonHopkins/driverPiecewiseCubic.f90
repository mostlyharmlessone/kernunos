    PROGRAM drivePiecewiseCubic

! Adapted from H&H drivePiecewiseLinear

! Generate the coefficient matrix for a equality-constrained least squares problem
! that comes from piece-wise cubic spline fitting of data with a
! continuous function.  The breakpoints (knots) are equally spaced.

! There are 2N unknowns at N knots in the problem and M data values.
! The 2N unknowns are the N z values and N z" second derivatives of the functions at the
! ends of the each breakpoint interval (the knots); the natural spline condition sets the end points = 0,
! removing 2 of the unknowns and making two constraints trivial; in the periodic case, there are 2N constraints and 2N unknowns

! The M data value are pairs (t_i, y(t_i)) where the t_i
! are random on (0,1).

! the y(t) is either y=x^2 or sin(4*3.14*x) selected by commenting one out; by default the sine function is active (lines 166/490/519)
! the periodic bc are obviously not suitable for the parabola, as the end conditions resemble a step function.
! Since the second derivative of the sine is zero at the ends,the natural spline or periodic conditions work equally well

! the design matric A (Mx2N) is for the LSQ solution to A*x=b (M data points) with constraint C*(z,z")=d as
! as either (2N x N)  constraints on the continuity of the first derivatives at the knots, h are the Lagrange multipliers
! then x == (z sub j,z" sub j) alternating.  b = y(t) here and d = 0 with these constraints

! The constraint on the first derivative can be periodic so that the 1st derivatives at the last knot and first knot are continuous
! or natural, in which case the second derivative at the ends is zero

! The matrix  B=[A^TA : C^T][ x ]  = [A^T*b] 2N
!               [ C   :  0 ][ h ]    [  d  ] N
!                 2N     N

! The matrix B has dimension (3N x 3N) but has the possibly ill-conditioned Gram product ATA.
! The constraint is C*y=d with h Lagrange multipliers

! The extension of H&H to avoid the normal equations is using
! The matrix B with dimension (M+3N)x(M+3N) and r residuals is

! The matrix  B= [A : I_M :  0 ][ x ]   [ b ] M
!                [0 : A^T : C^T][ r ] = [ 0 ] 2N
!                [C :  0  :  0 ][ h ]   [ d ] N
!                 2N   M     N

! which can be rearranged for symmetry as (Amy Tabb referencing Matrix Computations, Gene H. Golub and Charles F. Van Loan. 4th edition, 2013 ISBN 9781421407944.)
! https://amytabb.com/tips/2022/02/27/least-squares-with-equality-constraints/ although it is not here so arranged

! The matrix  B= [0 : A^T : C^T ][ x ]   [ 0 ] 2N
!                [A : I_M :  0  ][ r ] = [ b ] M
!                [C :  0  :  0  ][ h ]   [ d ] N
!                 2N    M     N


      USE set_precision, ONLY: dkind
      USE sparseTypes, ONLY: dpTriplet, dpTripletList, dpCSRSparseMatrix, &
          dpHBSparseMatrix, slu_dpHBSparseMatrix
      USE sparseOps, ONLY: OPERATOR(.p.), OPERATOR(.t.)
      USE sluInterop, ONLY: OPERATOR(.ip.), ASSIGNMENT(=) 
      USE sparseAssign, ONLY: ASSIGNMENT(=)
      USE lapackinterface, ONLY: dnrm2, dgesv

      IMPLICIT NONE

      INTERFACE
       FUNCTION findInterval(u, n, a, periodic) RESULT(k)
       USE set_precision, ONLY: dkind
       INTEGER, INTENT(IN) :: n
       REAL(dkind), INTENT(IN) :: u, a(*)
       LOGICAL, INTENT(IN) :: periodic
       END FUNCTION findInterval
      END INTERFACE

! Real constants
      REAL(dkind), PARAMETER :: one=1.0E0_dkind, zero=0.0E0_dkind
! Set problem size:
      INTEGER, PARAMETER :: n=20 !n=2000 ! Could make this an input value
! periodic = true means periodic bc, false, natural spline conditions
! csr = true means using the Gram product ATA and the 3n x 3n system, false means using the H&H M+3*n system
! sparse = true means using superlu to solve the system, false means converting to a dense matrix and using LAPACK
! obviously sparse = true is suitable for large n
      LOGICAL, PARAMETER :: periodic = .false., csr = .true., sparse = .true. ! Could make these an input value
! Define arrays for knots, data points, etc
      REAL(dkind), ALLOCATABLE :: a(:), rhs(:), t(:), x(:), r(:), rhs_m(:)
! iseed is used to store the seed used for the Fortran intrinsic
!       random number generator
! saw_points is used to ensure that every interval in the partition
!       contains at least one point
      INTEGER, ALLOCATABLE :: iseed(:), saw_points(:)
      INTEGER :: m !, findInterval
! Define what will be the collection of matrix triplets.
      TYPE (dpTripletList) :: s
! Define what will be the CSR version for the design matrix
      TYPE (dpCSRSparseMatrix) :: a_csr
! Define what will be the CSR version of A^TA for the design matrix
      TYPE (dpCSRSparseMatrix) :: ata_csr
! Define some  triplets
      TYPE (dpTriplet), ALLOCATABLE :: triplets(:)
! Define variables for LAPACK and printing results
      REAL(dkind), ALLOCATABLE :: dense(:,:),d(:)
      INTEGER, allocatable :: ipiv(:)
      INTEGER :: info
      REAL(dkind) :: sumsq
! Define the Harwell-Boeing derived type that holds the
! processed triplets.
      TYPE (dpHBSparseMatrix) :: b
! Define the ascended type that holds the Harwell-Boeing
! matrix and factorization quantities.
      TYPE (slu_dpHBSparseMatrix) :: g

! Modified to allow n intervals after each break point including the last one for the periodic case
! Define local variables
      REAL (dkind) :: delta, v, u, resid_error
      INTEGER :: errno, i, j, k, sz
      if (periodic) then
       ALLOCATE (a(n), saw_points(n), STAT=errno)
       IF (errno /= 0) THEN
        print *, "Allocate fails with errno: ", errno
       END IF
       delta = one/real(n,dkind)
! Define the array of breakpoints
       a(1) = zero
       a(n) = one-delta
        DO i = 2, n - 1
         a(i) = a(i-1) + delta
       END DO
      else
       ALLOCATE (a(n), saw_points(n-1), STAT=errno)
       IF (errno /= 0) THEN
         print *, "Allocate fails with errno: ", errno
       END IF
       delta = one/real(n-1,dkind)
! Define the array of breakpoints
       a(1) = zero
       a(n) = one
        DO i = 2, n - 1
         a(i) = a(i-1) + delta
       END DO
      endif

! Generate sufficient random values so that each interval
! has at least one value.  We do not store these data values
! here as we don't know the total number of data points required yet
! note for the periodic case we need a value in the n to 1 interval so that k can actually reach n
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
        k = findInterval(u, n, a, periodic)
        saw_points(k) = 1
        m = m + 1
      END DO
! Set random number seed so the same sequence results.
      CALL random_seed(put=iseed)
! Allocate local working space
     if (csr) then
      ALLOCATE (t(m),x(3*n),r(3*n),rhs_m(m),rhs(3*n),d(3*n),ipiv(3*n), STAT=errno)
     else
      ALLOCATE (t(m),x(3*n+m),r(3*n+m),rhs_m(m),rhs(3*n+m),d(3*n+m),ipiv(3*n+m), STAT=errno)
      IF (errno /= 0) THEN
        print *, "Allocate fails with errno: ", errno
      END IF
     endif
! initialize
     rhs(:) = zero
! Generate the random values and write the matrix entries.
! Record the function values in RHS(*).
     DO j = 1, m     ! j = data points
        CALL random_number(u)
         t(j) = u
        rhs_m(j) = sin(4*3.14*t(j))
!        rhs_m(j) = t(j)**2
        k = findInterval(t(j), n, a, periodic)
        v = one-(t(j)-a(k))/delta
! Gather up the list of the sparse matrix triplets (S) that
! will define B.  The next assignments (S =) are accumulation
! steps of the list of matrix entries.
! Write adjacent columns of the (design) A matrix, in NW corner of B:
! Starts at 1st row and column, m rows, 2*n columns; odd is z, even z''
      if (csr) then
! only write A for CSRSparse to make ATA
        s = dpTriplet(j,2*k-1,v)
        s = dpTriplet(j,2*k, delta*delta*v*(v*v-one)/6.0 )
        if (k .lt. n) then
         s = dpTriplet(j,2*k+1,one-v)
         s = dpTriplet(j,2*k+2,delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0 )
        endif
 ! For H&H approach write A, A^T and I_M
      else
        s = dpTriplet(j,2*k-1,v)
        s = dpTriplet(j,2*k, delta*delta*v*(v*v-one)/6.0 )
        if (k .lt. n) then
         s = dpTriplet(j,2*k+1,one-v)
         s = dpTriplet(j,2*k+2,delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0 )
        endif
! Write adjacent rows of the A^T matrix, in middle of B:
! Starts at m+1st row and 2*n+1st column, 2*n rows, m columns
        s = dpTriplet(2*k-1+m,2*n+j,v)
        s = dpTriplet(2*k+m,2*n+j, delta*delta*v*(v*v-one)/6.0 )
        if (k .lt. n) then
         s = dpTriplet(2*k+1+m,2*n+j,one-v)
         s = dpTriplet(2*k+2+m,2*n+j,delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0 )
        endif
! Write row of identity matrix I_M, top middle of B:
        s = dpTriplet(j,2*n+j,one)
      endif
     END DO

     if (csr) then
! CSR version
! Use overloaded assigment to convert from a list of
! triplets. Create a Compressed Sparse Row matrix representation for A, A^T.
        a_csr = s
        s = 0
! Create A^TA by multiplication of CSR sparse matrices
        ata_csr = (.t. a_csr) .p. a_csr
! Make triplets from ata_csr
        triplets = ata_csr
        s = triplets
! Create A^T*y
       rhs(1:2*n) = (.t. a_csr) .p. rhs_m(:)
      else
       rhs(1:m)=rhs_m(:)
      endif

! Constraints are not dependent on data j or m
      if (csr) then
! The matrix  B=[A^TA : C^T][ x ]  = [A^T*b] 2N
!               [ C   :  0 ][ h ]    [  d  ] N
!                 2N     N
! CSR versions start at 2n+1 column and 1st row for C^T, 2n rows, n columns
! Write adjacent columns of the C^T matrix, along NE border of B:
! two versions depending on periodicity
      DO k=1,n
        IF ( k .eq. 1 ) THEN
         IF (periodic) then
          s = dpTriplet(2*k-1,k+n+n,    2.0 )
          s = dpTriplet(2*k,k+n+n,  2.0*delta*delta/3.0 )
          s = dpTriplet(2*k+1,k+n+n,  -1.0)
          s = dpTriplet(2*k+2,k+n+n,  delta*delta/6.0)
! periodic terms
          s = dpTriplet(2*n-1,k+n+n,  -1.0)
          s = dpTriplet(2*n,k+n+n,  delta*delta/6.0 )
         ELSE
! terms here for z" == 0
          s = dpTriplet(2*k,k+n+n,    1.0 )
         ENDIF
        ENDIF
        IF ( k .gt. 1 .and. k .lt. n  ) THEN
         s = dpTriplet(2*k-3,k+n+n,  -1.0)
         s = dpTriplet(2*k-2,k+n+n,  delta*delta/6.0 )
         s = dpTriplet(2*k-1,k+n+n,    2.0 )
         s = dpTriplet(2*k,k+n+n,  2.0*delta*delta/3.0 )
         s = dpTriplet(2*k+1,k+n+n,  -1.0)
         s = dpTriplet(2*k+2,k+n+n,  delta*delta/6.0)
        ENDIF
! last column of C^T, two versions depending on periodicity
        IF ( k .eq. n ) THEN
         IF (periodic) then
          s = dpTriplet(2*k-3,k+n+n,  -1.0)
          s = dpTriplet(2*k-2,k+n+n,  delta*delta/6.0 )
          s = dpTriplet(2*k-1,k+n+n,     2.0 )
          s = dpTriplet(2*k,k+n+n,  2.0*delta*delta/3.0 )
! periodic terms
          s = dpTriplet(1,k+n+n, -1.0 )
          s = dpTriplet(2,k+n+n, delta*delta/6.0 )
         ELSE
! terms here for z" == 0
          s = dpTriplet(2*k,k+n+n,    1.0 )
         ENDIF
        ENDIF
! Write rows of the (constraint) C matrix, in SW corner of B:
! CSR versions start at 2n+1 row and 1st column for C, n rows, 2n columns
! These are the constraints on the slopes
        IF ( k .eq. 1 ) THEN
         IF (periodic) then
          s = dpTriplet(k+n+n,2*k-1,    2.0 )
          s = dpTriplet(k+n+n,2*k,  2.0*delta*delta/3.0 )
          s = dpTriplet(k+n+n,2*k+1,  -1.0)
          s = dpTriplet(k+n+n,2*k+2,  delta*delta/6.0)
! periodic terms
          s = dpTriplet(k+n+n,2*n-1,  -1.0)
          s = dpTriplet(k+n+n,2*n,  delta*delta/6.0 )
         ELSE
! terms here for z" == 0
          s = dpTriplet(k+n+n,2*k,   1.0 )
         ENDIF
        ENDIF
        IF ( k .gt. 1 .and. k .lt. n  ) THEN
         s = dpTriplet(k+n+n,2*k-3,  -1.0)
         s = dpTriplet(k+n+n,2*k-2,  delta*delta/6.0 )
         s = dpTriplet(k+n+n,2*k-1,    2.0 )
         s = dpTriplet(k+n+n,2*k,  2.0*delta*delta/3.0 )
         s = dpTriplet(k+n+n,2*k+1,  -1.0)
         s = dpTriplet(k+n+n,2*k+2,  delta*delta/6.0)
        ENDIF
! last row of C, two versions depending on periodicity
        IF ( k .eq. n ) THEN
         IF (periodic) then
          s = dpTriplet(k+n+n,2*k-3,  -1.0)
          s = dpTriplet(k+n+n,2*k-2,  delta*delta/6.0 )
          s = dpTriplet(k+n+n,2*k-1,     2.0 )
          s = dpTriplet(k+n+n,2*k,  2.0*delta*delta/3.0 )
! periodic terms
          s = dpTriplet(k+n+n,1, -1.0 )
          s = dpTriplet(k+n+n,2, delta*delta/6.0 )
         ELSE
! terms here for z" == 0
          s = dpTriplet(k+n+n,2*k,   1.0 )
         ENDIF
        ENDIF
      END DO

      else

! NOT the CSR version
! The matrix  B= [A : I_M :  0 ][ x ]   [ b ] M
!                [0 : A^T : C^T][ r ] = [ 0 ] 2N
!                [C :  0  :  0 ][ h ]   [ d ] N
!                 2N   M     N
      DO k=1,n
! Write adjacent columns of the C^T matrix, along middle right/E border of B:
! Starts at m+1 row and m+2n+1 column; 2n rows, n columns
! first column of C^T, two versions depending on periodicity
        IF ( k .eq. 1 ) THEN
         IF (periodic) then
          s = dpTriplet(2*k-1+m,k+m+n+n,    2.0 )
          s = dpTriplet(2*k+m,k+m+n+n,  2.0*delta*delta/3.0 )
          s = dpTriplet(2*k+1+m,k+m+n+n,  -1.0)
          s = dpTriplet(2*k+2+m,k+m+n+n,  delta*delta/6.0)
! periodic terms
          s = dpTriplet(2*n-1+m,k+m+n+n,  -1.0)
          s = dpTriplet(2*n+m,k+m+n+n,  delta*delta/6.0 )
         ELSE
! terms here for z" == 0
          s = dpTriplet(2*k+m,k+m+n+n,    1.0 )
         ENDIF
        ENDIF
        IF ( k .gt. 1 .and. k .lt. n  ) THEN
         s = dpTriplet(2*k-3+m,k+m+n+n,  -1.0)
         s = dpTriplet(2*k-2+m,k+m+n+n,  delta*delta/6.0 )
         s = dpTriplet(2*k-1+m,k+m+n+n,    2.0 )
         s = dpTriplet(2*k+m,k+m+n+n,  2.0*delta*delta/3.0 )
         s = dpTriplet(2*k+1+m,k+m+n+n,  -1.0)
         s = dpTriplet(2*k+2+m,k+m+n+n,  delta*delta/6.0)
        ENDIF
! last column of C^T, two versions depending on periodicity
        IF ( k .eq. n ) THEN
         IF (periodic) then
          s = dpTriplet(2*k-3+m,k+m+n+n,  -1.0)
          s = dpTriplet(2*k-2+m,k+m+n+n,  delta*delta/6.0 )
          s = dpTriplet(2*k-1+m,k+m+n+n,     2.0 )
          s = dpTriplet(2*k+m,k+m+n+n,  2.0*delta*delta/3.0 )
! periodic terms
          s = dpTriplet(1+m,k+m+n+n, -1.0 )
          s = dpTriplet(2+m,k+m+n+n, delta*delta/6.0 )
         ELSE
! terms here for z" == 0
          s = dpTriplet(2*k+m,k+m+n+n,    1.0 )
         ENDIF
        ENDIF
! Write rows of the (constraint) C matrix, in SW corner of B:
! Starts at m+2n+1 rows, 1st column, n rows, 2*n columns
! first row of C, two versions depending on periodicity
! These are the constraints on the slopes
        IF ( k .eq. 1 ) THEN
         IF (periodic) then
          s = dpTriplet(k+m+n+n,2*k-1,    2.0 )
          s = dpTriplet(k+m+n+n,2*k,  2.0*delta*delta/3.0 )
          s = dpTriplet(k+m+n+n,2*k+1,  -1.0)
          s = dpTriplet(k+m+n+n,2*k+2,  delta*delta/6.0)
! periodic terms
          s = dpTriplet(k+m+n+n,2*n-1,  -1.0)
          s = dpTriplet(k+m+n+n,2*n,  delta*delta/6.0 )
         ELSE
! terms here for z" == 0
          s = dpTriplet(k+m+n+n,2*k,   1.0 )
         ENDIF
        ENDIF
        IF ( k .gt. 1 .and. k .lt. n  ) THEN
         s = dpTriplet(k+m+n+n,2*k-3,  -1.0)
         s = dpTriplet(k+m+n+n,2*k-2,  delta*delta/6.0 )
         s = dpTriplet(k+m+n+n,2*k-1,    2.0 )
         s = dpTriplet(k+m+n+n,2*k,  2.0*delta*delta/3.0 )
         s = dpTriplet(k+m+n+n,2*k+1,  -1.0)
         s = dpTriplet(k+m+n+n,2*k+2,  delta*delta/6.0)
        ENDIF
! last row of C, two versions depending on periodicity
        IF ( k .eq. n ) THEN
         IF (periodic) then
          s = dpTriplet(k+m+n+n,2*k-3,  -1.0)
          s = dpTriplet(k+m+n+n,2*k-2,  delta*delta/6.0 )
          s = dpTriplet(k+m+n+n,2*k-1,     2.0 )
          s = dpTriplet(k+m+n+n,2*k,  2.0*delta*delta/3.0 )
! periodic terms
          s = dpTriplet(k+m+n+n,1, -1.0 )
          s = dpTriplet(k+m+n+n,2, delta*delta/6.0 )
         ELSE
! terms here for z" == 0
          s = dpTriplet(k+m+n+n,2*k,   1.0 )
         ENDIF
        ENDIF
       END DO

      endif

!Use superlu to solve sparse systems
if (sparse) then

! Use overloaded assigment to convert from a list of
! triplets. Create a Harwell-Boeing matrix representation for B.
! This assignment (B =) converts the S list to a sparse matrix format.
      b = s

! Clear out space occupied by S.  This assignment deallocates
! the space used accumulating the list, S.
      s = 0

! Solve for the coefficients of the piece-wise cubic spline.
! This defined operation works with a Harwell-Boeing matrix
! and ascends B to be a component of an extended type, G.
! Default settings of pivoting rules and other parameters
! are used.
     x = b .ip. rhs
     if (csr) then
      write(*,*) 'Using csr: no residual calculation'
     else
! Compute the residual.  The first M components of R
! are the same as the negative values of X(2N+1:2N+M).
      r = b .p. x(1:2*n)
      r = r - rhs(1:size(r))
      r(1:m) = r(1:m) + x(2*n+1:2*n+m)
      resid_error = dnrm2(m,r,1)/dnrm2(m,x(2*n+1),1)
      WRITE (*,'(A)') ' Data Fitting of y(t)=t**2, (0,1).'
      WRITE (*,'(A,2I10)') &
        ' The number of breakpoints (N) and data points (M) ', n, m
      WRITE (*,'(/A/(A,1PG12.5))') ' Relative Error (1) (Vector Norm)', &
        ' with solution and computed residual =', resid_error
     endif

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
     if (csr) then
      write(*,*) 'Using csr: no residual calculation'
     else
      r = b .p. x(1:2*n)
      r = r - rhs(1:size(r))
      r(1:m) = r(1:m) + x(2*n+1:2*n+m)
      resid_error = dnrm2(m,r,1)/dnrm2(m,x(2*n+1:),1)

      WRITE (*,'(A)') ' Repeated Data Fitting of y(t)=t**2, (0,1) - No Factorization -.'
      WRITE (*,'(A,2I10)') &
        ' The number of breakpoints (N) and data points (M) ', n, m
      WRITE (*,'(/A/(A,1PG12.5))') ' Relative Error (2) (Vector Norm)', &
        ' with solution and computed residual =', resid_error
     endif

! Free storage and clear matrix
      g = 0
      b = 0

else

! Use LAPACK not superlu
  a_csr = 0
  a_csr=s
  dense = a_csr
  d(:)=rhs(:)    ! d gets overwritten
  call DGESV(size(dense,1), 1, dense, size(dense,1), IPIV, d, size(dense,1), info ) ! d is overwritten
  write(*,*) 'info from LAPACK',info
  if (info .ne. 0) stop
   x(:) = d(:)

endif

! x are the coefficients so y=x(i)v+x(i+1)(1-v); at knots v=1 so x(i)==y(i) for linear
! x are the coefficients so y=x(2+i-1)v+x(2+i)*delta*delta*v*(v*v-one)/6.0
!                            +x(2*i+1)(1-v)+x(2*i+2)*delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0
!                          at knots v=1 so x(2*i-1)==y(i) for cubic

! so comparison of fit to data x(k) to computed actual data at knots (which you don't get as data) is

sumsq = 0
write(*,*) 'a(k),x(2*k-1),y(k),x(2*k-1)-y(a(k))'
 do k=1,n
!   write(*,*) a(k),x(2*k-1),a(k)**2,x(2*k-1)-a(k)**2
!   sumsq=sumsq+(x(2*k-1)-a(k)**2)**2
   write(*,*) a(k),x(2*k-1),sin(4*3.14*a(k)),x(2*k-1)-sin(4*3.14*a(k))
   sumsq=sumsq+(x(2*k-1)-sin(4*3.14*a(k)))**2
 end do
   sumsq=sqrt(sumsq)/n
write(*,*) 'Sum Squared',sumsq

! print to plot with gnuplot eg.

!do j=1,1001
!z = (j-1)/1000.0
!k = findInterval(z, n, a, periodic)
!        IF ( k .gt. 1 .and. k .lt. n  ) THEN
!        zs = -1.0*x(2*k-3) + x(2*k-2)*delta*delta/6.0 + 2.0*x(2*k-1) + &
!             2.0*x(2*k)*delta*delta/3.0 - 1.0*x(2*k+1) + x(2*k+2)*delta*delta/6.0  !should be zero
!        else
!        if (k .eq. 1) then
!         zs = -1.0*x(2*n-1) + x(2*n)*delta*delta/6.0 + 2.0*x(2*k-1) + &
!             2.0*x(2*k)*delta*delta/3.0 - 1.0*x(2*k+1) + x(2*k+2)*delta*delta/6.0  !should be zero
!         else
!         zs = -1.0*x(2*k-3) + x(2*k-2)*delta*delta/6.0 + 2.0*x(2*k-1) + &
!             2.0*x(2*k)*delta*delta/3.0 - 1.0*x(1) + x(2)*delta*delta/6.0  !should be zero
!         endif
!        ENDIF
!v = one-(z-a(k))/delta
!y = x(2*k-1)*v + x(2+k)*delta*delta*v*(v*v-one)/6.0 &
!  + x(2*k+1)*(1-v) + x(2*k+2)*delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0
!y2 = x(2*k)*v + x(2*k+2)*(1-v)

! write(*,*) z,z**2,y,y-z**2,zs,2,y2,k,a(k)
! write(*,*) z,sin(4*3.14*z),y,y-sin(4*3.14*z),zs,y2,-4*3.14*4*3.14*sin(4*3.14*z),k,a(k)

!end do

    END PROGRAM

    FUNCTION findInterval(u, n, a, periodic) RESULT(k)
    USE set_precision, ONLY: dkind
    INTEGER, INTENT(IN) :: n
    REAL(dkind), INTENT(IN) :: u, a(*)
    LOGICAL, INTENT(IN) :: periodic
    REAL(dkind) :: v, delta
    REAL(dkind), PARAMETER :: one = 1.0E0_dkind
    INTEGER :: k
! Modified to allow n intervals after each break point including the last one for periodic case
! This direct computation of the interval containing
! the data can be off (low) by 1.  So if the value
! of the basis function is > 1, move to the next interval.
    if (periodic) then
      delta = one/real(n,dkind)
    else
      delta = one/real(n-1,dkind)
    endif
    k = max(1,min(n,floor(real(n*u,dkind))))
    DO
      v = (u-a(k))/delta
      IF (v<=one .OR. k==n) EXIT
! Move to the next interval.        
      k = k + 1
    END DO

    END FUNCTION findInterval
