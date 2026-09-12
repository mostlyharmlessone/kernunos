    PROGRAM drivePiecewiseLinear
! Modified to test use of normal equations/Gram matrix, test of CSR sparse matrix routines and comparison with dense matrix lapack solution
! Generate the coefficient matrix for a least squares problem
! that comes from piece-wise linear fitting of data with a
! continuous function.  The breakpoints are equally spaced.  

! There are N unknowns in the problem and M data values.
! The N unknowns are the values of the linear functions at the
! ends of the each breakpoint interval.

! The M data value are pairs (t_i, y(t_i)) where the t_i
! are random on (0,1).

!                 N     M
! The matrix  B=[ A  : I_M][ x ]  =  [y]  M
!               [0_N : A^T][ r ]     [0]  N

! is first defined as a list of triplets.
! This matrix is assembled using overloaded assignment.
! B is then converted to Harwell-Boeing format using overloaded
! assignment between derived types.  The sparse matrix B has
! dimension (M+N) by (M+N).

! Can alternatively solve normal equations
! [A^TA ][ x ]  = [A^T*y] N x N
! followed by solution with LAPACK DGESV or use of superlu, as A^TA is also sparse.

      USE set_precision, ONLY: dkind
      USE sparseTypes, ONLY: dpTriplet, dpTripletList, dpCSRSparseMatrix, &
                dpHBSparseMatrix, slu_dpHBSparseMatrix
      USE sparseOps, ONLY: OPERATOR(.p.), OPERATOR(.t.)
      USE sluInterop, ONLY: OPERATOR(.ip.), ASSIGNMENT(=) 
      USE sparseAssign !, ONLY: ASSIGNMENT(=)
      USE lapackinterface, ONLY: dnrm2, dgesv

      IMPLICIT NONE

      INTERFACE
       FUNCTION findInterval(u, n, a) RESULT(k)
       USE set_precision, ONLY: dkind
       INTEGER, INTENT(IN) :: n
       REAL(dkind), INTENT(IN) :: u, a(n)
       END FUNCTION findInterval
      END INTERFACE

! Real constants
      REAL(dkind), PARAMETER :: one=1.0E0_dkind, zero=0.0E0_dkind
! Set problem size:
      INTEGER, PARAMETER :: n=100 !=2000 ! Could make this an input value
! Define arrays for knots, data points, etc
      REAL(dkind), ALLOCATABLE :: a(:), rhs(:), t(:), x(:), r(:)
! iseed is used to store the seed used for the Fortran intrinsic
!       random number generator
! saw_points is used to ensure that every interval in the partition
!       contains at least one point
      INTEGER, ALLOCATABLE :: iseed(:), saw_points(:)
      INTEGER :: m !, findInterval
! Define what will be the collection of matrix triplets.
      TYPE (dpTripletList) :: s, s_test
! Define the Harwell-Boeing derived type that holds the
! processed triplets.
      TYPE (dpHBSparseMatrix) :: b
! Define the ascended type that holds the Harwell-Boeing
! matrix and factorization quantities.
      TYPE (slu_dpHBSparseMatrix) :: g

! Define what will be the CSR version for the design matrix
      TYPE (dpCSRSparseMatrix) :: a_csr
! Define what will be the transposed CSR version for the design matrix
      TYPE (dpCSRSparseMatrix) :: aT_csr
! Define what will be the CSR version of A^TA for the design matrix
      TYPE (dpCSRSparseMatrix) :: ata_csr
 ! Define variables for test with Gram matrix
      REAL(dkind), ALLOCATABLE :: rhs_test(:),ata(:,:),d(:)
      INTEGER :: info
      INTEGER, ALLOCATABLE :: ipiv(:)
! Timing
      REAL(8) :: time_start, time_end
      REAL(dkind) :: sumsq


! Define local variables
      REAL (dkind) :: delta, u, v, resid_error, residuals
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
      ALLOCATE (t(m),x(n+m),r(n+m),rhs(n+m), STAT=errno)
      IF (errno /= 0) THEN
        print *, "Allocate fails with errno: ", errno
      END IF
! Generate the random values and write the matrix entries.
! Record the function values in RHS(*).
      DO j = 1, m
        CALL random_number(t(j))
        rhs(j) = t(j)**2
!        rhs(j) = sin(5*3.14*t(j))
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

! Use overloaded assigment to convert from a list of
! triplets. Create a CSR matrix representation for
! This assignment (a_csr =) converts the S list to a sparse matrix format
      ALLOCATE (rhs_test(n),d(n),ipiv(n), STAT=errno)
      a_csr = s_test
      aT_csr =  .t. a_csr        ! generates the sparse transpose (could have done it with triplets above)
      ata_csr = aT_csr .p. a_csr ! generates the NxN Gram matrix which may not be as sparse and may be ill-conditioned
      rhs_test(1:n)= aT_csr .p. rhs(1:m)
! Clear out space used by s_test
      s_test = 0

#if defined(__APPLE__ AND __GFORTRAN__)
! also need to pass -DCMAKE_Fortran_FLAGS="-D__APPLE__" to cmake
! do not compile for Mac/Darwin/Apple, appears to have array issues at marked lines

#else
! Using superlu with Gram matrix and normal equations, fastest
! Solve for the coefficients of the piece-wise linear spline; you don't get residuals this way
      call CPU_TIME(time_start)
! Convert to HB
      b= ata_csr
      d = b .ip. rhs_test
      r = ( a_csr .p. d )  - rhs     ! array error compiled under Darwin
      residuals = dnrm2(m,r,1)
      call CPU_TIME(time_end)
      write(*,*) 'Superlu with normal equations'
      WRITE (*,'(A)') ' Data Fitting of y(t)=t**2, (0,1).'
      WRITE (*,'(A,1PG12.5)') ' Computed Residuals (00) (Vector Norm)', residuals
      write(*,*) 'time ',time_end-time_start
      write(*,*) ' '

! Using lapack with Gram matrix and normal equations, converting to dense matrix, slowest
! Solve for the coefficients of the piece-wise linear spline; you don't get residuals this way
      call CPU_TIME(time_start)
! Convert to a dense matrix
      ata = ata_csr
      d(:)=rhs_test(:)    ! d gets overwritten
      call DGESV(n, 1, ata, n, IPIV, d, n, info ) ! d is overwritten
      r = ( a_csr .p. d ) - rhs       ! array error compiled under Darwin
      residuals = dnrm2(m,r,1)
      call CPU_TIME(time_end)
      write(*,*) 'Lapack with dense matrix normal equations'
      WRITE (*,'(A)') ' Data Fitting of y(t)=t**2, (0,1).'
      WRITE (*,'(A,1PG12.5)') ' Computed Residuals (0) (Vector Norm)', residuals
      write(*,*) 'time ',time_end-time_start
      write(*,*) ' '
#endif

! Using original H & H matrix above avoiding Gram matrix, and using superlu, intermediate in speed
! Use overloaded assigment to convert from a list of
! triplets. Create a Harwell-Boeing matrix representation for B.
! This assignment (B =) converts the S list to a sparse matrix format.
      call CPU_TIME(time_start)
      b = s
! Clear out space occupied by S.  This assignment deallocates
! the space used accumulating the list, S.
      s = 0
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
      residuals = dnrm2(m,r,1)
      r(1:m) = r(1:m) + x(n+1:n+m)
      resid_error = dnrm2(m,r,1)/dnrm2(m,x(n+1),1)
      call CPU_TIME(time_end)
      write(*,*) 'Superlu with sparse matrix, no Gram matrix'
      WRITE (*,'(A)') ' Data Fitting of y(t)=t**2, (0,1).'
      WRITE (*,'(A,1PG12.5)') ' Computed Residuals (1) (Vector Norm)', residuals
      write(*,*) 'time ',time_end-time_start
      WRITE (*,'(/A/(A,1PG12.5))') ' Relative Error (1) (Vector Norm)', &
        ' with solution and computed residual =', resid_error
      WRITE (*,'(A,2I10)') &
        ' The number of breakpoints (N) and data points (M) ', n, m


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
      residuals = dnrm2(m,r,1)
      r(1:m) = r(1:m) + x(n+1:n+m)
      resid_error = dnrm2(m,r,1)/dnrm2(m,x(n+1:),1)
      write(*,*) 'Superlu with sparse matrix, no Gram matrix'
      WRITE (*,'(A)') ' Repeated Data Fitting of y(t)=t**2, (0,1) - No Factorization -.'
      WRITE (*,'(A,1PG12.5)') ' Computed Residuals (2) (Vector Norm)', residuals

      WRITE (*,'(/A/(A,1PG12.5))') ' Relative Error (2) (Vector Norm)', &
        ' with solution and computed residual =', resid_error
      WRITE (*,'(A,2I10)') &
        ' The number of breakpoints (N) and data points (M) ', n, m


! so comparison of fit to data x(k) to computed actual data a(k)**2 at knots (which you don't get as data) is
sumsq = 0
 do k=1,n
   write(*,*) a(k),x(k),a(k)**2,x(k)-a(k)**2
   sumsq=sumsq+(x(k)-a(k)**2)**2
!   write(*,*) a(k),x(k),sin(5*3.14*a(k)),x(k)-sin(5*3.14*a(k))
!   sumsq=sumsq+(x(k)-sin(5*3.14*a(k)))**2
 end do
   sumsq=sqrt(sumsq)/n
write(*,*) 'Sum Squared',sumsq

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
