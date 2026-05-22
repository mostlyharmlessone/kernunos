  subroutine LSQspline(t, y, m, a, z, z2, n, err_report, periodic, csr , sparse)

! Adapted from H&H drivePiecewiseLinear

! Generates an equality-constrained least squares piece-wise cubic spline fitting of data with a
! continuous function with either natural or periodic boundary conditions.  The n breakpoints (knots) are equally spaced. 

! The input are the m data points (t, y(t)), assumed to be ordered.
! The output are the n knots and second derivatives at the knots of the LSQ piecewise cubic spline

! There are 2N unknowns at N knots in the problem and M data values.
! The 2N unknowns are the N z values and N z" second derivatives of the functions at the
! ends of the each breakpoint interval (the knots); the natural spline condition sets the end points = 0,
! removing 2 of the unknowns and making two constraints trivial; in the periodic case, there are 2N constraints and 2N unknowns
! periodic == .true. uses periodic conditions; .false. implies natural spline conditions

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
! csr == .true.  uses the 3N x 3N matrix above, csr = .false. uses the (3N+M) x (3N+M) version below

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

! The matrices are sparse, whether M x 2N or the Gram 3Nx3N version, however depending on the size of M, N, provision is made to convert to a dense form and solve with LAPACK
! as opposed to solving with superlu.
! sparse == .true. uses superlu ; sparse == .false. converts to dense matrix and uses LAPACK/DGESV

      USE set_precision, ONLY: wp
      USE sparseTypes, ONLY: dpTriplet, dpTripletList, dpCSRSparseMatrix, &
          dpHBSparseMatrix, slu_dpHBSparseMatrix
      USE sparseOps, ONLY: OPERATOR(.p.), OPERATOR(.t.)
      USE sluInterop, ONLY: OPERATOR(.ip.), ASSIGNMENT(=) 
      USE sparseAssign, ONLY: ASSIGNMENT(=)
      USE lapackinterface, ONLY: dnrm2, dgesv
      USE special_fct, ONLY : bsearch
      USE sparseUtils

      IMPLICIT NONE

      INTERFACE
       FUNCTION findInterval(u, n, a, delta) RESULT(k)
       USE set_precision, ONLY: wp
       INTEGER, INTENT(IN) :: n
       REAL(wp), INTENT(IN) :: u, a(*)
       REAL(wp), INTENT(IN) :: delta
       INTEGER :: k
       END FUNCTION findInterval
      END INTERFACE

! Number of data points
      INTEGER, INTENT(IN) :: m
! Data points
      REAL(wp), INTENT(IN) ::  t(m),y(m)
! number of knots:
      INTEGER, INTENT(IN) :: n  
! knots function, second derivatives of spline at knots
      REAL(wp), INTENT(OUT) :: a(n),z(n),z2(n)
! periodic = true means periodic bc, false, natural spline conditions
! csr = true means using the Gram product ATA and the 3n x 3n system, false means using the H&H M+3*n system
! sparse = true means using superlu to solve the system, false means converting to a dense matrix and using LAPACK
! obviously sparse = true is suitable for large n
      LOGICAL, INTENT(IN) :: periodic, csr , sparse

! error reporting 
      INTEGER, INTENT(OUT) :: err_report

! Real constants
      REAL(wp), PARAMETER :: one=1.0E0_wp, zero=0.0E0_wp
! Define arrays for knots, data points, etc
      REAL(wp), ALLOCATABLE :: rhs(:), x(:), r(:)

! Define what will be the collection of matrix triplets.
      TYPE (dpTripletList) :: s
! Define what will be the CSR version for the design matrix
      TYPE (dpCSRSparseMatrix) :: a_csr
! Define what will be the CSR version of A^TA for the design matrix
      TYPE (dpCSRSparseMatrix) :: ata_csr
! Define some  triplets
      TYPE (dpTriplet), ALLOCATABLE :: triplets(:)
! Define variables for LAPACK 
      REAL(wp), ALLOCATABLE :: dense(:,:),d(:)
      INTEGER, allocatable :: ipiv(:)
      INTEGER :: info
! Define the Harwell-Boeing derived type that holds the
! processed triplets.
      TYPE (dpHBSparseMatrix) :: b

! Modified to allow n intervals after each break point including the last one for the periodic case
! Define local variables
      REAL (wp) :: delta, v, u, resid_error
      INTEGER :: i, j, k, sz, high, low
      if (periodic) then
       delta = (t(m)-t(1))/real(n,wp)
! Define the array of breakpoints
       a(1) = t(1)
       a(n) = t(m)-delta
        DO i = 2, n - 1
         a(i) = a(i-1) + delta
       END DO
      else
       delta = (t(m)-t(1))/real(n-1,wp)
! Define the array of breakpoints
       a(1) = t(1)
       a(n) = t(m)
        DO i = 2, n - 1
         a(i) = a(i-1) + delta
       END DO
      endif
     err_report = 0
! Allocate local working space
     if (csr) then
      ALLOCATE (x(3*n),r(3*n),rhs(3*n),d(3*n),ipiv(3*n), STAT=err_report)
     else
      ALLOCATE (x(3*n+m),r(3*n+m),rhs(3*n+m),d(3*n+m),ipiv(3*n+m), STAT=err_report)
     endif
     IF (err_report /= 0) THEN
      print *, "Allocate fails with error: ", err_report
      return
     END IF
! initialize
     rhs(:) = zero
     DO j = 1, m     
        k = findInterval(t(j), n, a, delta)
        v = one-(t(j)-a(k))/delta
!       findInterval is faster than binary search, but is wrong sometimes
        if ((v .gt. 1) .or. (v .lt. 0)) then
         err_report = err_report + 1
         call bsearch(t(j),a,n,high,low)
         k=low
         v = one-(t(j)-a(k))/delta
        endif
! Gather up the list of the sparse matrix triplets (S) that
! will define B.  The next assignments (S =) are accumulation
! steps of the list of matrix entries.
! Write adjacent columns of the (design) A matrix, in NW corner of B:
! Starts at 1st row and column, m rows, 2*n columns; odd is z, even z''
      if (csr) then
! only write A for CSRSparse to make ATA
        if (abs(v) > 0) s = dpTriplet(j,2*k-1,v)
        if (abs(delta*delta*v*(v*v-one)/6.0) > 0) s = dpTriplet(j,2*k, delta*delta*v*(v*v-one)/6.0 )
        if (k .lt. n) then
         if (abs(one-v) > 0) s = dpTriplet(j,2*k+1,one-v)
         if (abs(delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0) > 0) s = dpTriplet(j,2*k+2,delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0 )
        endif
 ! For H&H approach write A, A^T and I_M
      else
        if (abs(v) > 0) s = dpTriplet(j,2*k-1,v)
        if (abs(delta*delta*v*(v*v-one)/6.0) > 0)s = dpTriplet(j,2*k, delta*delta*v*(v*v-one)/6.0 )
        if (k .lt. n) then
         if (abs(one-v) > 0) s = dpTriplet(j,2*k+1,one-v)
         if (abs(delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0) > 0) s = dpTriplet(j,2*k+2,delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0 )
        endif
! Write adjacent rows of the A^T matrix, in middle of B:
! Starts at m+1st row and 2*n+1st column, 2*n rows, m columns
        if (abs(v) > 0) s = dpTriplet(2*k-1+m,2*n+j,v)
        if (abs(delta*delta*v*(v*v-one)/6.0) > 0) s = dpTriplet(2*k+m,2*n+j, delta*delta*v*(v*v-one)/6.0 )
        if (k .lt. n) then
         if(abs(one-v) > 0) s = dpTriplet(2*k+1+m,2*n+j,one-v)
         if (abs(delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0) > 0) s = dpTriplet(2*k+2+m,2*n+j,delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0 )
        endif
! Write row of identity matrix I_M, top middle of B:
        s = dpTriplet(j,2*n+j,one)
      endif
     END DO

     if (err_report .ne. 0) then
      write(*,*) 'Errors in findInterval', err_report
      err_report = 0
     endif

     if (csr) then
! CSR version
! Use overloaded assigment to convert from a list of
! triplets. Create a Compressed Sparse Row matrix representation for A, A^T.
        a_csr = s
        if (a_csr%errFlag .ne. 0) then
         write(*,*) 'FATAL Error in CSRCOO in LSQspline',a_csr%errFlag
         stop
        endif
        s = 0
! Create A^TA by multiplication of CSR sparse matrices
        ata_csr = (.t. a_csr) .p. a_csr
        if (ata_csr%errFlag .ne. 0) then
         write(*,*) 'FATAL Error in AMUB in LSQSpline',ata_csr%errFlag
         stop
        endif
! Make triplets from ata_csr
        triplets = ata_csr
        if (ata_csr%errFlag .ne. 0) then
         write(*,*) 'FATAL Error in CSRCOO in LSQspline',a_csr%errFlag
         stop
        endif
        s = triplets
! Create A^T*y
       rhs(1:2*n) = (.t. a_csr) .p. y(:)
      else
!  NOT csr
       rhs(1:m)=y(:)
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
     b = 0

   else

! Use LAPACK not superlu
    a_csr = 0
    a_csr = s
    s = 0
! convert the sparse matrix to a dense matrix
    dense = a_csr
    call DGESV(size(dense,1), 1, dense, size(dense,1), IPIV, rhs, size(dense,1), info ) ! rhs is overwritten
    if (info .ne. 0) err_report = info
    if (info .ne. 0) write(*,*) 'LAPACK DGESV info: ',info
    a_csr = 0
    x(:) = rhs(:)
   endif

   do i=1,n
    z(i)=x(2*i-1)
    z2(i)= x(2*i)
   end do

   DEALLOCATE (x,r,rhs,d,ipiv)

  end subroutine LSQSpline

    FUNCTION findInterval(u, n, a, delta) RESULT(k)
    USE set_precision, ONLY: wp
    INTEGER, INTENT(IN) :: n
    REAL(wp), INTENT(IN) :: u, a(*)
    REAL(wp), INTENT(IN) :: delta
    REAL(wp) :: v
    REAL(wp), PARAMETER :: one = 1.0E0_wp
    INTEGER :: k
    k = max(1,min(n,floor(real(n*u/a(n),wp))))
    DO
      v = (u-a(k))/delta
      IF (v<=one .OR. k==n) EXIT
! Move to the next interval.        
      k = k + 1
    END DO
    END FUNCTION findInterval
