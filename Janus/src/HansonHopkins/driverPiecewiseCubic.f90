    PROGRAM drivePiecewiseCubic

! Generate the coefficient matrix for a equality-constrained least squares problem
! that comes from piece-wise cubic spline fitting of data with a
! continuous function.  The breakpoints (knots) are equally spaced.

! There are 2N unknowns at N knots in the problem and M data values.
! The 2N unknowns are the N z values and N z" second derivatives of the functions at the
! ends of the each breakpoint interval (the knots); the natural spline condition sets the end points = 0,
! removing 2 of the unknowns and making two constraints trivial; in the periodic case, there are 2N constraints and 2N unknowns

! The M data value are pairs (t_i, y(t_i)) where the t_i
! are random on (0,1).

! the design matric A (Mx2N) is for the LSQ solution to A*x=b (M data points) with constraint C*(z,z")=d as
! as either (2N x N)  constraints

! on the continuity of the first derivatives at the knots, h are the Lagrange multipliers
! then x == (z sub j,z" sub j) alternating.  b = y(t) here and d = 0 with these constraints

! The constraint on the first derivative can be periodic so that the 1st derivatives at the last knot and first knot are continuous
! or natural, in which case the second derivative at the ends is zero

! The matrix  B=[A^TA : C^T][ x ]  = [A^T*b] 2N
!               [ C   :  0 ][ h ]    [  d  ] N
!                 2N     N

! The matrix B has dimension (3N x 3N) but has the possibly ill-conditioned Gram product ATA.

! The extension of H&H to avoid the normal equations is using
! The matrix B with dimension (M+3N)x(M+3N)

! The matrix  B= [A : I_M :  0 ][ x ]   [ b ] M
!                [0 : A^T : C^T][ r ] = [ 0 ] 2N
!                [C :  0  :  0 ][ h ]   [ d ] N
!                 2N   M     N
! which adds the constraint C*y=d, with h Lagrange multipliers
! which can be rearranged for symmetry as (Amy Tabb referencing Matrix Computations, Gene H. Golub and Charles F. Van Loan. 4th edition, 2013 ISBN 9781421407944.)

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
      USE lapackinterface, ONLY: dnrm2, dgesv, GaussJordan
      USE sparseUtils

      IMPLICIT NONE

! Real constants
      REAL(dkind), PARAMETER :: one=1.0E0_dkind, zero=0.0E0_dkind
! Set problem size:
      INTEGER, PARAMETER :: n=10 !n=2000 ! Could make this an input value
      LOGICAL, PARAMETER :: periodic = .false. ! Could make this an input value
! Define arrays for knots, data points, etc
      REAL(dkind), ALLOCATABLE :: a(:), rhs(:), t(:), x(:), r(:),rhs_test(:)
! iseed is used to store the seed used for the Fortran intrinsic
!       random number generator
! saw_points is used to ensure that every interval in the partition
!       contains at least one point
      INTEGER, ALLOCATABLE :: iseed(:), saw_points(:)
      INTEGER :: m, findInterval
! Define what will be the collection of matrix triplets.
      TYPE (dpTripletList) :: s, s_test, sc_test
! Define what will be the CSR version for the design matrix
      TYPE (dpCSRSparseMatrix) :: a_csr, b_csr
! Define what will be the CSR version of A^TA for the design matrix
      TYPE (dpCSRSparseMatrix) :: ata_csr

!!!!!!testing
      real(dkind), ALLOCATABLE :: dense(:,:),d(:)
      integer, allocatable :: ipiv(:)
      integer :: info,high,low
      real(dkind) :: sumsq,y,y1,y2,z,zs

! Define the Harwell-Boeing derived type that holds the
! processed triplets.
      TYPE (dpHBSparseMatrix) :: b
! Define the ascended type that holds the Harwell-Boeing
! matrix and factorization quantities.
      TYPE (slu_dpHBSparseMatrix) :: g

! Define local variables
      REAL (dkind) :: delta, v, u, resid_error
      INTEGER :: errno, i, j, k, sz
! Modified to allow n intervals

!!!!!
!      ALLOCATE (a(n), saw_points(n-1), STAT=errno)
      ALLOCATE (a(n), saw_points(n), STAT=errno)


      IF (errno /= 0) THEN
        print *, "Allocate fails with errno: ", errno
      END IF
      delta = one/real(n-1,dkind)


!!!!!!!!!!
!      delta = one/real(n,dkind)

! Define the array of breakpoints
      a(1) = zero
      a(n) = one
! Define the knots or inner breakpoints
!!!!!!!!!!!
!      DO i = 2, n

       DO i = 2, n - 1
        a(i) = a(i-1) + delta
      END DO

! Generate sufficient random values so that each interval
! has at least one value.  We do not store these data values
! here as we don't know the total number of data points required yet
! note for the periodic case we need a value in the n to 1 interval so that k can actually reach n
! multiply u by 1+delta
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

!!!!!!!!!!!!!!!!!!!!!!!
        k = findInterval(u*(1.0+delta), n, a)
        call bsearch(u*(1.0+delta),a,n,high,low)

!        k = findInterval(u, n, a)

        saw_points(k) = 1
        m = m + 1
      END DO
! Set random number seed so the same sequence results.
      CALL random_seed(put=iseed)
! Allocate local working space
      ALLOCATE (t(m),x(3*n+m),r(3*n+m),rhs(3*n+m),d(3*n+m),rhs_test(3*n),ipiv(3*n+m), STAT=errno)
      IF (errno /= 0) THEN
        print *, "Allocate fails with errno: ", errno
      END IF
! Generate the random values and write the matrix entries.
! Record the function values in RHS(*).
      DO j = 1, m     ! j = data points
        CALL random_number(u)
        t(j) = u *(1.0+delta)
!         t(j) = u
!        rhs(j) = sin(5*3.14*t(j)) !t(j)**2
        rhs(j) = t(j)**2

!!!!!!!!!!!!!!!
        call bsearch(t(j),a,n,high,low)
        k = findInterval(t(j), n, a)
        if (k .ne. low) write(*,*) 'low',low,k,high,t(j),a(k)
        if (k .ne. high) write(*,*) 'high',low,k,high,t(j),a(k)

        v = one-(t(j)-a(k))/delta
! Gather up the list of the sparse matrix triplets (S) that
! will define B.  The next assignments (S =) are accumulation
! steps of the list of matrix entries.

! Write adjacent columns of the (design) A matrix, in NW corner of B:
! Starts at 1st row and column, m rows, 2*n columns; odd is z, even z''
        s = dpTriplet(j,2*k-1,v)
        s = dpTriplet(j,2*k, delta*delta*v*(v*v-one)/6.0 )
        s = dpTriplet(j,2*k+1,one-v)
        s = dpTriplet(j,2*k+2,delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0 )
! copy for test of CSRSparse to make ATA
        s_test = dpTriplet(j,2*k-1,v)
        s_test = dpTriplet(j,2*k, delta*delta*v*(v*v-one)/6.0 )
        s_test = dpTriplet(j,2*k+1,one-v)
        s_test = dpTriplet(j,2*k+2,delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0 )
! Write adjacent rows of the A^T matrix, in middle of B:
! Starts at m+1st row and 2*n+1st column, 2*n rows, m columns
        s = dpTriplet(2*k-1+m,2*n+j,v)
        s = dpTriplet(2*k+m,2*n+j, delta*delta*v*(v*v-one)/6.0 )
        s = dpTriplet(2*k+1+m,2*n+j,one-v)
        s = dpTriplet(2*k+2+m,2*n+j,delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0 )
! Write row of identity matrix I_M, top middle of B:
        s = dpTriplet(j,2*n+j,one)

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


! CSR versions start at 2n+1 row and 1st column for C,   n rows, 2n columns
!                       2n+1 column and 1st row for C^T, 2n rows, n columns

      END DO

! Define the rest of the right-hand side; the constraint C*y = d = 0
      rhs(m+1:3*n+m) = zero

! CSR version
! ?needs more operators for assembly
! Use overloaded assigment to convert from a list of
! triplets. Create a Compressed Sparse Row matrix representation for A, A^T.
!        a_csr = s_test
!        s_test = 0
! Create A^TA by multiplication of CSR sparse matrices
!        ata_csr = (.t. a_csr) .p. a_csr
! Make triplets from ata_csr
!        triplets = ata_csr
!        s_test = triplets
!        b_csr = sc_test + s_test
! Create A^T*y
!        rhs_test(1:3*n) = a_csr .p. rhs(1:m)

! Use overloaded assigment to convert from a list of
! triplets. Create a Harwell-Boeing matrix representation for B.
! This assignment (B =) converts the S list to a sparse matrix format.
      b = s

  a_csr=s
  dense = a_csr
  d(:)=rhs(:)    ! d gets overwritten
!  size(dense,1)->3*n+m
  call DGESV(size(dense,1), 1, dense, size(dense,1), IPIV, d, size(dense,1), info ) ! d is overwritten
 write(*,*) 'info',info
if (info .ne. 0) stop

! Clear out space occupied by S.  This assignment deallocates
! the space used accumulating the list, S.
      s = 0

! Solve for the coefficients of the piece-wise cubic spline.
! This defined operation works with a Harwell-Boeing matrix
! and ascends B to be a component of an extended type, G.
! Default settings of pivoting rules and other parameters
! are used.
      x = b .ip. rhs
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
      r = b .p. x(1:2*n)
      r = r - rhs(1:size(r))
      r(1:m) = r(1:m) + x(2*n+1:2*n+m)
      resid_error = dnrm2(m,r,1)/dnrm2(m,x(2*n+1:),1)

      WRITE (*,'(A)') ' Repeated Data Fitting of y(t)=t**2, (0,1) - No Factorization -.'
      WRITE (*,'(A,2I10)') &
        ' The number of breakpoints (N) and data points (M) ', n, m
      WRITE (*,'(/A/(A,1PG12.5))') ' Relative Error (2) (Vector Norm)', &
        ' with solution and computed residual =', resid_error


!the difference is easily seen by setting n=20 instead of 2000
!plot t(j),z(j) ie t(j), t(j)**2 j=1,m and also a(k),x(k) k=1,n

!!!!!!!!!!!!!!!!!!!!!!!!!!!
! x are the coefficients so y=x(i)v+x(i+1)(1-v); at knots v=1 so x(i)==y(i) for linear
! x are the coefficients so y=x(2+i-1)v+x(2+i)*delta*delta*v*(v*v-one)/6.0
!                            +x(2*i+1)(1-v)+x(2*i+2)*delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0
!                          at knots v=1 so x(2*i-1)==y(i) for cubic

!      call SplineEval(0,x,y,y2,n,u,f,fp,fpp,fppp)
      !  x->a(k) (n values), y->z(j) (m values), y2->z2(k) (n values) at knots


! so comparison of fit to data x(k) to computed actual data a(k)**2 at knots (which you don't get as data) is
sumsq = 0
 do k=1,n
   write(*,*) a(k),x(2*k-1),a(k)**2,x(2*k-1)-a(k)**2
   sumsq=sumsq+(x(2*k-1)-a(k)**2)**2
!   write(*,*) a(k),x(2*k-1),sin(5*3.14*a(k)),x(2*k-1)-sin(5*3.14*a(k))
!   sumsq=sumsq+(x(2*k-1)-sin(5*3.14*a(k)))**2
 end do
   sumsq=sqrt(sumsq)/n
write(*,*) 'Sum Squared',sumsq


!X(), Y() [in] Data pairs (X(I), Y(I), I = 1, ..., NXY).
!The contents of X() must satisfy X(1) ≤ X(2) ≤ ...
!≤ X(NXY).
!SD() [in] If SD(1) > 0., each SD(I) must be positive
!and must be the user’s a priori estimate of the standard deviation of the uncertainty (e.g., observational
!error) in the corresponding data value Y(I).
!If SD(1) < 0., |SD(1)| will be used as the a priori
!tandard deviation of each data value Y(I). In this
!case the array SD() may be dimensioned as SD(1).
!NXY [in] Number of data points. Require NXY ≥ 4.
!B() [in] Set by the user to specify the knot abscissae
!and endpoints for the spline curve. Must satisfy B(1)
!< B(2) < ... < B(NB). Also require B(1) ≤ X(1),
!B(NB) ≥ X(NXY).
!NB [in] Number of knots, including endpoints. The
!number of segments in the spline curve will be NB −
!1. The number of degrees of freedom for the fit will
!be NB + 2. Require 2 ≤ NB ≤ NXY − 2.
!W( , ) [scratch] Working space, dimensioned
!W(LDW, 5).
!LDW [in] Leading dimension for the work array W(,).
!LDW must be at least NB + 4, but the execution
!time is less for larger values of LDW, up to NB+3+k,
!where k is the largest number of data points lying between any adjacent pair of knots.
!YKNOT(), YPKNOT() [out] Arrays, each of length
!at least NB, in which the subroutine will store a definition of the fitted spline curve as a sequence of values
!of the curve and its first derivative at the knot abscissae B(i). Letting f denote the fitted curve, the
!elements of these arrays will be set to
!YKNOT(i) = f(B(i)), i = 1, ..., NB
!YPKNOT(i) = f
!0
!(B(i)), i = 1, ..., NB
!SIGFAC [out] Set by the subroutine as a measure of
!the residual error of the fit. See Section D.
!IERR1 [out] Error status indicator. Set on the basis
!of tests done in SC2FIT as well as error indicators
!IERR2 set by SBACC and IERR3 set by SBSOL.
!Zero indicates no errors detect

!  call DC2FIT(X, Y, SD, NXY, B, NB, W, NW, YKNOT,YPKNOT,SIGFAC,IERR)




do j=1,1001
z = (j-1)/1000.0
k = findInterval(z, n, a)
        IF ( k .gt. 1 .and. k .lt. n  ) THEN
        zs = -1.0*x(2*k-3) + x(2*k-2)*delta*delta/6.0 + 2.0*x(2*k-1) + &
             2.0*x(2*k)*delta*delta/3.0 - 1.0*x(2*k+1) + x(2*k+2)*delta*delta/6.0  !should be zero
        else
        zs = 0
        ENDIF
v = one-(z-a(k))/delta
y = x(2*k-1)*v + x(2+k)*delta*delta*v*(v*v-one)/6.0 &
  + x(2*k+1)*(1-v) + x(2*k+2)*delta*delta*(one-v)*((one-v)*(one-v)-one)/6.0
y2 = x(2*k)*v + x(2*k+2)*(1-v)
!if (v .eq. 0 .or. v .eq. 1) then
 write(*,*) z,z**2,y,y-z**2,zs,y2,k,a(k)
!endif
end do

! Free storage and clear matrix
      g = 0
      b = 0
    END PROGRAM

    FUNCTION findInterval(u, n, a) RESULT(k)
    USE set_precision, ONLY: dkind
    INTEGER, INTENT(IN) :: n
    REAL(dkind), INTENT(IN) :: u, a(*)
    REAL(dkind) :: v, delta
    REAL(dkind), PARAMETER :: one = 1.0E0_dkind
    INTEGER :: k
! modified to allow n intervals after each break point including the last one for periodic case
! This direct computation of the interval containing
! the data can be off (low) by 1.  So if the value
! of the basis function is > 1, move to the next interval.
      delta = one/real(n-1,dkind)
    k = max(1,min(n,floor(real(n*u,dkind))))
    DO
      v = (u-a(k))/delta
      IF (v<=one .OR. k==n) EXIT
! Move to the next interval.        
      k = k + 1
    END DO

    END FUNCTION findInterval
