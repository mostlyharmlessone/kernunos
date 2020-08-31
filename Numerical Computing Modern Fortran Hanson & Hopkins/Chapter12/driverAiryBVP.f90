    PROGRAM driverAiryBVP

! Solve the Airy boundary value problem y''-x * y = 0, y(0)=y(1)=1
! in a least-squares sense.  Use piece-wise linear continuous
! functions with equally spaced breakpoints.

! The interval [0,1] is partitioned into n-1 intervals each having
! width h=1/(n-1). There are n values of the function y(x) at the
! breakpoints. The n-2 unknowns are the values of the piece-wise
! linear functions at the ends of each internal breakpoint 
! interval. The left and right values are fixed at the value 1
! by the boundary conditions.

      USE set_precision, ONLY: dkind
      USE sparseTypes, ONLY: dpTriplet, dpTripletList,&
          dpHBSparseMatrix
      USE sparseOps, ONLY: OPERATOR(.p.)
      USE sluInterop, ONLY: OPERATOR(.ip.)
      USE sparseAssign, ONLY: ASSIGNMENT(=)
      IMPLICIT NONE

! Constants
      REAL (dkind), PARAMETER :: one = 1.0E0_dkind, &
             two = 2.0E0_dkind, four = 4.0E0_dkind, &
             twelve = 12.0E0_dkind, zero = 0.0E0_dkind
! Set problem size -- could make this run-time data
      INTEGER, PARAMETER :: n = 10001

! Define array for the knots
      REAL (dkind) :: b(n), x(n), y(n+n-2), z(n+n-2)
! Declare variables for the collection of matrix triplets and 
! the HB matrix for SuperLU
      TYPE (dpTripletList) :: s
      TYPE (dpHBSparseMatrix) :: g
      TYPE (dpTriplet), ALLOCATABLE :: t(:)
! Local variables
      INTEGER :: i, j
      REAL (dkind) :: h, u, v

! Define the value of h, the interval width.
      h = one/REAL(n-1,dkind)

! Define entries for the matrix of integrals
      DO i = 1, n
! Get the pieces of the integrals for each of the n-1 intervals.
! This defines the sparse matrix by rows.

! Get the contribution of basis function i with the series.
! Each basis function has support that is non-zero on
! [max(0,(i-1)*h),min(1,(i+1)h)].

! Note: All matrix entries and right-hand side values
!       have been scaled by h, the constant increment.
        SELECT CASE (i)

        CASE (1) ! Left end       
          u = (twelve+h**3)/twelve
          s = dpTriplet(i,1,u)
          v = -(twelve-h**3)/twelve
          s = dpTriplet(i,2,v)

        CASE (n) ! Right end
          u = -(twelve-two*h**2+h**3)/twelve
          s = dpTriplet(i,n-1,u)
          v = (twelve+four*h**2-h**3)/twelve
          s = dpTriplet(i,n,v)

        CASE DEFAULT ! 1 < i < n
          u = -(twelve-h**3*(two*REAL(i,dkind)-one))/twelve
          s = dpTriplet(i,i-1,u)
          v = (twelve+h**3*(four*REAL(i,dkind)-one))/twelve
          s = dpTriplet(i,i,v)

          v = (twelve+h**3*(four*REAL(i,dkind)+one))/twelve
          s = dpTriplet(i,i,v)
          u = -(twelve-h**3*(two*REAL(i,dkind)+one))/twelve
          s = dpTriplet(i,i+1,u)
        END SELECT
      END DO
! Define the fixed values
      b = zero
      b(1) = one
      b(n) = one

! Use prior estimate of the end derivatives,
! one-sided divided differences for y_n'(0) and y_1'(0).
      s = dpTriplet(1,2,one)
      s = dpTriplet(n,n-1,one)

! Convert matrix to Harwell-Boeing format
      g = s
! Fix the boundary values and compute resulting
! right-hand side 
      x = zero
      x(1) = one
      x(n) = one

! Recover the non-zero values from G and represent them
! as a list of triplets.
      t = g

! Use columns 2,...,n-1 to define a new matrix.
! This new matrix is used to define the least-squares system.

! Clear out set of triplets in s and reuse for least-squares
      s = 0
      DO j = 1, SIZE(t)
      ASSOCIATE(colInd=>t(j)%columnIndex, rowInd=>t(j)%rowIndex, &
                val=>t(j)%value)
! Skip the first and last columns
        IF (colInd==1 .OR. colInd==n) THEN
! Compute resulting right-hand side
          b(rowInd) = b(rowInd) - val
        ELSE
! Add elements to matrix C (not original columns 1 and n)
          s = dpTriplet(t(j)%rowIndex,t(j)%columnIndex-1,t(j)%value)
          s = dpTriplet(t(j)%columnIndex-1+n,t(j)%rowIndex+n-2,t(j)%value)
        END IF
      END ASSOCIATE  
      END DO

! Define NE corner, the n by n identity matrix.
      DO i = 1, n
        s = dpTriplet(i,i+n-2,one)
      END DO

! Clear G and convert to Harwell-Boeing format 
      g = 0
! This matrix is now 2*n - 2 by 2*n - 2 
      g = s
! Define the right-hand side for the least-squares problem
      y(1:n) = b(1:n)
      y(n+1:2*n-2) = zero
! Solve the sparse system for values and residuals 
! This involves a hidden factorization, solve, and 
! memory release operation.  Since there is only one
! solve operation, this is efficient.
      z = g .ip. y
! Place components 1,...,n-2 into right places in solution array 
      x(2:n-1) = z(1:n-2)
! Do what is required with the solution vector x(1:n) -- we just
! print out a coupld of values
      WRITE (*, '(''x(2) = '', e12.4, '' x(n-1) = '', e12.4)') &
             x(2), x(n-1)
! Use maximum residual value to estimate error 
      WRITE (*,'(A, I7/A, 1P D12.4)') 'With n function values = ', n, &
        'Maximum Least Squares Residual Value =', MAXVAL(ABS(z(n-1:2*n-2)))
    END PROGRAM driverAiryBVP
