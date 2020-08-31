    PROGRAM exampleLapack90
      USE set_precision, ONLY : wp
! This program is a Fortran 90 code that uses
! a pair of Lapack routines (dgetrf and dgetrs) to solve
! a randomly generated system of linear equations. The 
! order of the problem to be solved is input by the user.

! Declare allocatable arrays so we may reserve the correct
! amount of space for the particular problem being solved.
      REAL (wp), ALLOCATABLE :: a(:,:), b(:,:), y(:)
      INTEGER, ALLOCATABLE :: ipvt(:)
! Declare the system size (n) and an error flag (info)
! as well as a couple of loop control variables
      INTEGER :: n, info, i, j
! These are values for relative sizes and errors.
      REAL (wp) :: relerr, dnrm2
! These are values for the elapsed time of solving.
      REAL :: tstart, tend
      REAL (wp), PARAMETER :: one = 1.0E0_wp, zero = 0.0E0_wp

! Loop until an input value of n<0 is obtained also exit
! if an illegal input value is detected.
      DO WHILE (.TRUE.)
        WRITE (*,ADVANCE='no', &
              FMT='(''Input the required dimension of the linear system : '')')
        READ (*,'(i5)',iostat=info) n
        IF (n<=0) THEN
          STOP
        ENDIF
        IF (info/=0) THEN
          WRITE(*,'(''Illegal value given for argument '', i3)') &
               info
          STOP
        END IF
! Allocate array space
        ALLOCATE (a(n,n), b(n,1), y(n), ipvt(n), STAT=info)
        IF (info /=0) THEN
          WRITE(*,'(''Error attempting to allocate array space '''// &
                    '''for system of order '', i6)') n
          STOP
        END IF
! Fill the coefficient matrix a and the solution vector
! y with random numbers. This uses the Fortran 90 intrinsic
! for generating random numbers because it's easier!

        DO j = 1, n
          DO i = 1, n
            CALL random_number(a(i,j))
          END DO
          CALL random_number(y(j))
          b(j,1) = zero
        END DO

! Compute the matrix-vector product B = A*Y.
        CALL dgemv('No',n,n,one,a,n,y,1,zero,b,1)

! Start timing the solution computation.
        CALL cpu_time(tstart)

! Factor the A matrix into its LU form with partial pivoting.
! The flag INFO is zero if there was no diagonal term == 0.
! This is not a good test for ill-conditioning.
        CALL dgetrf(n,n,a,n,ipvt,info)

! Check that the Lapack routine has been successful
        IF (info<0) THEN
          WRITE (*,'(''Argument '',i3,'' has an illegal value'')') - info
        ELSE IF (info>0) THEN
          WRITE (*,'(''Zero diagonal value detected in upper ''// &
            &                       ''triangular factor at position &
            &'',i7)') info
        ELSE

! If there are no errors in the factorization stage then
! proceed to solve the lnear system
          CALL dgetrs('n',n,1,a,n,ipvt,b,n,info)

! Check that the Lapack routine has been successful
          IF (info<0) THEN
            WRITE (*,'(''Argument '',i3,'' has an illegal value'')') - info
          END IF

! Stop the timer
          CALL cpu_time(tend)

! Compute the difference between the solution vector returned by
! Lapack and the vector used to generate the right hand side.
          DO i = 1, n
            b(i,1) = b(i,1) - y(i)
          END DO

          relerr = dnrm2(n,b,1)/dnrm2(n,y,1)

! Write out relative errors and timings
          WRITE (*,'(a,i5,a,0pd12.4,a)') &
            'The compute time for solving a system of size ', &
            n, ' is', tend - tstart, ' seconds'
          WRITE (*,'(a,1pd12.6)') &
            'The relative error  Y - inverse(A)*(A*Y) = ', relerr
        END IF

        DEALLOCATE(a, b, y, ipvt, STAT=info)
        IF (info /= 0) THEN
          WRITE(*,'(''Deallocate failed'')')
        END IF
      END DO ! end of do while
    END PROGRAM exampleLapack90
