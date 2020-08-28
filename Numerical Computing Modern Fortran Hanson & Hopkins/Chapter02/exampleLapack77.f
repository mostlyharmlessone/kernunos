      PROGRAM exampleLapack77
C This program is (almost!) a Fortran 77 code that uses
C a pair of Lapack routines (dgetrf and dgetrs) to solve
C a randomly generated system of linear equations. The
C order of the problem to be solved is input by the user.      

C First we need to reserve array space for a maximum
C size of problem because Fortran 77 did not allow
C dynamic array allocation. If problems arise that
C are larger than this maximum the parameter maxdim
C must be increased, the driver program recompiled and
C the whole code relinked.      
      DOUBLE PRECISION one, zero
      PARAMETER (one = 1.0d0, zero = 0.0d0)
      INTEGER maxdim
      PARAMETER (maxdim=10000)
      DOUBLE PRECISION a(maxdim, maxdim), b(maxdim,1),
     +                 y(maxdim)
      INTEGER ipvt(maxdim)
C Declare the user-defined system size (n) and an error
C flag (info) as well as a couple of loop control variables
      INTEGER n, info, i, j
C Declare a variable for the relative error calculation and
C declare the return type of the BLAS Euclidean norm routine
      DOUBLE PRECISION relerr, dnrm2
C We cheat in the code and use the Fortran 90 intrinsics for
C timing the execution of the program.
C Note: these are defined as default REALs
      REAL tstart, tend
C Loop until an input value of n<0 is obtained. Also exit
C if an illegal input value is detected.      
10    CONTINUE
      WRITE(*,'(A)')
     +    'Input the required dimension of the linear system : '
      READ(*,'(I5)',IOSTAT=info)n
      IF (n .le. 0 .or. info .ne.0) GO TO 50
C Fill the coefficient matrix A and the solution vector
C y with random numbers. This uses the Fortran 90 intrinsic
C for generating random numbers because it is easier!
      
      DO 30 j = 1, n
        DO 20 i = 1, n
          CALL RANDOM_NUMBER(a(i,j))
20      CONTINUE
        CALL RANDOM_NUMBER(y(j))
        b(j,1) = zero
30    CONTINUE
C Compute the matrix-vector product B = A*Y.
      CALL dgemv('No',n,n,one,a,maxdim,y,1,zero,b,1)
C Start timing the solution computation.  
      CALL CPU_TIME(tstart)
C Factor the A matrix into its LU form using Gaussian
C elimination with partial pivoting.
C The flag info is zero if there was no diagonal term == 0.
C This is not a good test for ill-conditioning.
      CALL dgetrf(n, n, a, maxdim, ipvt, info)
C Check that the Lapack routine has been successful      
      IF (info .lt. 0) THEN
        WRITE(*,'(''Argument '',i3,'' has an illegal value'')')
     +         -info
      ELSE IF (info .gt. 0) THEN
        WRITE(*,'(''Zero diagonal value detected in upper ''//
     +              ''triangular factor at position '',i7)') info
      ELSE
C If there are no errors in the factorization stage then
C proceed to solve the linear system        
        CALL dgetrs('n', n, 1, a, maxdim, ipvt, b, maxdim, info)
C Check that the Lapack routine has been successful        
        IF (info .lt. 0) THEN
          WRITE(*,'(''Argument '',i3,'' has an illegal value'')')
     +         -info
        END IF
C Stop the timer        
        CALL CPU_TIME(tend)
C Compute the difference between the solution vector returned by
C Lapack and the vector used to generate the right hand side.        
        DO 40 i = 1, n
          b(i,1) = b(i,1) - y(i)
40      CONTINUE
        relerr = dnrm2(n, b, 1)/dnrm2(n,y,1)
C Write out relative errors and timings
        WRITE(*,'(A,I5,A,0PE12.4,A)') 'The compute time for '//
     +            'solving a system of size ',n,' is', tend-tstart,
     +            ' seconds'
        WRITE(*,'(A,1PD12.6)') 'The relative error  Y - '//
     +          'inverse(A)*(A*Y) = ', relerr
      END IF
      GO TO 10
50    STOP
      END PROGRAM exampleLapack77
