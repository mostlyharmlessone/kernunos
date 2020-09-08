    MODULE piApproxExample
    USE set_precision, ONLY: wp
    INCLUDE 'mpif.h'
    REAL(wp), PARAMETER :: one = 1.0e0_wp, zero = 0.0e0_wp, &
      four = 4.0e0_wp, half = 0.5e0_wp
    INTEGER, PARAMETER :: rootId = 0
    INTEGER :: numProcs, myId, mpiVersion, mpiSubversion, n, m
    INTEGER :: recvStat(mpi_status_size)
    CHARACTER, ALLOCATABLE :: processName(:)
    REAL(wp) :: timePsum(1), timeTotal, piApprox

    CONTAINS
    SUBROUTINE mpi_startup
! This is a collection of calls to MPI routines that are
! commonly used in the set up phase of an MPI application.
! We set the variables numProcs (total number of processes in
! the current work-group) and myId (the rank/node number of the
! executing process). In addition we obtain the version of mpi
! being used and the name of the executing processor
    INTEGER :: ierr, nameSize
    CHARACTER :: tempName(mpi_max_processor_name)

! Initialise the mpi system, this must be called before any
! other mpi library routines.
    CALL mpi_init(ierr)
! Get the number of processes available in the work group
! We are using the default communicator mpi_comm_world
    CALL mpi_comm_size(mpi_comm_world, numProcs, ierr)
! Get the rank of the executing process
    CALL mpi_comm_rank(mpi_comm_world, myId, ierr)
! Get the version of the mpi library the code has been
! linked against. This is given in the form (version.subversion)
    CALL mpi_get_version(mpiVersion, mpiSubversion, ierr)
! Get the name of the executing processor
! This is at most mpi_max_processor_name characters, get
! the name and then store it in a correct sized array
    CALL mpi_get_processor_name(tempName, nameSize, ierr)
    ALLOCATE(processName(nameSize))
    processName(:) = tempName(1:nameSize)

    END SUBROUTINE mpi_startup

    SUBROUTINE mpi_shutdown
! Routine to tidy up and then shut down the mpi system. Note 
! that mpi_finalize does not halt the executable but it does
! prevent any further calls to the mpi_system

! Just to be safe -- we wait until all the executing 
! processes arrive here
    CALL mpi_barrier(mpi_comm_world, ierr)
! Deallocate space for processor name
    DEALLOCATE(processName)
! Stop the mpi system -- no more mpi library calls allowed
    CALL mpi_finalize(ierr)
! Halt the execution
    STOP
    END SUBROUTINE mpi_shutdown

    SUBROUTINE readIntBcast(prompt, val, quit)
    INTEGER, INTENT(OUT) :: val
    CHARACTER(LEN=*), INTENT(IN) :: prompt
    LOGICAL, INTENT(OUT) :: quit
! Routine to input a legal integer, val, from the user
! following the output of the user-supplied prompt. The input
! is  performed on the root process and then broadcast to all
! the other processes in the work group.
! quit -- set false for n>0 and true otherwise.

    INTEGER :: ierr, itemp(1)
    LOGICAL :: init
! Check that the mpi system has been initialized if not
! initialize it
    quit = .FALSE.
    CALL mpi_initialized(init, ierr)
    IF (.NOT. init) THEN
      CALL mpi_startup
    END IF
! Only the root node deals with input/output
    IF (myId == rootId) THEN
! Loop until we have a valid integer
getInput: &
      DO
        WRITE(*, '(A)') TRIM(prompt) 
        READ(*,'(I10)',IOSTAT=ierr) itemp(1)
        IF (ierr == 0) EXIT ! Legal integer input
      END DO getInput
    END IF

    CALL mpi_bcast(itemp, 1, mpi_integer, rootid, mpi_comm_world, ierr)
    val = itemp(1)
! If n is zero or less then halt execution.
! Note all processes have the value of n here so all can
! now halt their own execution.
    IF (val <= 0) THEN
      quit = .TRUE.
    END IF

    END SUBROUTINE readIntBcast

    SUBROUTINE computePi(noError)
    LOGICAL, INTENT(OUT) :: noError
    INTEGER :: i
    REAL(wp) :: partialSum(1), temp(1), h
! Routine that computes an approximation to pi via numerical
! integration. Each process computes a partial sum of the
! terms required by the simple mid-point rule.
! The root process than gathers these together, computes the
! final approximation and then broadcasts it to all the other
! processes
    noError = .TRUE.
    h = one/(REAL(n, wp))
! All processes compute their partial sum based on the value
! of n and their rank
    timePsum = mpi_wtime()
    partialSum(1) = partSum()
    timePsum = mpi_wtime() - timePsum
! The root process now needs to collect the sum of all the
! partial values.
! Note: this is not the most efficient way of doing this but
! we wish to illustarte as many mpi routines as possible. See
! exercises for improving this.
    CALL mpi_reduce(partialSum, temp, 1, mpi_double_precision, &
         mpi_sum, rootId, mpi_comm_world, ierr)
    noError = noError .AND. (ierr == mpi_success)
    piApprox = temp(1)

    IF (myId == rootId) THEN
! The root process then performs the final scaling before 
! sending the result to all the other processes
      piApprox = four*h*piApprox
! Use a simple send and receive rather than a broadcast
! The root process sends the result to each process in turn
      DO i = 1, numProcs-1
        CALL mpi_send(piApprox, 1, mpi_double_precision, i, &
          rootId, mpi_comm_world, ierr)
        noError = noError .AND. (ierr == mpi_success)
      END DO
    ELSE
! The other processes pick up the value when it arrives. Each
! send needs a matching receive but each send/receive waits for 
! the other
      CALL mpi_recv(piApprox, 1, mpi_double_precision, rootId, &
        MPI_ANY_TAG, mpi_comm_world, recvStat, ierr)
      noError = noError .AND. (ierr == mpi_success)
    END IF

    CONTAINS

    FUNCTION partSum() RESULT(res)
    REAL(wp) :: res
    REAL(wp) :: h
    INTEGER :: i
! Form the partial sum based on n and the id of the process
    h = one / REAL(n, wp)

    res = zero
    DO i = myid+1, n, numProcs
      res = res + f(h*(REAL(i, wp) - half))
    END DO

    END FUNCTION partSum

    FUNCTION f(x) RESULT(res)
    REAL(wp), INTENT(IN) :: x
    REAL(wp) :: res

    res = one /(one + x*x)

    END FUNCTION f

    END SUBROUTINE computePi

    SUBROUTINE printPiApprox
    REAL(wp) :: pi
! Only the root process outputs the approximation and error
    IF (myId == rootId) THEN
      pi = four*atan(one)
      WRITE(*,'(a, i10, a / a, e16.8 / a, e16.8)') &
        'Using ', n, ' Integration intervals', &
        'Approximate value of pi: ', piApprox, &
        'Relative error:          ', ABS((pi-piApprox)/pi) 
    END IF

    END SUBROUTINE printPiApprox

    SUBROUTINE printTimes
! Accumulate the total time taken by all the processes in forming
! the aprtial sum. Print this along with the maximum and minimum
! times along with their process ids
    REAL(wp) :: allTimes(numProcs)
! Use gather to get the time from all the processes on the root
    CALL mpi_gather(timePsum, 1, mpi_double_precision, allTimes, &
      1, mpi_double_precision, rootId, mpi_comm_world, ierr)

      IF (myId == rootId) THEN
        WRITE(*, '(A, F10.2, A)') &
          'Accumulated time of all processes to compute partial sum:',&
          SUM(allTimes), ' seconds'
        WRITE(*, '(A, F10.2, A, i2)') &
          'Maximum time to compute partial sum: ', &
          MAXVAL(allTimes), ' on process ', MAXLOC(allTimes)
        WRITE(*, '(A, F10.2, A, i2)') &
          'Minimum time to compute partial sum: ', &
          MINVAL(allTimes), ' on process ', MINLOC(allTimes)
      END IF

    END SUBROUTINE printTimes

    SUBROUTINE runPiApprox
    LOGICAL :: noError, quit
! Set up mpi system, get user input, compute pi and distribute
! to other processes and output approx and timings
    CALL mpi_startup
    loop: &
    DO 
      CALL readIntBcast( &
          'Input number of integration intervals (<=0 to quit):', &
          n, quit)
      IF (quit) THEN
        EXIT loop
      ELSE
! Adjust value if necessary to ensure distinct points
        n = MIN(n, NINT(one/(SQRT(EPSILON(one)))))
        CALL computePi(noError)
        IF (noError) THEN
          CALL printPiApprox
          CALL printTimes
        ELSE
          IF (myId == rootId) THEN
            WRITE(*,'(A)') 'MPI error occurred during pi approximation'
          END IF
        END IF
      END IF
    END DO loop
    CALL mpi_shutdown

    END SUBROUTINE runPiApprox

    SUBROUTINE matVecMult(b, x, y)
    REAL(wp), INTENT(IN) :: b(:,:), x(:)
    REAL(wp), INTENT(OUT) :: y(:)
! Routine to compute the result of a matrix vector multiply
! where the matrix is partitioned by columns over the processes
! b(m,c1:c2), along with the same portion of the x vector
! x(c1:c2). Each process perform a straight matrix-vector multiply
! on the segment it has and these intermediate sums are gathered
! on the root process
    REAL(wp) :: z(m)
    INTEGER :: ierr

! Everybody do the partial sum. A straight matrix multiply
! is OK because all the sizes have been allocated correctly
    z = MATMUL(b, x)

! Collect all the partial sums to generate the final vector
    CALL mpi_reduce(z, y, m, mpi_double_precision, mpi_sum, &
         rootId, mpi_comm_world, ierr)

    END SUBROUTINE matVecMult

    SUBROUTINE genAndDistData(m, n, mxa, v, b, x, z)
    INTEGER, INTENT(IN) :: m, n
    REAL(wp), INTENT(OUT) :: mxa
    REAL(wp), ALLOCATABLE, INTENT(OUT) :: v(:), b(:,:), x(:), z(:)
! Routine to generate a full matrix and vector (a and v) for 
! checking purposes. It also allocates the correct space for 
! the partitions on all the process (b and x) and distributes
! the data from a/v to b/x.
    REAL(wp), ALLOCATABLE :: a(:,:)
    INTEGER :: nstart(0:numProcs-1), nsize(0:numProcs-1)

! Root process allocates a and v plus the result vector, y
! -- not required by other processes
    IF (myId == rootId) THEN
      ALLOCATE(a(m,n), v(n), z(m)) ! Should check this succeeds!
! Set random numbers
      CALL RANDOM_NUMBER(a)
      CALL RANDOM_NUMBER(v)
    END IF

! Work out the partitioning of the columns. Try and balance this
! distribution as far as possible
    CALL setColPart
! Everybody allocates their own partition to size
    ASSOCIATE (nz=>nsize(myId))
    ALLOCATE(b(m, nz), x(nz)) 
    END ASSOCIATE
! and then distribution portions of a/v to b/x to setup
! the partitioned matrices on all the processes
    CALL sendPartData

    CONTAINS

    SUBROUTINE setColPart
! Calculate the number of columns to be stored on each process
! (and the size 0f the section of the vector). We set this on
! all processes even though each only requires its own values.

    INTEGER :: i, nm1, inc

    nm1 = numProcs - 1
    inc = MAX(1, NINT(REAL(n,wp)/REAL(numProcs, wp)))

! nsize contains the number of columns oneach process
! nstart(i) .. nstart(i)+nsize(i)-1 is the range of columns
    nstart(0) = 1
    DO i = 1, nm1
      nsize(i-1) = inc
      nstart(i) = MIN(n+1, nstart(i-1)+inc)
    END DO

! The last process may be an odd man out
    IF (numProcs > 1) THEN
      nsize(nm1) = MAX(0, n-SUM(nsize(0:nm1-1)))
    ELSE
! Running on a single process; i.e., sequentially
      nsize(nm1) = n
    END IF
! Number of processes available may exceed the number of 
! columns to distribute
    WHERE (nstart > n) 
      nsize = 0
      nstart = n
    END WHERE

    END SUBROUTINE setColPart

    SUBROUTINE sendPartData
! Send the partition of data from a and v on the root process
! to the allocated arrays b and x on each process
! This is a copy on the root process
    INTEGER :: i, nm1, ierr
    LOGICAL :: complete
    INTEGER :: sendReqs(1:2*numProcs-2), recvReq, recaReq

    nm1 = numProcs - 1

    IF (myId == rootId) THEN
! Send data to each of the other processes in the group.
! We give an example here of the use of Isend (non-blocking
! send) and use an array of request flags so we can check when
! they have all finished
      DO i = 1, nm1
! Send out data to each process in the group. Isend does not
! wait for any send to complete before starting the next one
        ASSOCIATE (ns=>nstart(i), nz=>nsize(i))
        CALL mpi_isend(a(1:m, ns:ns+nz-1), m*nz,  &
           mpi_double_precision, i, i, mpi_comm_world, &
           sendReqs(2*i-1), ierr)
        CALL mpi_isend(v(ns:ns+nz-1), nz,  &
           mpi_double_precision, i, i, mpi_comm_world, &
           sendReqs(2*i), ierr)
        END ASSOCIATE
      END DO
! All the sends have been initiated by root. We can now do
! extra work while we wait for them to complete, so long as we
! avoid reusing the send buffers we are using
! Put root's data into the partitioned array
      b(1:m, 1:nsize(0)) = a(1:m, 1:nsize(0))
      x(1:nsize(0)) = v(1:nsize(0))
! Compute the model solution
      z = MATMUL(a, v)
! Now we have to wait until all the sends have completed
      complete = .FALSE.
      DO WHILE (.NOT. complete)
        CALL mpi_testall(2*nm1, sendReqs, complete, &
                         mpi_statuses_ignore, ierr)
      END DO
    ELSE
! Each process receives its part of the data
      CALL mpi_irecv(b(:,:), m*nsize(myId), mpi_double_precision, &
           rootId, myId, mpi_comm_world, recaReq, ierr)
      CALL mpi_irecv(x(:), nsize(myId), mpi_double_precision, &
           rootId, myId, mpi_comm_world, recvReq, ierr)
! Each process could now perform extra work that doesn't
! involve the send buffers while the receives finish.
! In this case both the above could have been replaced by mpi_recv
! calls since this is equivalent to a non-blocking call plus
! a completion test. non-blocking/blocking pairs are allowed in 
! any combination.
!
! We choose to illustarte two ways of testing for completion
! Just park and wait for the go-ahead
      CALL mpi_wait(recAReq, mpi_status_ignore, ierr)
! Or maybe do some further work and test when convenient
      complete = .FALSE.
      DO WHILE (.NOT. complete) 
        CALL mpi_test(recvReq, complete, mpi_status_ignore, ierr)
      END DO
    END IF

    END SUBROUTINE sendPartData

    END SUBROUTINE genAndDistData

! A routine to check the results of the distributed code
! against a straight matmul call
    SUBROUTINE checkRes(mxa, z, y)
    REAL(wp), INTENT(IN) :: mxa
    REAL(wp), INTENT(IN) :: y(:), z(:)
! Assume this routine is only called by the rootId
! Check that the product a*v matches the distributed result, y
    REAL(wp) ::  error
    
    error = MAXVAL(ABS(y-z))/(mxa + MAXVAL(ABS(z)))
    IF (error > SQRT(EPSILON(error))) THEN
      WRITE (*, '(A)')  &
      'Distributed matrix-vector product probably has serious errors'
    ELSE
      WRITE (*, '(A)')  'Distributed matrix-vector product agrees!'
    END IF

    END SUBROUTINE checkRes

    SUBROUTINE testMatVecMult
! Routine to run a simple test of the matrix vector product
! distributed code. Generate some random data, allocate space
! for the partitioned matrices, distribute from the root 
! process, perform the distributed matrix/vector multiply, and
! check the result with the result from a full matmul
    REAL(wp), ALLOCATABLE :: v(:), b(:,:), x(:), y(:), z(:)
    REAL(wp) :: mxa
    LOGICAL :: quit

    CALL mpi_startup
    quit = .FALSE.

loop: &
    DO
      CALL readIntBcast('Enter number of rows, m (<=0 to quit): ',&
           m, quit)
      IF (quit) EXIT loop
      CALL readIntBcast('Enter number of cols, n (<=0 to quit): ',&
           n, quit)
      IF (quit) EXIT loop
! Generate and distribute data
      CALL genAndDistData(m, n, mxa, v, b, x, z)
      IF (myId == rootId) THEN
! Generate the result vector on the root process
        ALLOCATE(y(m))
      ELSE
        ALLOCATE(y(1))
      END IF
      CALL matVecMult(b, x, y)

      IF (myId == rootId) THEN
        CALL checkRes(mxa, z, y)
      END IF
    END DO loop

    CALL mpi_shutdown

    END SUBROUTINE testMatVecMult


    END MODULE piApproxExample
