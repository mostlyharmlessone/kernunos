MODULE MY_MPI_MODULE
    USE FORTRAN_TO_MPI
    USE SET_PRECISION
    IMPLICIT NONE
    
    INTEGER, PARAMETER :: MAXLINE=1024, NUMBER=0
    CHARACTER(LEN=MAXLINE) COMMAND_LINE
    CHARACTER(C_CHAR), ALLOCATABLE, TARGET :: CLINE(:), Name(:)
    INTEGER MPI_COMM_WORLD, MPI_INTEGER, MPI_DOUBLE_PRECISION, MPI_SUM,&
            MPI_MAX_PROCESSOR_NAME, MPI_SUCCESS, &
            MYNODE, NNODES, SIZE, LENGTH, VERSION, SUBVERSION
    INTEGER, TARGET :: MPI_STATUS_IGNORE        
CONTAINS
    FUNCTION MY_MPI_INIT()RESULT(IERR)
    INTEGER IERR
    INTEGER I
! Get named parameters compatible with C version
! in this environment:
    MPI_COMM_WORLD         = MPI_Value('MPI_COMM_WORLD')
    MPI_INTEGER            = MPI_Value('MPI_INTEGER')
    MPI_DOUBLE_PRECISION   = MPI_Value('MPI_DOUBLE_PRECISION')
    MPI_SUM                = MPI_Value('MPI_SUM')
    MPI_MAX_PROCESSOR_NAME = MPI_Value('MPI_MAX_PROCESSOR_NAME')
    MPI_SUCCESS            = MPI_Value('MPI_SUCCESS')
    MPI_STATUS_IGNORE      = MPI_Value('MPI_STATUS_IGNORE')
! Call Fortran 2003 intrinsic subroutine to get command line:
    call get_command(COMMAND_LINE, LENGTH, IERR)
! Convert to a C string variable:
    ALLOCATE(CLINE(LENGTH+1))
    DO I=1,LENGTH
      CLINE(I)=COMMAND_LINE(I:I)
    END DO
    CLINE(LENGTH+1)= C_NULL_CHAR
! Initialize MPI: (The first arg is 0 or 1).
    IERR = MPI_INIT(1, C_LOC(CLINE))    
! Get the number of nodes and the individual node index:
    IERR = MPI_COMM_SIZE(MPI_COMM_WORLD,NNODES)
    IERR = MPI_COMM_RANK(MPI_COMM_WORLD,MYNODE)
! Get the version of MPI being used:    
    IERR=MPI_GET_VERSION(VERSION, SUBVERSION)

! Allocate space for holding the name:
    ALLOCATE(Name(MPI_MAX_PROCESSOR_NAME))
! Demonstrate getting the processor name and its size,
! <= MPI_MAX_PROCESSOR_NAME.
    IERR = MPI_Get_processor_name(Name, SIZE)        
    END FUNCTION MY_MPI_INIT
END MODULE

program main
    USE MY_MPI_MODULE
!**********************************************************************
!   f2003corecall.f90 - 

!   f2003pi:
!   (A) compute pi by integrating f(x) = 1/(1 + x**2)     
!   Based on a similar code from:
!  (C) 2001 by Argonne National Laboratory.
!     
!   Each node: 
!    1) receives the number of rectangles used in the approximation.
!    2) calculates the areas of it's rectangles.
!    3) Synchronizes for a global summation.
!   Node 0 reads the number of rectangles and prints the result.

!   f2003mv: 
!   (B) compute matrix-vector products by work-sharing
!   Each node:
!    1) receives the number of rows and columns for the matrix
!    2) computes the index and number of columns it will use
!    3) receives a piece of a vector it will use
!    4) computes a piece of the matrix-vector product
!   Node 0 receives sums the pieces, printing sizes and elapsed time.
!       Initially node 0 generates the entire  matrix and sends
!       pieces to each node. The results are compared for errors.
CALL f2003pi
! All running processes in MPI_COMM_WORLD hold:
   IERR=MPI_BARRIER(MPI_COMM_WORLD)
   IF(MYNODE==0) THEN
! Note that all processes reached barrier A:
     WRITE(*,'(A//)')"All communicator members passed barrier A"    
   END IF
   
CALL f2003mv
! All running processes in MPI_COMM_WORLD hold:
   IERR=MPI_BARRIER(MPI_COMM_WORLD)
   IF(MYNODE==0) THEN
! Note that all processes reached barrier A:
     WRITE(*,'(A)')"All communicator members passed barrier B"    
   END IF
! Shut down MPI and stop: 
    IERR = MPI_FINALIZE()
end program

subroutine f2003pi
USE MY_MPI_MODULE
IMPLICIT NONE

INTEGER :: ANYERROR, I, IERR,  INIT, IOERR, N
INTEGER, TARGET ::  IFLAGS(4), ITEMP(2)
REAL(DKIND), TARGET :: SUM, PIAPPROX, DTEMP(2), PI
REAL(DKIND) :: ONE=1.D0, H
REAL(DKIND) :: TIME_compute, TIME_Total
INTEGER, ALLOCATABLE, TARGET :: GLOBAL_FLAGS(:)
REAL(DKIND), ALLOCATABLE, TARGET :: NODE_TIMES(:,:)
TYPE(MPI_STATUS), TARGET :: STATUS

! Initialize MPI, if needed.
IERR = MPI_INITIALIZED(INIT)
IF(INIT==0)IERR = MY_MPI_INIT()

ALLOCATE(GLOBAL_FLAGS(4*NNODES),NODE_TIMES(2,NNODES))

! Start all error flags set to no errors:
IFLAGS=MPI_SUCCESS
GLOBAL_FLAGS=MPI_SUCCESS
DO
! See if any node had a previous message passing error.
! The logic applied here is to see if any node had an error
! for the broadcasts or reduction in the last loop execution.
     IERR=MPI_Allgather(c_loc(IFLAGS), 4, MPI_INTEGER,&
          c_LOC(GLOBAL_FLAGS), 4, MPI_INTEGER, MPI_COMM_WORLD)
     ANYERROR = maxval(abs(GLOBAL_FLAGS))
! End of error processing for previous errors.     
    
    IF(MYNODE==0) THEN
      IF(ANYERROR /= MPI_SUCCESS) THEN
        write(*,'(A/(4I5/))')"ERROR FLAGS, per Node", Global_Flags
        WRITE(*,'(A)')"Message Passage or Reduction Error - Quit"       
      ELSE
        WRITE(*,'(/A,I2,".",I1)')&
        "Using MPI Version ",VERSION,SUBVERSION
        WRITE(*,'(A,256A1)')&
        "The root node has the processor name ", Name(1:Size)
        WRITE(*,'(A/)',ADVANCE='No') & 
        "Enter the number of integration intervals: (a value <=0 or text quits))>>"
    ! Read the integer value but get an IOSTAT that may note errors.
    ! Such errors usually come from entering non-integer text.  
        READ(*,'(I10)',IOSTAT=IOERR) N
        N=MIN(N,NINT(ONE/sqrt(EPSILON(SUM))))
      END IF
    END IF
    

    ! Quit if a tranmission error happened.
      IF(ANYERROR /= MPI_SUCCESS) &
        Ierr=MPI_Abort(MPI_COMM_WORLD, ANYERROR)
    ! Send the the number of intervals and any I/O error stat to all.
      ITEMP=(/N, IOERR/)
      TIME_TOTAL=MPI_WTIME()
      IFLAGS(1)=MPI_Bcast(C_LOC(ITEMP), 2, MPI_INTEGER, 0, MPI_COMM_WORLD)
    ! Extract the number of intervals and the error flag.  
      N=ITEMP(1); IOERR=ITEMP(2)
      
    ! If there was a non-positive integer value entered or
    ! any non-integer character, exit the loop and quit.
    ! This is the usual way of quitting.  
      IF(N <= 0 .OR. IOERR /= 0) EXIT
      TIME_Compute = MPI_WTIME()
    ! Perform a single node's piece of the numerical quadrature.  
      H=ONE/REAL(N,KIND(H))
      SUM=0.D0
    ! Perform a piece of the mid-point integration formula
    ! at this node.  The product factor 4*H is applied only by the
    ! root node.
      DO I=MYNODE+1, N, NNODES
        SUM = SUM + F(H * (REAL(I,KIND(H)) - 0.5D0))
      END DO
      TIME_Compute=MPI_WTIME()-TIME_COMPUTE 
! Send the sum of approximate quadrature values to
! all nodes.  Node 0 scales by 4*H and prints results:      
      IFLAGS(2)=MPI_Allreduce(C_LOC(SUM),C_LOC(PIAPPROX),1,MPI_DOUBLE_PRECISION,&
                MPI_SUM, MPI_COMM_WORLD)
      TIME_TOTAL=MPI_WTIME()-TIME_TOTAL 
      
      DTEMP=(/TIME_COMPUTE, TIME_TOTAL/)
      IFLAGS(3)=MPI_Gather(c_loc(DTEMP), 2, MPI_DOUBLE_PRECISION, &
           C_LOC(NODE_TIMES),2, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD)         
      IF(MYNODE==0) THEN
    ! Scale reduced sum result by 4*H to get approximation to PI:  
          PIAPPROX=4.D0*H*PIAPPROX
          
    ! Get a working precision accurate value of PI:      
          PI=4.D0*ATAN(ONE)
    ! Show results and relative error:      
          WRITE(*,'(A, 1PD20.14/A,1PD14.6)') & 
          "Approximate value of PI = ", PIAPPROX,&
          "Relative Error = ",(PI-PIAPPROX)/PI
          WRITE(*,'(A)') &
          'Computation and then Total Time (S) - up to 5 nodes'
          WRITE(*,'(5I14)')(I-1,I=1,min(5,NNODES))
          WRITE(*,'(1p5D14.4)')(NODE_TIMES(1,I),I=1,min(5,NNODES))
          WRITE(*,'(1p5D14.4)')(NODE_TIMES(2,I),I=1,min(5,NNODES))
      END IF 
! Send the value of PI to each NODE with MPI_SEND.
! A TAG value is arbitrarily set to the node destination.
! This same TAG value must match for the node receiving.
  IF(MYNODE==0) THEN
      
      DO I=1,NNODES-1
         IFLAGS(4)=MPI_SEND(c_loc(PI), 1, MPI_DOUBLE_PRECISION, I,&
              I, MPI_COMM_WORLD)
      END DO
  ELSE
      IF(NNODES > 1) THEN
! Each node with a rank > 0 receives SUM,PI with MPI_RECV.
! Each node accepts these values corresponding to its TAG value.
         IFLAGS(4)=MPI_RECV(c_loc(PI), 1, MPI_DOUBLE_PRECISION, 0, &
              MYNODE, MPI_COMM_WORLD, c_LOC(STATUS))
       END IF 
  END IF                      
END DO


CONTAINS
! This is the function being integrated.  It has the value PI/4
! with integration limits from 0 to infinity.
  FUNCTION F(X) RESULT(Y)
      REAL(KIND(H)) X,Y
      Y=ONE/(ONE+X**2)
  END FUNCTION
END SUBROUTINE

SUBROUTINE f2003mv
USE MY_MPI_MODULE
IMPLICIT NONE

INTEGER I, K, M, N, IERR, INC, INIT, IOERRM, IOERRN, IWAIT
        
INTEGER, TARGET :: ITEMP(4)
INTEGER, ALLOCATABLE :: NSIZE(:), NSTART(:), DISPL(:), &
                        RECVCOUNT(:)
REAL(DKIND), ALLOCATABLE, TARGET :: A(:,:), B(:,:), Y(:)
REAL(DKIND), ALLOCATABLE, TARGET :: AG(:,:), X(:), Z(:)
TYPE(MPI_STATUS), TARGET :: STATUS
TYPE(MPI_REQUEST) REQUEST
REAL(DKIND) ERROR, NORMA, NORMZ, TIMES, TIMEE, TIMETOTAL

! Initialize MPI, if needed
IERR = MPI_INITIALIZED(INIT)
IF(INIT==0)IERR = MY_MPI_INIT()
IF(MYNODE==0) THEN
        WRITE(*,'(A/)',ADVANCE='No') & 
        "For Matrix - Vector products, enter the number of matrix rows, M: >>"
    ! Read the integer value but get an IOSTAT that may note errors.
    ! Such errors usually come from entering non-integer text.  
        READ(*,'(I10)',IOSTAT=IOERRM) M

        WRITE(*,'(A/)',ADVANCE='No') & 
        "Enter the number of matrix cols, N: >>"
        READ(*,'(I10)',IOSTAT=IOERRN) N
END IF

ITEMP=(/M,N,IOERRM,IOERRN/)
! Send sizes and I/O flags to all nodes:
IERR=MPI_Bcast(C_LOC(ITEMP), 4, MPI_INTEGER, 0, MPI_COMM_WORLD)
IF(ANY(ITEMP(3:4) /= 0)) RETURN ! Had an I/O error or bad input.
M=ITEMP(1); N=ITEMP(2)
IF(M <= 0 .or. N <= 0) RETURN  ! Matrix size is undefined

! Allocate and define the check matrix at the root node.
IF(MYNODE == 0) THEN
    ALLOCATE(A(M,N))
    CALL RANDOM_NUMBER(A)
END IF
! Compute the information for columns that each node will 
! use in computing matrix-vector products.  The values
! of NSIZE(:) and NSTART(:) are chosen to balance
! the problem size at each node.
   INC=max(1,nint(REAL(N,DKIND)/NNODES))
   ALLOCATE(NSIZE(0:NNODES-1), NSTART(0:NNODES-1),&
            RECVCOUNT(0:NNODES-1), DISPL(0:NNODES-1))
   NSTART(0)=1
   DO I=1,NNODES-1
       NSTART(I)=min(N+1,NSTART(I-1)+INC)
       NSIZE(I-1)=INC
   END DO
   NSIZE(NNODES-1)=N
   IF(NNODES > 1) NSIZE(NNODES-1)=max(0,N-SUM(NSIZE(0:NNODES-2)))
   
   WHERE(NSTART > N)
     NSIZE=0
     NSTART=N
   END WHERE

! Allocate local arrays for partitions of the matrix A.
   ALLOCATE(B(M,NSIZE(MYNODE)),AG(M,NNODES),STAT=IERR)
   ALLOCATE(Y(M),X(N),Z(M),STAT=IERR)
! Send the parts of the partitioned matrix from the
! root node to the rest of the group.
   IF(MYNODE==0) THEN
       DO I=1,NNODES-1
       IERR=MPI_ISEND(c_loc(A(1,NSTART(I))), M*NSIZE(I), &
            MPI_DOUBLE_PRECISION, I, I, MPI_COMM_WORLD, REQUEST)
       END DO
! Node 0 holds on to part of the large matrix.            
       B(:,1:NSIZE(0))=A(:,1:NSIZE(0))
   ELSE
       IERR=MPI_IRECV(c_loc(B),M*NSIZE(MYNODE), MPI_DOUBLE_PRECISION, 0, &
                  MYNODE, MPI_COMM_WORLD, REQUEST) 
   END IF 
! Now enter the part of the algorithm that generates an array
! X(:) at the root node and asynchronously sends partitions
! to the nodes.  Each node computes Y=B*X(1:nsize(I)).  The
! result is reduced using the SUM operation at the root node.
    IF(NNODES > 1)IERR=MPI_WAIT(REQUEST, c_LOC(STATUS))
    
    TIMETOTAL=0.D0
    DO K=1,10
    TIMES=MPI_WTIME()
    IF(MYNODE==0) THEN
       CALL RANDOM_NUMBER(X)
       DO I=1,NNODES-1
! Start the send operation for the segments of X
! corresponding to those columns at the nodes.       
       IERR=MPI_ISEND(c_loc(X(NSTART(I))), NSIZE(I), &
            MPI_DOUBLE_PRECISION, I, I, MPI_COMM_WORLD, REQUEST)
       END DO
! The root node starts computing, possibly before the send
! opertions are completed.  
       Z=matmul(B,X(1:NSIZE(0)))
   ELSE
       IERR=MPI_IRECV(c_loc(X),NSIZE(MYNODE), MPI_DOUBLE_PRECISION, 0, &
                  MYNODE, MPI_COMM_WORLD, REQUEST)
       IWAIT=0
! Sit in a busy loop until the receive is finished.       
       DO WHILE(IWAIT==0)
         IERR=MPI_TEST(REQUEST, IWAIT, c_loc(MPI_STATUS_IGNORE))
       END DO 
! Note that the segment of vector x arrives in the first NSIZE(MYNODE)
! entries of the array X. Must wait until it arrives before computing.      
       Z=matmul(B,X(1:NSIZE(MYNODE))) 
   END IF
! Add all partial products of partitions.     
       IERR=MPI_Reduce(C_LOC(Z),C_LOC(Y),M,MPI_DOUBLE_PRECISION,&
                MPI_SUM, 0, MPI_COMM_WORLD)
       TIMEE=MPI_WTIME()
! Total the time taken by the distributed computation.
! The time to check the result is not totaled.  Note that the check
! values are computed with MATMUL.
       TIMETOTAL=TIMETOTAL+(TIMEE-TIMES)
                        
! Now Y=A*X should == Z, the TRUE result.  It will not agree
! exactly due to rounding errors.  Check that there are
! no serious errors or blunders.
       IF(MYNODE==0 .and. K == 1) THEN
           Z=matmul(A,X)
           NORMA=maxval(ABS(A))
           NORMZ=maxval(ABS(Z))
           ERROR=maxval(ABS(Y-Z))/(NORMA+NORMZ)
! The ERROR value will be about EPSILON(ERROR).  But
! this larger tolerance catches a serious problem:           
           IF(ERROR > SQRT(EPSILON(ERROR))) THEN
             IOERRM=1
           ELSE
             IOERRM=0
           END IF
       END IF
    END DO ! K                   
    
    IF(MYNODE==0)THEN
      WRITE(*,'(A/(4I8,1PD12.4))')&
      "Dimensions M,N, #Repeats, #Nodes, Avg Matrix-vector product Time  =",&
            M,N,K-1,NNODES,TIMETOTAL/REAL(K-1,DKIND)
            
      IF(IOERRM == 0) THEN
        WRITE(*,'(A)')" Distributed matrix-vector products are correct."
      ELSE
        WRITE(*,'(A)')" A distributed matrix-vector product has serious errors."
      END IF
    END IF            
END SUBROUTINE

!  Sample Input – Using 4 Nodes
! 1000
! 0
! 10000 1000
! 
!  Sample Output – Times are not repeatable
! Using MPI Version  2.1
! The root node has the processor name machine14
! Enter the number of integration intervals: (a value <=0 or text quits))>>
! Approximate value of PI = 3.14159273692313D+00
! Relative Error =  -2.652582D-08
! Computation and then Total Time (S) - up to 5 nodes
!             0             1             2             3
!    5.0068D-06    3.8147D-06    4.0531D-06    4.0531D-06
!    4.9114D-05    2.3317D-04    2.6202D-04    2.2888D-04
! All communicator members passed barrier A
! For Matrix - Vector products, enter the number of matrix rows, M: >>
! Enter the number of matrix cols, N: >>
! Dimensions M,K, #Repeats, #Nodes, Avg Matrix-vector product Time  =
!   10000    1000      10       4  1.7935D-02
!  Distributed matrix-vector products are correct.
! All communicator members passed barrier B
