MODULE FORTRAN_TO_MPI
USE, INTRINSIC :: ISO_C_BINDING
IMPLICIT NONE
! This is support for data types used by the Fortran to
! C calls in MPICH2.
TYPE,BIND(C):: MPI_Status
   INTEGER(c_int) :: statusCount
   INTEGER(c_int) :: canceled
! These are the three mandatory components
   INTEGER(c_int) :: mpi_source
   INTEGER(c_int) :: mpi_tag
   INTEGER(c_int) :: mpi_error
END TYPE
! Some other versions of MPI_Status:

!OpenMPI 1.3
!* MPI_Status
!struct ompi_status_public_t {
!int MPI_SOURCE;
!int MPI_TAG;
!int MPI_ERROR;
!int _count;
!int _CANCELED;
!};

!  MVAPICH is a flavor of MPICH:
!#define MPI_STATUS_SIZE 4 

!typedef struct { 
!int count;
!int MPI_SOURCE;
!int MPI_TAG;
!int MPI_ERROR;
!#if (MPI_STATUS_SIZE > 4)
!int extra[MPI_STATUS_SIZE - 4];
!#endif
!} MPI_Status;

!MacMPI:
!typedef struct {
!   int source;
!   int tag;
!   int error;
!   int len;
!   int type;
!} MPI_Status;


TYPE, BIND(C):: MPI_Request
  INTEGER(C_INT) Request
END TYPE

! Basic module for direct calls to the C bindings for MPI - C RULES!
! Supported routines from the core set are interfaced to 
! their underlying C codes.


INTERFACE
    
    FUNCTION MPI_GET_CVALUE(string)BIND(C,NAME='MPI_Value')RESULT(VALUE)
        IMPORT C_PTR
        TYPE(C_PTR), VALUE :: string
        INTEGER VALUE
    END FUNCTION MPI_GET_CVALUE
    
    FUNCTION MPI_GET_VERSION(VERSION, SUBVERSION)&
        BIND(C,NAME='MPI_Get_version')RESULT(IERR)
        IMPORT C_INT        
        INTEGER(C_INT), INTENT(OUT) :: VERSION, SUBVERSION
        INTEGER(C_INT) IERR
    END FUNCTION MPI_GET_VERSION
       
    FUNCTION MPI_INIT(NUMBER, COMMAND_LINE)BIND(C,Name='MPI_Init')RESULT(IERR)
        IMPORT C_PTR, C_INT
        INTEGER(C_INT),INTENT(IN) :: NUMBER
        INTEGER(C_INT) IERR
        TYPE(C_PTR) COMMAND_LINE
    END FUNCTION MPI_INIT
    
    FUNCTION MPI_FINALIZE()BIND(C,Name='MPI_Finalize')RESULT(IERR)
        IMPORT C_INT
        INTEGER(C_INT) IERR
    END FUNCTION MPI_FINALIZE
    
    FUNCTION MPI_ABORT(COMM, ERRORCODE)BIND(C,Name='MPI_Abort')RESULT(IERR)
        IMPORT C_INT
        INTEGER(C_INT), VALUE, INTENT(IN) :: COMM
        INTEGER(C_INT), VALUE, INTENT(IN) :: ERRORCODE
        INTEGER(C_INT) IERR
    END FUNCTION MPI_ABORT
    
    FUNCTION MPI_BARRIER(COMM) BIND(C,Name='MPI_Barrier') RESULT(IERR)
        IMPORT C_INT
        INTEGER(C_INT), VALUE, INTENT(IN) :: COMM
        INTEGER(C_INT) IERR
    END FUNCTION MPI_BARRIER
        
    FUNCTION MPI_INITIALIZED(FLAG)BIND(C,Name='MPI_Initialized')RESULT(IERR)
        IMPORT C_INT
        INTEGER(C_INT),INTENT(OUT) :: FLAG
        INTEGER(C_INT) IERR
    END FUNCTION MPI_INITIALIZED
    
    FUNCTION MPI_COMM_SIZE(COMM, NUMBER)BIND(C,Name='MPI_Comm_size')RESULT(IERR)
        IMPORT C_INT
        INTEGER(C_INT), VALUE :: COMM
        INTEGER(C_INT), INTENT(OUT) :: NUMBER
        INTEGER(C_INT) IERR
    END FUNCTION MPI_COMM_SIZE
    
    FUNCTION MPI_COMM_RANK(COMM, NODE)BIND(C,Name='MPI_Comm_rank')RESULT(IERR)
        IMPORT C_INT
        INTEGER(C_INT), VALUE :: COMM
        INTEGER(C_INT),INTENT(OUT) :: NODE
        INTEGER(C_INT) IERR
    END FUNCTION MPI_COMM_RANK
    
    FUNCTION MPI_COMM_DUP(COMM, NEWCOMM)BIND(C,Name='MPI_Comm_dup')RESULT(IERR)
        IMPORT C_INT
        INTEGER(C_INT),INTENT(IN), VALUE :: COMM
        INTEGER(C_INT),INTENT(OUT) :: NEWCOMM
        INTEGER(C_INT) IERR
    END FUNCTION MPI_COMM_DUP
    
    FUNCTION MPI_COMM_COMPARE(COMM1,COMM2, IRESULT)&
        BIND(C,Name='MPI_Comm_compare')RESULT(IERR)
        IMPORT C_INT
        INTEGER(C_INT),INTENT(IN), VALUE :: COMM1, COMM2
        INTEGER(C_INT),INTENT(OUT) :: IRESULT
        INTEGER(C_INT) IERR
    END FUNCTION MPI_COMM_COMPARE
    
    FUNCTION MPI_GET_PROCESSOR_NAME(NAME, NAME_SIZE)&
             BIND(C,Name='MPI_Get_processor_name')RESULT(IERR)
        IMPORT C_INT, C_CHAR
        CHARACTER(C_CHAR), INTENT(OUT) :: NAME(*)
        INTEGER(C_INT) :: NAME_SIZE, IERR
    END FUNCTION MPI_GET_PROCESSOR_NAME
    
    FUNCTION MPI_WTIME()BIND(C,Name='MPI_Wtime')RESULT(SECONDS)
        IMPORT C_DOUBLE
        REAL(C_DOUBLE) SECONDS
    END FUNCTION MPI_WTIME
    
    FUNCTION MPI_WTICK()BIND(C,Name='MPI_Wtick')RESULT(RESOLUTION)
        IMPORT C_DOUBLE
        REAL(C_DOUBLE) RESOLUTION
    END FUNCTION MPI_WTICK
    
    FUNCTION MPI_GATHER(SENDBUF, SENDCOUNT, SENDTYPE,&
             RECVBUF, RECVCOUNT, RECVTYPE, ROOT, COMM)&
             BIND(C,Name='MPI_Gather')RESULT(IERR)
             IMPORT C_INT, C_PTR
             INTEGER(C_INT), INTENT(IN), VALUE :: SENDCOUNT,&
                             SENDTYPE, RECVCOUNT, RECVTYPE, ROOT, COMM
             TYPE(C_PTR),INTENT(IN),  VALUE :: SENDBUF
             TYPE(C_PTR),INTENT(IN), VALUE :: RECVBUF
             INTEGER(C_INT) IERR
    END FUNCTION MPI_GATHER
    
    FUNCTION MPI_GATHERV(SENDBUF, SENDCOUNT, SENDTYPE,&
             RECVBUF, RECVCOUNT, DISPL,&
             RECVTYPE, ROOT, COMM)&
             BIND(C,Name='MPI_Gatherv')RESULT(IERR)
             IMPORT C_INT, C_PTR
             INTEGER(C_INT), INTENT(IN), VALUE :: SENDCOUNT,&
                             SENDTYPE, RECVTYPE, ROOT, COMM
             TYPE(C_PTR),INTENT(IN),  VALUE :: SENDBUF
             TYPE(C_PTR),INTENT(IN), VALUE :: RECVBUF
             INTEGER(C_INT),INTENT(IN) :: RECVCOUNT(*), DISPL(*)
             INTEGER(C_INT) IERR
    END FUNCTION MPI_GATHERV
    
    FUNCTION MPI_ALLGATHER(SENDBUF, SENDCOUNT, SENDTYPE,&
             RECVBUF, RECVCOUNT, RECVTYPE, COMM)&
             BIND(C,Name='MPI_Allgather')RESULT(IERR)
             IMPORT C_INT, C_PTR
             INTEGER(C_INT), INTENT(IN), VALUE :: SENDCOUNT,&
                             SENDTYPE, RECVCOUNT, RECVTYPE, COMM
             TYPE(C_PTR),INTENT(IN),  VALUE :: SENDBUF
             TYPE(C_PTR),INTENT(IN), VALUE :: RECVBUF
             INTEGER(C_INT) IERR
    END FUNCTION MPI_ALLGATHER
    
    FUNCTION MPI_ALLGATHERV(SENDBUF, SENDCOUNT, SENDTYPE,&
             RECVBUF, RECVCOUNTS, DISPLS, RECVTYPE, COMM)&
             BIND(C,Name='MPI_Allgatherv')RESULT(IERR)
             IMPORT C_INT, C_PTR
             INTEGER(C_INT), INTENT(IN), VALUE :: SENDCOUNT,&
                             SENDTYPE, RECVTYPE, COMM
             INTEGER(C_INT), INTENT(IN) :: RECVCOUNTS, DISPLS                             
             TYPE(C_PTR),INTENT(IN),  VALUE :: SENDBUF
             TYPE(C_PTR),INTENT(IN), VALUE :: RECVBUF
             INTEGER(C_INT) IERR
    END FUNCTION MPI_ALLGATHERV
    FUNCTION MPI_SCATTER (SENDBUF, SENDCOUNT, SENDTYPE, &
                          RECVBUF, RECVCOUNT, RECVTYPE, &
                          ROOT, COMM)BIND(C,Name='MPI_Scatter')RESULT(IERR)
             IMPORT C_INT, C_PTR
             TYPE(C_PTR), INTENT(IN), VALUE :: SENDBUF, RECVBUF
             INTEGER(C_INT), INTENT(IN), VALUE :: SENDCOUNT, SENDTYPE
             INTEGER(C_INT), INTENT(IN), VALUE :: RECVCOUNT, RECVTYPE
             INTEGER(C_INT), INTENT(IN), VALUE :: ROOT, COMM
             INTEGER(C_INT) IERR
    END FUNCTION MPI_SCATTER
    FUNCTION MPI_SCATTERV(SENDBUF, SENDCOUNT, DISPL, SENDTYPE,&
             RECVBUF, RECVCOUNT, RECVTYPE, &
             ROOT, COMM) BIND(C,Name='MPI_Scatterv')RESULT(IERR)
             IMPORT C_INT, C_PTR
             INTEGER(C_INT), INTENT(IN) :: SENDCOUNT(*), DISPL(*)
             INTEGER(C_INT), INTENT(IN), VALUE :: SENDTYPE, ROOT, COMM
             TYPE(C_PTR),INTENT(IN), VALUE :: SENDBUF, RECVBUF
             INTEGER(C_INT),INTENT(IN), VALUE :: RECVCOUNT, RECVTYPE
             INTEGER(C_INT) IERR
    END FUNCTION MPI_SCATTERV

    FUNCTION MPI_BCAST(BUF, COUNT, DATATYPE, TAG, COMM)&
        BIND(C,NAME='MPI_Bcast')RESULT(IERR)
        IMPORT C_INT, C_PTR
        TYPE (C_PTR), VALUE :: BUF
        INTEGER(C_INT), INTENT(IN), VALUE :: &
          COUNT, DATATYPE, TAG, COMM
        INTEGER(C_INT) IERR
    END FUNCTION MPI_BCAST
                    
    FUNCTION mpi_send(buf, bufCount, datatype, dest, tag, comm) &
                     BIND(C,NAME='MPI_Send') RESULT(ierr)
      IMPORT c_int, c_ptr
      TYPE (c_ptr), VALUE :: buf
      INTEGER(c_int), INTENT(IN), VALUE :: bufCount, datatype, &
                                           dest, tag, comm
      INTEGER(c_int) :: ierr
    END FUNCTION mpi_send    
! The STATUS argument can have a derived type or an integer as the
! last argument.  So a generic interface is provided that allows for
! either of these two argument types in that position.
   FUNCTION mpi_recv(buf, bufCount, datatype, source, tag, &
                    comm, status) &
                    BIND(C,NAME='MPI_Recv') RESULT(ierr)
     IMPORT c_int, c_ptr
     TYPE (c_ptr), VALUE :: buf
     INTEGER(c_int), INTENT(IN), VALUE :: bufCount, datatype, &
                                          source, tag, comm
     TYPE(c_ptr), VALUE :: status
     INTEGER(c_int) :: ierr
   END FUNCTION mpi_recv
    
    FUNCTION MPI_REDUCE(BUF, OPVALUE, COUNT, DATATYPE, OPERATION, TAG, COMM)&
        BIND(C,NAME='MPI_Reduce')RESULT(IERR)
        IMPORT C_INT, C_PTR
        TYPE (C_PTR), VALUE :: BUF, OPVALUE
        INTEGER(C_INT), INTENT(IN), VALUE :: &
          COUNT, DATATYPE, OPERATION, TAG, COMM
        INTEGER(C_INT) IERR
    END FUNCTION MPI_REDUCE
    
    FUNCTION MPI_ALLREDUCE(SENDBUF, RECBUF, COUNT, DATATYPE, OPERATION, COMM)&
        BIND(C,NAME='MPI_Allreduce')RESULT(IERR)
        IMPORT C_INT, C_PTR
        TYPE (C_PTR), VALUE :: SENDBUF, RECBUF
        INTEGER(C_INT), INTENT(IN), VALUE :: &
          COUNT, DATATYPE, OPERATION, COMM
        INTEGER(C_INT) IERR
    END FUNCTION MPI_ALLREDUCE
     
    FUNCTION MPI_IRECV(BUF, COUNT, DATATYPE, SOURCE, TAG, COMM, REQUEST)&
        BIND(C,NAME='MPI_Irecv')RESULT(IERR)
        IMPORT C_INT, C_PTR, MPI_Request
        TYPE (C_PTR), VALUE :: BUF
        INTEGER(C_INT), INTENT(IN), VALUE :: &
          COUNT, DATATYPE, SOURCE, TAG, COMM
        TYPE(MPI_Request) :: REQUEST
        INTEGER(C_INT) IERR
    END FUNCTION MPI_IRECV
    
    FUNCTION MPI_ISEND(BUF, COUNT, DATATYPE, DEST, TAG, COMM, REQUEST)&
        BIND(C,NAME='MPI_Isend')RESULT(IERR)
        IMPORT C_INT, C_PTR, MPI_Request
        TYPE (C_PTR), VALUE :: BUF
        INTEGER(C_INT), INTENT(IN), VALUE :: &
          COUNT, DATATYPE, DEST, TAG, COMM
        TYPE(MPI_Request) :: REQUEST
        INTEGER(C_INT) IERR
    END FUNCTION MPI_ISEND
    
    FUNCTION MPI_TEST(REQUEST, FLAG, STATUS)BIND(C,Name='MPI_Test')&
             RESULT(IERR)
       IMPORT C_INT, MPI_Request,C_PTR !, MPI_Status
       TYPE(MPI_Request), INTENT(IN) :: Request
       INTEGER(C_INT) FLAG
       !TYPE(MPI_Status), INTENT(INOUT) :: Status
       TYPE(C_PTR), VALUE :: STATUS
       INTEGER(C_INT) IERR
    END FUNCTION MPI_TEST
        
    
    FUNCTION MPI_WAIT(REQUEST, STATUS)BIND(C,Name='MPI_Wait')&
             RESULT(IERR)
       IMPORT C_INT, MPI_Request, C_PTR, MPI_Status
       TYPE(MPI_Request), INTENT(IN) :: Request
       !TYPE(MPI_Status), INTENT(INOUT) :: Status
       TYPE(C_PTR), VALUE :: STATUS
       INTEGER(C_INT) IERR
    END FUNCTION MPI_WAIT
        
END INTERFACE

CONTAINS

    FUNCTION MPI_VALUE(Fortran_Characters)RESULT(VALUE)
! This function is used to find the MPI parameter values.
! With this function the Fortran codes use the C MPI bindings.    
    USE, INTRINSIC :: ISO_C_BINDING, only: &
        C_CHAR, C_NULL_CHAR, C_INT
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: Fortran_Characters
    INTEGER(C_INT) VALUE
    INTEGER I, K
    CHARACTER(C_CHAR), ALLOCATABLE, TARGET :: Cstring(:)
    LOGICAL :: DEBUG=.FALSE.
! Fortran Character data and C string data
! are interoperable by creating a C string
! from a Character literal, etc. 
! Note that alpha characters are mapped to upper case.
! That way the Fortran code can use either case for the
! names of the values.
    K=len(UPCASE(trim(Fortran_Characters)))+1
    ALLOCATE (Cstring( K))  
    DO I=1,K-1
       Cstring(I)=Fortran_Characters(I:I)
    END DO
    IF(DEBUG) THEN
       WRITE(*,*) 'Number of character = ',K
       WRITE(*,*) 'Fortran Characters = ',Fortran_Characters(1:K-1)
    END IF
    Cstring(K)=C_NULL_CHAR
    VALUE=MPI_Get_CValue(c_loc(Cstring))
    
    END FUNCTION MPI_VALUE
    
    FUNCTION UPCASE(STRING) RESULT(UPPER)
    CHARACTER(LEN=*), INTENT(IN) :: STRING
    CHARACTER(LEN=LEN(STRING)) :: UPPER
    INTEGER :: J
    DO J = 1,LEN(STRING)
      IF(STRING(J:J) >= "a" .AND. STRING(J:J) <= "z") THEN
           UPPER(J:J) = ACHAR(IACHAR(STRING(J:J)) - 32)
      ELSE
           UPPER(J:J) = STRING(J:J)
      END IF
    END DO
    END FUNCTION UPCASE   
END MODULE
