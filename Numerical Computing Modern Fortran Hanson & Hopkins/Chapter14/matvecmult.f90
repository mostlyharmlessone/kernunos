    PROGRAM matvecmul
      USE set_precision, ONLY : wp
      IMPLICIT NONE
! Compute matrix-vector product y = A*x.
      INTEGER :: i, ip,itemp, j, k, myimage, numim,&
              nv, nvlast, nvrest
      LOGICAL ALLOC_FAIL
      REAL(wp), ALLOCATABLE :: b(:, :), y(:), w(:)
      REAL(wp) :: error
! Declare coarrays
      INTEGER :: istat[*], m[*], n[*]
      REAL(wp), ALLOCATABLE :: a(:,:)[:], x(:)[:], z(:)[:]

! Use intrinsic functions this_image() and num_images() to get the
! number of this image and the total number of images.
      myimage = this_image()
      numim = num_images()

BLOCK: &
      DO
        IF (myimage==1) THEN
          WRITE (*, '(I0, A)') numim, ' images being used'
          WRITE (*, '(A)') 'Input m, the number of rows (<=0 to quit): '
          READ (*, *) m
          WRITE (*, '(A)') 'Input n, the number of cols (<=0 to quit): '
          READ (*, *) n
        END IF

! <= A synchronization point
! Other images wait here for matrix dimensions to be defined by input.
        SYNC ALL 
! Copy m,n to the other images:
        m = m[1]
        n = n[1]

! All images exit if m and/or n are <= 0
        IF (m <= 0 .OR. n <= 0) THEN
          istat = 0
          EXIT BLOCK
        END IF

! Compute the partition -- extra columns on image num_images()
! Note: this is slightly different to the MPI implementation
! nv = number of columns on this image
! nvlast = number of columns on image num_images()
! nvrest = number of columns on images 1, 2, ..., num_images()-1
        nvrest = n/numim
! More images that columns -- exit
        IF (nvrest == 0) THEN
          IF (myimage == 1) THEN
            WRITE(*, '(''More images than columns!'')')
            WRITE(*, '(''Number of columns: '', i6)') n
            WRITE(*, '(''Number of images: '', i6)') numim
          END IF
          istat = 0
          EXIT BLOCK
        END IF

        nvlast = nvrest + MOD(n, numim)
        IF (myimage == numim) THEN
          nv = nvlast
        ELSE
          nv = nvrest
        END IF
        SYNC ALL ! <= Synchronization point 
! Allocate the various arrays required making sure all coarrays
! are of the same size -- max partition size = nvlast (number of
! columns assigned to image num_image())
        ALLOCATE (a(m,nvlast)[*], w(nvlast), z(m)[*], x(n)[*], y(m), &
                  STAT=istat)
        SYNC ALL ! <= Synchronization point
! Do an All-to-All transfer of an allocation failure logical .OR.
! that occurred on any image.  This avoids a hangup.
        ALLOC_FAIL = (istat /= 0)
        DO j=1,myimage-1
          ALLOC_FAIL=ALLOC_FAIL .or. (istat[j] /= 0)
        END DO
        DO J=myimage+1, numim
          ALLOC_FAIL=ALLOC_FAIL .or. (istat[j] /= 0)
        END DO
        IF (ALLOC_FAIL) EXIT BLOCK

        CALL populateAandx()

! Copy the components of the x vector on image 1 to the alternate 
! images 2,...  N. B. Images need to wait until x is ready.
        SYNC ALL ! <= A synchronization point
        ip =  (myimage - 1)*nvrest
        w(1:nv) = x(ip+1:ip+nv)[1]

        SYNC ALL ! <= A synchronization point
! Compute a piece of the entire matrix-vector product. Each image
! computes with the columns it owns.
! This operation is potentially parallel.
        z(:) = MATMUL(a(:,1:nv), w(1:nv))
        IF (myimage==1) THEN
! Add the partial products from each image.
! This occurs on image 1 only.
          y(:) = z(:)
          DO j = 2, numim
            y(:) = y(:) + z(:)[j]
          END DO

! Check the results on image number 1. This is to verify that 
! the process is correct, and normally would not be done.
          ALLOCATE (b(m,n), STAT=istat)
          IF (istat /= 0) EXIT BLOCK
! Move the columns of A from image 1.
          b(:, 1:nvrest) = a
! Move the A arrays on images > 1 to consecutive columns in B
! on image 1.
          DO j = 2, numim - 1
            ip = (j - 1)*nvrest
            b(:, ip+1:ip+nvrest) = a(:,1:nvrest)[j]
          END DO
! Last image has more columns
          b(:, n-nvlast+1:n) = a[numim]  ! This is the whole matrix a 

! Compute matrix-vector product after all
! columns have been copied to image number 1.
          z = MATMUL(b, x)
! Y and Z should agree to within rounding errors.
          error = MAXVAL(ABS(y-z))/(MAXVAL(ABS(b)) + MAXVAL(ABS(z)))
          IF (error < SQRT(EPSILON(error))) THEN
            WRITE(*,'(A,1pE12.4)') &
                 'Satisfactory Error Check on IMAGE 1 = ', error
          ELSE
            WRITE(*,'(A,1pE12.4)') &
                 'Error: Large Error Detected on IMAGE 1 = ', error
          END IF
        END IF
        EXIT BLOCK
      END DO BLOCK

      CRITICAL
        IF (istat /= 0) &
          WRITE(*,'(A, I0)') &
               'Memory allocation failure on IMAGE = ',myimage
      END CRITICAL

      CONTAINS

      SUBROUTINE  populateAandx()
      INTEGER :: i, j, randl
      INTEGER, ALLOCATABLE :: iseed(:)

! Generate different random number sequences on each image.
! Otherwise each sequence might be identical.
! Get the size of the implementation dependent seed array
! and allocate space for it.
      CALL RANDOM_SEED(SIZE=randl)
      ALLOCATE (iseed(randl), STAT=istat)
 
      CALL SYSTEM_CLOCK(COUNT=i)
      DO j = 1, randl
! N. B. This is an arbitrary choice for a random number seed.
! Some dependence on the image number is needed to assure a 
! different sequence on each image.
        iseed(j) = i*myimage + 2*j + 1
      END DO
      CALL RANDOM_SEED(PUT=iseed)
! Populate the local copies of A and x
      CALL RANDOM_NUMBER(a(:,1:nv))
      IF (myimage==1) THEN
        CALL random_number(x)
      END IF
      END SUBROUTINE populateAandx
    END PROGRAM matvecmul
