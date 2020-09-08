    PROGRAM approximatePi
      USE set_precision, ONLY : wp
      IMPLICIT NONE
      REAL(wp), PARAMETER :: zero = 0.0e0_wp, one = 1.0e0_wp, &
        four = 4.0E0_wp, half = 0.5e0_wp
! The number of panels and partial sums are coarrays
      INTEGER :: n[*]
      REAL (wp) :: partialSum[*]
! Variables that are local to all images
      INTEGER :: i, myimage, numim
      REAL (wp) :: h, pi, total
! Use the intrinsic functions this_image() and num_images() to get
! the number of this image and the total number of images
      myimage = this_image()
      numim = num_images()
      IF (myimage==1) THEN
        WRITE (*, '(A)') 'Input the number of quadrature points:'
        READ (*, *) n
      END IF  

! <= First synchronization point
! Other images wait here until the number of points is defined
      SYNC ALL
      n=n[1]
! Compute the panel size
      h = one/REAL(n, KIND=wp)
      partialSum = zero
! Compute individual pieces of the quadrature
      DO i = myimage, n, numim
        partialSum = partialSum + f(h*(REAL(i,KIND=wp)-half))
      END DO

! <= Second synchronization point
! Add the pieces from all the images when they are ready
      SYNC ALL 
      IF (myimage==1) THEN
        total = partialSum
        DO i = 2, numim
          total = total + partialSum[i]
        END DO
! Scale the approximation to get the approximate \pi
        total = four*h*total
        WRITE (*, *) 'Using ', num_images(),' images, and n = ', n
        WRITE (*, *) 'Approximate Value of PI = ', total
! Check the result with an accurate value of \pi
        pi = four*ATAN(one)
        WRITE (*, *) 'Relative error = ', (pi-total)/pi
      END IF

    CONTAINS

      FUNCTION f(x) RESULT (y)
        REAL (wp), INTENT(IN) :: x
        REAL (wp) :: y
        y = one/(one+x**2)
      END FUNCTION f
    END PROGRAM approximatePi
