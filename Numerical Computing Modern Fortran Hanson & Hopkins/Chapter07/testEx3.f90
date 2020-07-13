    PROGRAM testEx3
      USE set_precision, ONLY : wp
      USE quadpack2003, ONLY : qag2003
      USE testModEx3, ONLY : my_complex, my_complex_data

      IMPLICIT NONE
      REAL (wp), PARAMETER :: zero = 0.0E0_wp, one = 1.0E0_wp, &
        half = 0.5E0_wp, pi = 4.0E0_wp*ATAN(one)
      TYPE (my_complex_data) my_data
      REAL (wp) :: c(2) ! Result is a complex value.
      ASSOCIATE(alpha  => my_data%alpha,&
                abserr => my_data%abserr,&
                ier    => my_data%ier)

      alpha = CMPLX(half,zero,wp)

      CALL qag2003(my_complex,my_data,zero,pi,c)

      WRITE (*,'(/A, 2F10.6)') ' Complex Value of ALPHA = ', alpha
      WRITE (*,'(A/20x,1pG12.4,A, 1pG12.4)') &
        ' Upper Unit Circle Integral of 1/z**ALPHA is  = ', c(1),&
        '  + I *', c(2)
      WRITE (*,*) ' IER from line integration = ', ier
      WRITE (*,'(A,1pG12.4)') &
        ' Line integral max. error estimate = ', abserr
      END ASSOCIATE

! Expected results:  
! Complex Value of ALPHA =   0.500000  0.000000
! Upper Unit Circle Integral of 1/z**ALPHA is  =
!                      -2.000      + I *   2.000
!  IER from line integration =            0
! Line integral max. error estimate =   2.2204E-14

    END PROGRAM testEx3

