    PROGRAM testEx1fs
      USE set_precision, ONLY : wp
      USE quadpack2003, ONLY : qag2003
      USE testModEx1fs, ONLY : my_f, my_quad_data

      IMPLICIT NONE
      REAL (wp), PARAMETER :: zero = 0.0E0_wp, one = 1.0E0_wp, &
        pi = 4.0E0_wp*ATAN(one)
      TYPE (my_quad_data) :: my_data
      REAL (wp) :: intapprox

      my_data%omega = 100.0E0_wp
      CALL qag2003(my_f,my_data,zero,pi,intapprox)
      WRITE (*,'(A,F9.6)') ' Integral (PI*J_0(100)) is  = ', intapprox

! Expected result with WP=kind(1.D0):
! Integral (PI*J_0(100)) is  =  0.062787

    END PROGRAM testEx1fs
