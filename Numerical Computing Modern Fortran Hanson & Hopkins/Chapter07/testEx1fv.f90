    PROGRAM testEx1fv
      USE set_precision, ONLY : wp
      USE quadpack2003, ONLY : qag2003
      USE testModEx1fs, ONLY : my_f, my_quad_data

      IMPLICIT NONE
      REAL (wp), PARAMETER :: zero = 0.0E0_wp, one = 1.0E0_wp, &
        pi = 4.0E0_wp*ATAN(one)
      INTEGER, PARAMETER :: nvalues = 100
      INTEGER :: i
      TYPE (my_quad_data) :: my_data(0:nvalues)
      REAL (wp) :: intapprox(0:nvalues)

      my_data(:)%omega = (/ (REAL(i,wp),i=0,nvalues) /)
!$OMP PARALLEL DO
      DO i = 0, nvalues
        CALL qag2003(my_f,my_data(i),zero,pi,intapprox(i))
      END DO
!$OMP END PARALLEL DO
      DO i = 0, nvalues
        WRITE (*,'(A,F9.6,A,I4)') 'Integral (PI*J_0(i)) is  = ', &
          intapprox(i), ', with i = ', i
      END DO
    END PROGRAM testEx1fv
