    MODULE testModEx3
      USE set_precision, ONLY : wp
      USE quadpack2003, ONLY : quadpackbase, qag2003, fv

      IMPLICIT NONE

      REAL(wp), PARAMETER :: zero = 0.0E0_wp, half = 0.5E0_wp, &
                one = 1.0E0_wp, two = 2.0E0_wp
! The Type Extension declaration: MY_COMPLEX_DATA goes here.
      TYPE, EXTENDS (quadpackbase) :: my_complex_data
! The integrated function is z**-alpha and the interval of integration
! is normally [0, PI] on the upper unit circle.

        COMPLEX (wp) :: alpha
      END TYPE my_complex_data
! Followed by:
    CONTAINS
      FUNCTION my_complex(t,extend) RESULT (y)
        IMPLICIT NONE
        REAL (wp), INTENT (IN) :: t(:)

        CLASS (quadpackbase), INTENT (INOUT) :: extend
        REAL (wp) :: y(SIZE(t),extend%ncomponents)
        COMPLEX (wp) z(SIZE(t)), w(SIZE(t))
        SELECT TYPE (EXTEND)
          TYPE IS (my_complex_data)
            ASSOCIATE(alpha => extend%alpha)
! Evaluation of z**-alpha, integrating around the upper 
! half of the unit circle. The value of alpha is complex 

! This is the z value on the unit circle, exp(i*t)            
            z(:) = CMPLX(COS(t),SIN(t),wp)
! This is z**-alpha, intent is a principal branch             
            w = z**(-alpha)
! This defines the differential, dz(t) = i*exp(i*t)dt,
! i=sqrt(-1), i.e. = CMPLX(zero,one,wp)
            w = CMPLX(zero,one,wp)*z*w
! Return integrand result as a vector quantity:
! real then imaginary parts             
            y(:,1) = REAL(w,wp)
            y(:,2) = AIMAG(w)
            END ASSOCIATE
         END SELECT
      END FUNCTION my_complex
    END MODULE testModEx3
