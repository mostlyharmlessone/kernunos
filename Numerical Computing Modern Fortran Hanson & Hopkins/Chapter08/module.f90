    MODULE normal_pdf
! This demonstrates the use of QAG2003 by integrating
! the function s*exp(-(x-mu)**2/(2*sigma**2)), 
! s=1/sqrt(2*pi*sigma**2).  The integration limits are
! from mu-1 to mu+1.  
      USE set_quadpack_data, ONLY : quadpackbase, wp, fs, qag2003
      REAL (wp), PARAMETER :: zero = 0.0E0_wp, one = 1.0E0_wp, &
        two = 2.0E0_wp

! Extend the base class so that mu and sigma are passed
! to the evaluation function, FMODULE(x(:), EXTEND).
      TYPE, EXTENDS (quadpackbase) :: extype
        REAL (wp) :: mu = zero
        REAL (wp) :: sigma = one
      END TYPE extype

    CONTAINS
! Define the integrand function using the supplied
! values of mu, sigma
      FUNCTION fmodule(x,ex) RESULT (y)
        IMPLICIT NONE
        REAL (wp), INTENT (IN) :: x(:)

        CLASS (quadpackbase), INTENT (INOUT) :: ex
        REAL (wp) :: y(size(x))
! This is the integrand constant scale factor,
! s=1/sqrt(2*pi).        
        REAL (wp), PARAMETER :: s = one/sqrt(8._wp*atan(one))
        SELECT TYPE (ex)
        TYPE is(extype)
          ASSOCIATE (mu => ex % mu, sigma => ex % sigma)
            y = (s/sigma)*exp(-(x-mu)**2/(two*sigma**2))
          END ASSOCIATE
        END SELECT
      END FUNCTION
    END MODULE
