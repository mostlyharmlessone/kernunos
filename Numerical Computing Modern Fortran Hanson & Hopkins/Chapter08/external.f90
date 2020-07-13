! Define the integrand function externally and use the
! passed values of mu, sigma:
    FUNCTION fexternal(x,ex) RESULT (y)
! Note that it is necessary to use-associate the
! defined extended base class and other symbols.    
      USE normal_pdf, ONLY : quadpackbase, wp, fs, qag2003, extype
      REAL (wp), PARAMETER :: one = 1.0E0_wp, two = 2.0E0_wp
      IMPLICIT NONE
      REAL (wp), INTENT (IN) :: x(:)

      CLASS (quadpackbase), INTENT (INOUT) :: ex
      REAL (wp) :: y(size(x))
! This is the integrand constant scale factor,
! s=1/sqrt(2*pi).     
      REAL (wp) :: s = one/sqrt(8._wp*atan(one))

      SELECT TYPE (ex)
      TYPE is(extype)
        ASSOCIATE (mu => ex % mu, sigma => ex % sigma)
          y = (s/sigma)*exp(-(x-mu)**2/(two*sigma**2))
        END ASSOCIATE
      END SELECT

    END FUNCTION
