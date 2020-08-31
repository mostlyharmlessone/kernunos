! Define the integrand function externally and use the
! passed values of mu, sigma:
    FUNCTION fexternal(x,ex) RESULT (y)
! Note that it is necessary to use-associate the
! defined extended base class and other symbols.    
      USE set_precision, ONLY : wp
      USE normalPdf, ONLY : quadpackbase, extype
      IMPLICIT NONE
      REAL (wp), PARAMETER :: one = 1.0E0_wp, two = 2.0E0_wp, &
        eight = 8.0E0_wp
      REAL (wp), INTENT (IN) :: x(:)
      CLASS (quadpackbase), INTENT (INOUT) :: ex
      REAL (wp) :: y(size(x))
! This is the integrand constant scale factor,
! s=1/sqrt(2*pi).     
      REAL (wp) :: s = one/sqrt(eight*atan(one))

      SELECT TYPE (ex)
      TYPE IS (extype)
        ASSOCIATE (mu => ex % mu, sigma => ex % sigma)
          y = (s/sigma)*exp(-(x-mu)**2/(two*sigma**2))
        END ASSOCIATE
      END SELECT

    END FUNCTION fexternal
