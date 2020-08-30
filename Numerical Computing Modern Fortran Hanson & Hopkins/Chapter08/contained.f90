    PROGRAM document_example
      USE normal_pdf, ONLY : extype, fmodule
      USE set_quadpack_data, ONLY : quadpackbase, qag2003, wp, fs
! Define a variable of the extended type:
      TYPE (extype) t
      REAL (wp), PARAMETER :: one = 1.0E0_wp, two = 2.0E0_wp

! These are the values of mu, sigma used:
      REAL (wp) :: mu1 = one, sigma1 = two
      REAL (wp) approx1, approx2, approx3

! Since FEXTERNAL is not a module routine,
! nor a contained routine, 
! it is necessary to specify its abstract interface.
      PROCEDURE(fs) :: fexternal
! This improves readability, by simplifying the
! symbols associated with derived types.

      ASSOCIATE (mu => t % mu, sigma => t % sigma)
! Evaluate the integral with three methods of
! defining the routine that gives values of
! the integrand function.

! Place values of mu and sigma in the extended type
! components:
      mu = mu1
      sigma = sigma1

! Approximate the integral using a module that
! contains the function FMODULE.  
      CALL qag2003(fmodule,t,mu-one,mu+one,approx1)

! A second alternate packaging is to contain the
! evaluation function FCONTAINED in this
! program unit.  
      CALL qag2003(fcontained,t,mu-one,mu+one,approx2)

! A third packaging method is to use 
! an external function:
      CALL qag2003(fexternal,t,mu-one,mu+one,approx3)
      IF (t%ier==0 .AND. (approx1==approx2) .AND. (approx1==approx3)) &
        THEN
        WRITE (*,'(A,F5.2,A,F5.2/A,F7.3)') &
          ' Normal cumulative for mu = ', mu,  ' sigma = ', &
          sigma, ' within limits [mu-1,m+1] is = ', approx1
      END IF
      END ASSOCIATE

    CONTAINS
! Define the integrand function and use the
! passed values of mu, sigma:
      FUNCTION fcontained(x,ex) RESULT (y)
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
! Expected results:    
! Normal cumulative for mu =  1.00 sigma =  2.00
! within limits [mu-1,m+1] is =   0.383 
    END PROGRAM document_example
