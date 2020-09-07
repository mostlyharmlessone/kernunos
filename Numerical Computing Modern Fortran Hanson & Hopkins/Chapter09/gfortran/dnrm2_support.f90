  MODULE dnrm2_support
  USE set_precision, ONLY : dkind
!
! Module containing an implementation of Blue's algorithm
! for computing the Euclidean norm of a vector. This 
! routine is more reliable that the basic dnrm2 code
! supplied in the blas level 1 implementation and should be
! used when there is no support for ieee arithmetic.
!
! If ieee arithmetic support is available the hybrid
! naive/Kahan algorithm should be used. This code is
! available in the module dnrm2_ieee.

    IMPLICIT NONE
    PUBLIC :: dnrm2_blue

    CONTAINS

      FUNCTION dnrm2_blue(n, x, incx) RESULT (norm)
        INTEGER, INTENT (IN) :: n, incx
        REAL (dkind), INTENT (IN) :: x(1:(n-1)*incx+1)
        REAL (dkind) :: norm

! For IEEE arithmetic we can precompute the various constants
! required for the correct scaling to takeplace. Using the
! notation of Blue's paper
!     b = 2**(-511); B = bu = 2**(485); 
!     s = 2**(-512); S = su = 2**(538);
!     sqrteps = 2**(-26)
! The constant required to detect a true overflow is given by
!     R/S = huge(.)/S = 2**(1023)*(2-2**(-53))/2**(538)
!         = 2**(485)*(2-epsilon(.))
! Some constants are initialized as reciprocals so division is
! avoided.

        REAL (dkind), PARAMETER :: two = 2.0E0_dkind, one = 1.0E0_dkind, &
          zero = 0.0E0_dkind
!     REAL(dkind), PARAMETER :: b=SCALE(one, -511), s=SCALE(one, -512), &
!                       bu=SCALE(one, 485), su=SCALE(one, 538), &
!                       rs=SCALE(one, 512),rsu=SCALE(one, -538),&
!                       oflimit=bu*(two-epsilon(one)), &
!                       sqrteps=SCALE(one, -26)

        REAL (dkind) :: b, s, bu, su, rs, rsu, oflimit, sqrteps

        INTEGER, PARAMETER :: small = 1, med = 2, big = 3
        REAL (dkind) :: a(3), ymin, xi
        INTEGER :: i

! Fast return if n < 1 or incx < 1
        IF (n<1 .OR. incx<1) THEN
          norm = zero
          RETURN
        END IF

! Blue's algorithm must scale so we know that either a(big) or a(small)
! will be non-zero. Note: the computation can sill overflow but it can't
! underflow

        b = SCALE(one, -511)
        s = SCALE(one, -512)
        bu = SCALE(one, 485)
        su = SCALE(one, 538)
        rs = SCALE(one, 512)
        rsu = SCALE(one, -538)
        oflimit = bu*(two-EPSILON(one))
        sqrteps = SCALE(one, -26)

        a = zero
        DO i = 1, (n-1)*incx + 1, incx
          xi = ABS(x(i))
          IF (xi>bu) THEN
! Scale to try and avoid overflow
            a(big) = a(big) + (xi*rsu)**2
          ELSE IF (xi<b) THEN
! Scale to try and avoid underflow
            a(small) = a(small) + (xi*rs)**2
          ELSE
            a(med) = a(med) + xi**2
          END IF
        END DO

        IF (a(big)>zero) THEN
          a(big) = SQRT(a(big))
          IF (a(big)>oflimit) THEN
! The result may still overflow when we scale it back up
            norm = HUGE(zero)
            RETURN
          END IF
          IF (a(med)>zero) THEN
            norm = a(big)*su
            a(med) = SQRT(a(med))
          ELSE
            norm = a(big)*su
            RETURN
          END IF

! There is no way that both a(big) and a(small) can both be usefully > zero
        ELSE IF (a(small)>zero) THEN
          IF (a(med)>zero) THEN
            a(med) = SQRT(a(med))
            norm = SQRT(a(small))*s
          ELSE
            norm = SQRT(a(small))*s
            RETURN
          END IF
        ELSE
          norm = SQRT(a(med))
          RETURN
        END IF

! There may still be a contribution from a(med) that will count
! towards the correct value of the return value
        IF (a(med)>zero) THEN
          ymin = min(a(med), norm)
          norm = max(a(med), norm)
          IF (ymin>sqrteps*norm) norm = norm*SQRT(one+(ymin/norm)**2)
        END IF

      END FUNCTION dnrm2_blue
  END MODULE dnrm2_support
