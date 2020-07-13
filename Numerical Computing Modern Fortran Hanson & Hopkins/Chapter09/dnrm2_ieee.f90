    MODULE dnrm2_ieee

      USE set_precision, ONLY: dkind

      USE, INTRINSIC :: IEEE_ARITHMETIC
      USE, INTRINSIC :: IEEE_EXCEPTIONS
! Be specific about the constants required from ieee_features
! to ensure that any partial implementation will have the
! required parts.
      USE, INTRINSIC :: IEEE_FEATURES, ONLY: IEEE_DIVIDE, IEEE_DENORMAL,&
      IEEE_DATATYPE, IEEE_HALTING, IEEE_INEXACT_FLAG, IEEE_INF,&
      IEEE_INVALID_FLAG, IEEE_NAN, IEEE_SQRT, IEEE_UNDERFLOW_FLAG

      IMPLICIT NONE
      PRIVATE

! This is the list of symbols used by dnrm2().
! Other symbols are not available without enlarging the list.
      PUBLIC dkind, dnrm2, dnrm2kc
      PUBLIC IEEE_ALL, IEEE_SET_FLAG, IEEE_GET_FLAG, IEEE_VALUE, &
        IEEE_POSITIVE_INF, IEEE_SIGNALING_NAN, IEEE_QUIET_NAN, &
        IEEE_NEGATIVE_INF, IEEE_IS_FINITE, IEEE_IS_NAN, IEEE_STATUS_TYPE, &
        IEEE_INEXACT, IEEE_UNDERFLOW, IEEE_OVERFLOW, IEEE_INVALID, &
        IEEE_DIVIDE_BY_ZERO, IEEE_SET_HALTING_MODE, IEEE_GET_HALTING_MODE, &
        IEEE_GET_STATUS, IEEE_SET_STATUS, IEEE_SUPPORT_FLAG, &
        IEEE_SUPPORT_DATATYPE, IEEE_SUPPORT_NAN, IEEE_SUPPORT_INF
    CONTAINS

      FUNCTION dnrm2kc(n, x, incx) RESULT (y)
      INTEGER, INTENT (IN) :: n, incx
      REAL (dkind), INTENT (IN) :: x(1:(n-1)*incx+1)
      REAL (dkind) y
! Communicated by W. Kahan, April, 2012 and based on a Matlab
! code given in 
! "Prescaling for a sum of squares in IEEE 754 Floating Point"
!
! The code needs a number of constants for scaling purposed
! For IEEE double precision these are all powers of 2
!    rttau = sqrt(tiny(zero)) = 2**(-511)
!    rtomega = sqrt(huge(zero)) = 2**(512)
!              use 2**1024 rather than huge(zero) which is the 
!              largest representable fp value.
!    Huge = hugeu = 1/(eps*rttau) = 2**563
!    huge = hugel =  rtomega = 2**512
!    tiny = tinyl = sqrt(eps)/rtomega = 2**(-538)
!    Tiny = tinyu = rttau/eps = 2**(-459)

      REAL (dkind), PARAMETER :: zero = 0.0E0_dkind, one = 1.0E0_dkind
      REAL (dkind), PARAMETER :: hugel = SCALE(one, 512), &
          hugeu = SCALE(one, 563), tinyl = SCALE(one, -538), &
          tinyu = SCALE(one, -459)

! Local variables:
      REAL (dkind) ahat, big, norm, s, sc, t
      INTEGER i

! This loop forces RETURN in one place,
! after the EXIT from the loop.
block:  DO
          IF (n==1) THEN
            y = ABS(x(1))
            EXIT block
          END IF
          big = hugel/sqrt(real(n,dkind))
          ahat = zero
          norm = zero
          s = zero
! The cases N <= 0 or INCX <= 0 were already eliminated.
          DO i = 1, (n-1)*incx + 1, incx
! If any component is infinite, the result is infinite.
! So just return with a positive infinite result.
            IF (.NOT. IEEE_IS_FINITE(x(i))) THEN
! Either Inf or NaN can trigger a .TRUE. above.
! If the result was an Inf, X(I) will not be a NaN.
              IF (IEEE_IS_NAN(x(i))) THEN
                s = IEEE_VALUE(s, IEEE_QUIET_NAN)
! Even though a NaN was seen there may still be Infs.
                CYCLE
              ELSE
! Now we know the result is an Inf; set value and quit.
                y = IEEE_VALUE(y, IEEE_POSITIVE_INF)
                EXIT block
              END IF
            ELSE
! May need scaling of the vector.
              ahat = max(ahat, ABS(x(i)))
            END IF
! Scaling is going to be needed.
            IF (ahat>=big) CYCLE
! This is the compensated summation.
            s = s + x(i)**2
            t = norm
            norm = t + s
            s = (t-norm) + s
          END DO ! DO I=1,(N-1)*INCX+1,INCX

! Filter out NaNs.  These may occur if there were
! NaNs in X(:); this test will not be reached
! if there was an Inf in array X(:).
          IF (IEEE_IS_NAN(s)) THEN
            y = s
            EXIT block
          END IF
!  Test whether scaling by  sc /= 1  is necessary:
          IF (ahat>=big) THEN
            sc = tinyl
          ELSE IF (ahat<=tinyu) THEN
            sc = hugeu
          ELSE
            y = SQRT(norm)
            EXIT block
          END IF
! Now scaling is required.  Another pass over the data is 
! made.  This may be a third pass, thanks to using the
! naive loop first.
          norm = zero
          s = zero

          CALL IEEE_SET_FLAG(IEEE_UNDERFLOW, .FALSE.)
          CALL IEEE_SET_FLAG(IEEE_INEXACT, .FALSE.)
          DO i = 1, (n-1)*incx + 1, incx
            s = s + (x(i)*sc)**2
            t = norm
            norm = t + s
            s = (t-norm) + s
          END DO
          y = SQRT(norm)/sc
          EXIT block
        END DO block
! All returns are right here.
      END FUNCTION dnrm2kc

      FUNCTION dnrm2(n, x, incx) RESULT (norm)

! An efficient routine for robustly computing the l_2 norm
! of a vector, x.  The code uses the IEEE intrinsic modules
! (when available) in Fortran 2003.

! The basic concept is that for the great majority of input
! vectors there is no need to resort to the testing or scaling required
! by Kahan's algorithm (or similar variants). Thus we attempt to
! compute the norm using a simple loop and only if this
! approach causes a NaN, overflow or underflow do we use the more
! complex approach.  The result is a saving in time for "normal"
! problems.  The computation of the initial sum of
! squares will complete in order for exception flags to be
! tested and remedial action to be taken.

! If the input vector contains Inf or -Inf then the result is 
! returned as Inf and no overflow flag is set. That distinguishes
! this case from the overflow case below.

! If the input vector contains NaN values and no Infs the result
! is returned as a quiet NaN and no flags are set. This agrees
! with the philosophy of propagating NaNs through a calculation.

! If the size of the vector is zero then a value of zero is returned.
! If INCX <= 0, a value of zero is returned (this is consistent with
! the original Blas level 1 routine) and the IEEE_INVALID flag
! is set.  This is not part of the
! specifications for DNRM2 but values of INCX must be positive
! If the norm of the input vector is computed greater than huge (i.e.,
! scaling cannot provide a valid computation) the answer is returned as
! Inf and the overflow flag is set.


        USE set_precision, ONLY: dkind

        INTEGER, INTENT (IN) :: n, incx
        REAL (dkind), INTENT (IN) :: x(1:(n-1)*incx+1)
        REAL (dkind) :: norm, s, t
        INTEGER :: i
        REAL (dkind), PARAMETER :: zero = 0.0E0_dkind

! A derived type from the IEEE modules for saving
! and then restoring the status on input.    
        TYPE (IEEE_STATUS_TYPE) :: input_status
! Use an array to store flag information
        INTEGER, PARAMETER :: overflow = 1, underflow = 2, inexact = 3
        LOGICAL :: signal(1:3)

        norm = zero
! Quick return if null array -- exact result
        IF (n<=0) RETURN
        IF (incx<=0) THEN
! A non-positive value of INCX is undefined.
! Set the IEEE_INVALID flag and return
          CALL IEEE_SET_FLAG(IEEE_INVALID, .TRUE.)
          RETURN
        END IF

block:  DO
! Save the current IEEE state, we may trigger an underflow or an
! overflow which we wish to trap and which we may later wish to
! unset if the computation proves possible by scaling.
          CALL IEEE_GET_STATUS(input_status)
          CALL IEEE_SET_HALTING_MODE(IEEE_ALL, .FALSE.)
          CALL IEEE_SET_FLAG(IEEE_ALL, .FALSE.)
          signal(1:3) = .FALSE.

! Deal with n == 1 as a special case
! Returns abs(x(1)) and this must be exact in the case of a normal
! value of x(1). For x(1) = +Inf or -Inf the value of +Inf is 
! and for x(1) = NaN a quiet NaN is returned. In all cases no
! flags signal.

          IF (n==1) THEN
            IF (IEEE_IS_NAN(x(1))) THEN
              norm = IEEE_VALUE(norm, IEEE_QUIET_NAN)
            ELSE IF (.NOT. IEEE_IS_FINITE(x(1))) THEN
              norm = IEEE_VALUE(norm, IEEE_POSITIVE_INF)
            ELSE
              norm = ABS(x(1))
            END IF
          ELSE

! Try a simple way.  Go through the entire loop.
            s = zero
            DO i = 1, (n-1)*incx + 1, incx
              s = s + x(i)**2
              t = norm
              norm = t + s
              s = (t-norm) + s
            END DO
! Retrieve the underflow and overflow status flags. If
! either of these are signaling then we have a genuine
! scaling problem and we call Kahan's algorithm.
            CALL IEEE_GET_FLAG(IEEE_OVERFLOW, signal(overflow))
            CALL IEEE_GET_FLAG(IEEE_UNDERFLOW, signal(underflow))
            IF (signal(overflow) .OR. signal(underflow) .OR. &
              IEEE_IS_NAN(norm) .OR. .NOT. iEEE_IS_FINITE(norm)) THEN
! If the input vector contained NaNs and no Infs then the
! value of norm is a NaN. 
! If there were any Infs and some NaNs, then norm will also be a NaN.
! So we must scan the data again if norm is a NaN.  

! The flags ieee_overflow, ieee_underflow also cause this scan.
! Reset flags -- any previously set were only being used
! to steer the computation internally
              signal( (/overflow,underflow,inexact/) ) = .FALSE.
              CALL IEEE_SET_FLAG(IEEE_OVERFLOW, .FALSE.)
              CALL IEEE_SET_FLAG(IEEE_UNDERFLOW, .FALSE.)
              CALL IEEE_SET_FLAG(IEEE_INEXACT, .FALSE.)
              norm = dnrm2kc(n, x, incx)
! NaN or Inf results are defined as exact.
              IF (IEEE_IS_NAN(norm) .OR. .NOT. IEEE_IS_FINITE(norm)) &
                CALL IEEE_SET_FLAG(IEEE_INEXACT, .FALSE.)
              EXIT block
            ELSE
! This is the normal place to compute the sqrt() of sum of
! squares of components. Most of the time the easy cases
! take this exit.
              norm = SQRT(norm)
            END IF
          END IF
          EXIT block

        END DO block
! Reset the IEEE state on entry plus any flags that must
! signal after this computation. 
        CALL IEEE_GET_FLAG(IEEE_OVERFLOW, signal(overflow))
        CALL IEEE_GET_FLAG(IEEE_UNDERFLOW, signal(underflow))
        CALL IEEE_GET_FLAG(IEEE_INEXACT, signal(inexact))

        CALL IEEE_SET_STATUS(input_status)

        IF (signal(overflow)) CALL IEEE_SET_FLAG(IEEE_OVERFLOW, .TRUE.)
        IF (signal(inexact)) CALL IEEE_SET_FLAG(IEEE_INEXACT, .TRUE.)
! Kahan cannot return an underflow and Kahan is called if
! the naive loop returns an undeflow. So no underflow
! signal may occur
!       IF (signal(underflow)) CALL IEEE_SET_FLAG(IEEE_UNDERFLOW, .TRUE.)
! All returns are here except INCX <= 0.

      END FUNCTION dnrm2

    END MODULE dnrm2_ieee
