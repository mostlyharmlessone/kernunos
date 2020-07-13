    MODULE dnrm2_testmod
      USE dnrm2_ieee

      IMPLICIT NONE

      TYPE expectedresult
        LOGICAL :: expectedflags(5)
        REAL (dkind) :: expectednorm
      END TYPE expectedresult

      INTEGER n, incx
      INTEGER :: nx = 1000, last_test = 20
      CHARACTER (LEN=96) testdescript

    CONTAINS

      SUBROUTINE indivtest(testno, x, res, nVal, incxVal)

        REAL (dkind), INTENT (IN) :: x(:)
        INTEGER, INTENT (IN) :: testno
        TYPE (expectedresult), INTENT (IN) :: res
        INTEGER, OPTIONAL :: nVal, incxVal
        REAL (dkind) :: norm, relerr, want
        REAL (dkind), PARAMETER :: zero = 0.0E0_dkind
        LOGICAL :: flagsset(5)
        INTEGER :: i

        CHARACTER (LEN=*), PARAMETER :: flagtype(5) = (/ 'overflow      ', &
          'divide_by_zero', 'invalid       ', 'underflow     ', &
          'inexact       ' /)

! Execute the function
        CALL IEEE_SET_FLAG(IEEE_ALL, .FALSE.)
        IF (PRESENT(nVal)) THEN
          n = nVal
        ELSE
          n = size(x)
        END IF

        IF (PRESENT(incxVal)) THEN
          incx = incxVal
        ELSE
          incx = 1
        END IF

        norm = dnrm2(n, x, incx)

        CALL IEEE_GET_FLAG(IEEE_ALL, flagsset)

        want = res%expectednorm

        WRITE (*, '(a, i3, " -- ", A)') 'Test No: ', testno, &
          trim(testdescript)
        testdescript = ''
        IF (IEEE_IS_FINITE(want)) THEN
          IF (.NOT. IEEE_IS_FINITE(norm)) THEN
            WRITE (*, '(''Infinite result returned. Expected: '',e16.8)') want

          ELSE IF (testno/=last_test) THEN
            IF (res%expectednorm == zero) THEN
              relerr = abs(norm)
            ELSE
              relerr = abs((norm-res%expectednorm)/res%expectednorm)
            END IF
            WRITE (*, '(3d16.8, /A)') res%expectednorm, norm, relerr, &
              ' Values Expected, Returned, Relative Error with Expected'
          END IF
        ELSE
          IF (IEEE_IS_FINITE(norm)) THEN
            WRITE (*, '(''  Expected infinite result, got: '',e16.8)') norm
          ELSE
            WRITE (*, '(''  Returned expected infinite result'')')
          END IF
! If both are infinite keep quiet
        END IF
! Check for support of all IEEE_USUAL flags.
! Check flags as expected
        DO i = 1, 5
! Don't check the result if the compiler does not support it.
          SELECT CASE (i)
          CASE (1)
            IF (.NOT. IEEE_SUPPORT_FLAG(IEEE_OVERFLOW,zero)) CYCLE
          CASE (2)
            IF (.NOT. IEEE_SUPPORT_FLAG(IEEE_DIVIDE_BY_ZERO,zero)) CYCLE
          CASE (3)
            IF (.NOT. IEEE_SUPPORT_FLAG(IEEE_INVALID,zero)) CYCLE
          CASE (4)
            IF (.NOT. IEEE_SUPPORT_FLAG(IEEE_UNDERFLOW,zero)) CYCLE
          CASE (5)
            IF (.NOT. IEEE_SUPPORT_FLAG(IEEE_INEXACT,zero)) CYCLE
          END SELECT

          IF (res%expectedflags(i) .NEQV. flagsset(i)) THEN
            WRITE (*, '(a, '' flag incorrectly set. Expected: '',l1)') &
              flagtype(i), res%expectedflags(i)
          END IF
        END DO
        WRITE (*, *)

      END SUBROUTINE indivtest

      SUBROUTINE indivnftest(testno, x, nan, inf)
!USE, INTRINSIC :: IEEE_EXCEPTIONS, ONLY : IEEE_ALL, IEEE_SET_FLAG, &
!IEEE_GET_FLAG

        REAL (dkind), INTENT (IN) :: x(:)
        LOGICAL, OPTIONAL :: nan, inf
        INTEGER, INTENT (IN) :: testno
        REAL (dkind) :: norm
        LOGICAL :: flagsset(5)
        INTEGER :: i

        CHARACTER (LEN=*), PARAMETER :: flagtype(size(IEEE_ALL)) = (/ &
          'overflow      ', 'divide_by_zero', 'invalid       ', &
          'underflow     ', 'inexact       ' /)

        WRITE (*, '(a, i4, 1x, A)') 'Test No: ', testno, trim(testdescript)
        testdescript = ''
! Execute the function
        CALL IEEE_SET_FLAG(IEEE_ALL, .FALSE.)
        norm = dnrm2(size(x), x, 1)
        CALL IEEE_GET_FLAG(IEEE_ALL, flagsset)

        IF (PRESENT(nan)) THEN
          IF (.NOT. IEEE_IS_NAN(norm)) THEN
            WRITE (*, '(''NaN result expected but not returned. '''// &
              '''Result = '', e16.8)') norm
          ELSE
            WRITE (*, '(''  Returned expected NaN result'')')
          END IF
        ELSE IF (PRESENT(inf)) THEN
          IF (IEEE_IS_FINITE(norm)) THEN
            WRITE (*, '(''Inf result expected but not returned. '''// &
              '''Result = '', e16.8)') norm
          ELSE
            WRITE (*, '(''  Returned expected infinite result'')')
          END IF
        END IF

! Check flags -- none should have been set
! Complain if any flags are true
        DO i = 1, size(IEEE_ALL)
          IF (flagsset(i)) THEN
            WRITE (*, '(a, '' flag incorrectly set. '')') flagtype(i)
          END IF
        END DO
        WRITE (*, *)

      END SUBROUTINE indivnftest

      SUBROUTINE runtests

        TYPE (expectedresult) :: res
        REAL (dkind), ALLOCATABLE :: x(:)
        REAL (dkind) :: a, v, infVal
        REAL (dkind), PARAMETER :: two = 2.0E0_dkind, one = 1.E0_dkind, &
          zero = 0.E0_dkind
        INTEGER :: testno, i, intv
        LOGICAL :: nan, inf

        CALL IEEE_SET_HALTING_MODE(IEEE_ALL, .FALSE.)

        testno = 0
        infVal = IEEE_VALUE(zero, IEEE_POSITIVE_INF)

! Test 1
! Null array -- should return zero. Should not access the array x.
        ALLOCATE (x(0))
        res = expectedresult( (/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./), &
          zero)
        testno = testno + 1
        testdescript = 'n <= 0 signifies null array'
        CALL indivtest(testno, x, res, nVal=0)
        DEALLOCATE (x)

! Test 2
! Illegal value of incx -- should return zero and set invalid flag??
        ALLOCATE (x(1))
        res = expectedresult( (/.FALSE.,.FALSE.,.TRUE.,.FALSE.,.FALSE./), &
          zero)
        testno = testno + 1
        testdescript = 'Illegal value of incx'
        CALL indivtest(testno, x, res, incxVal=0)
        DEALLOCATE (x)

! Test 3
! Vector of length > 1 -- positive infinity
        ALLOCATE (x(3))
        x = (/ two, IEEE_VALUE(one,IEEE_POSITIVE_INF), -one /)
        testno = testno + 1
        CALL indivnftest(testno, x, inf=inf)
        DEALLOCATE (x)

! Test 4
! Vector of length >1 -- negative infinity
        ALLOCATE (x(3))
        x = (/ two, IEEE_VALUE(one,IEEE_NEGATIVE_INF), -one /)
        testno = testno + 1
        CALL indivnftest(testno, x, inf=inf)
        DEALLOCATE (x)

! Test 5
! Vector of length >1 -- signaling NaN
        ALLOCATE (x(3))
        x = (/ two, IEEE_VALUE(one,IEEE_SIGNALING_NAN), -one /)
        testno = testno + 1
        CALL indivnftest(testno, x, nan=nan)
        DEALLOCATE (x)

! Test 6
! Vector of length >1 -- quiet NaN
        ALLOCATE (x(3))
        x = (/ two, IEEE_VALUE(one,IEEE_QUIET_NAN), -one /)
        testno = testno + 1
        CALL indivnftest(testno, x, nan=nan)
        DEALLOCATE (x)

! Test 7
! Obvious overflow -- both values maxReal
        ALLOCATE (x(2))
        x = (/ huge(zero), huge(zero) /)
        res = expectedresult( (/.TRUE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./), &
         infVal)
        testno = testno + 1
        testdescript = 'Obvious overflow'
        CALL indivtest(testno, x, res)
        DEALLOCATE (x)

! Test 8
! Boundary overflow value -- this should just trip
        ALLOCATE (x(2))
        x = (/ huge(0.0E0_dkind), scale(one,998) /)
        res = expectedresult( (/.TRUE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./), &
         infVal)
        testno = testno + 1
        testdescript = 'Boundary value causing overflow'
        CALL indivtest(testno, x, res)
        DEALLOCATE (x)

! Test 9
! Boundary overflow value -- this should just not trip
        ALLOCATE (x(2))
        x = (/ huge(0.0E0_dkind), scale(one,997) /)
        res = expectedresult( (/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./), &
         huge(0.0E0_dkind) )
        testno = testno + 1
        testdescript = 'Boundary value not causing overflow'
        CALL indivtest(testno, x, res)
        DEALLOCATE (x)

! Test 10
! Check underflow doesn't occur for very small values
        ALLOCATE (x(4))
        v = tiny(zero)
        x = (/ v,v,v,v /)
        res = expectedresult( (/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./), &
         two*v )
        testno = testno + 1
        testdescript = 'No underflow when dealing with very small values'
        CALL indivtest(testno, x, res)
        DEALLOCATE (x)

! Test 11
! Simple data generating representable result
        ALLOCATE (x(2))
        x = (/ 3.0E0_dkind, 4.0E0_dkind /)
        res = expectedresult( (/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./), &
         5.0E0_dkind )
        testno = testno + 1
        testdescript = 'Simple data for representable result'
        CALL indivtest(testno, x, res)
        DEALLOCATE (x)

! Test 12
! Simple data generating representable result (n>>3)
        intv = 85
        v = REAL(intv, KIND(zero))
        ALLOCATE (x(intv*intv))
        a = (v**4 - 24.0d0*v*v -25.0d0)/48.0d0
        x = (/ (a+REAL(i,KIND(zero)),  i= 0,intv*intv-1)/)
        res = expectedresult( (/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./), &
         ((v**5 + 47.0E0_dkind*v)/48.0E0_dkind) )
        testno = testno + 1
        testdescript = 'Simple data for representable result (exact)'
        CALL indivtest(testno, x, res)
        DEALLOCATE (x)

! Test 13
! Simple data generating representable result (inexact flag set)
        intv = 89
        v = REAL(intv, KIND(zero))
        ALLOCATE (x(intv*intv))
        a = (v**4 - 24.0d0*v*v -25.0d0)/48.0d0
        x = (/ (a+REAL(i,KIND(zero)),  i= 0,intv*intv-1)/)
        res = expectedresult( (/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.TRUE./), &
         ((v**5 + 47.0E0_dkind*v)/48.0E0_dkind) )
        testno = testno + 1
        testdescript = 'Simple data for representable result (inexact)'
        CALL indivtest(testno, x, res)
        DEALLOCATE (x)

! Test 14
! Simple data generating representable result
        ALLOCATE (x(1))
        x = (/ 3.0E0_dkind /)
        res = expectedresult( (/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./), &
         3.0E0_dkind )
        testno = testno + 1
        testdescript = 'n=1: Trivial case'
        CALL indivtest(testno, x, res)
        DEALLOCATE (x)

! Test 15
! Simple data generating representable result -- boundary case
        ALLOCATE (x(1))
        x = (/ -HUGE(zero) /)
        res = expectedresult( (/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./), &
        HUGE(zero)  )
        testno = testno + 1
        testdescript = 'n=1: Trivial case (boundary value)'
        CALL indivtest(testno, x, res)
        DEALLOCATE (x)


! Test 16
! Simple data generating representable result -- boundary case
        ALLOCATE (x(1))
        x = (/ -TINY(zero) /)
        res = expectedresult( (/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./), &
        TINY(zero)  )
        testno = testno + 1
        testdescript = 'n=1: Trivial case (boundary value)'
        CALL indivtest(testno, x, res)
        DEALLOCATE (x)


! Test 17
! Vector of length = 1 -- positive infinity
        ALLOCATE (x(1))
        x = (/IEEE_VALUE(one,IEEE_POSITIVE_INF)/)
        testno = testno + 1
        testdescript = 'n=1: +Inf'
        CALL indivnftest(testno, x, inf=inf)
        DEALLOCATE (x)

! Test 18
! Vector of length = 1 -- negative infinity
        ALLOCATE (x(1))
        x = (/IEEE_VALUE(one,IEEE_NEGATIVE_INF)/)
        testno = testno + 1
        testdescript = 'n=1: -Inf'
        CALL indivnftest(testno, x, inf=inf)
        DEALLOCATE (x)

! Test 19
! Vector of length = 1 -- signaling NaN
        ALLOCATE (x(1))
        x = (/IEEE_VALUE(one,IEEE_SIGNALING_NAN)/)
        testno = testno + 1
        testdescript = 'n=1: sNaN'
        CALL indivnftest(testno, x, nan=nan)
        DEALLOCATE (x)

! Test 20
! Vector of length = 1 -- quiet NaN
        ALLOCATE (x(1))
        x = (/IEEE_VALUE(one,IEEE_QUIET_NAN)/)
        testno = testno + 1
        testdescript = 'n=1: qNaN'
        CALL indivnftest(testno, x, nan=nan)
        DEALLOCATE (x)

! Test 21
! Vector of length = 5 -- incx = 2, n=3 -- exact result
        ALLOCATE (x(5))
        x = (/two, 5.0e0_dkind, 3.0e0_dkind, 9.0e0_dkind,  &
              6.0e0_dkind /)
        res = expectedresult( (/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./), &
          7.0e0_dkind)
        testno = testno + 1
        testdescript = 'incx = 2, exact result'
        CALL indivtest(testno, x, res, incxVal=2, nVal=3)
        DEALLOCATE (x)

! Test 22
! Vector of length = 4 -- incx = 3, n=2 -- inexact result
        ALLOCATE (x(4))
        x = (/two, 3.0e0_dkind, 4.0e0_dkind, 5.0e0_dkind/)
        res = expectedresult( (/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.TRUE./), &
          sqrt(29.0e0_dkind))
        testno = testno + 1
        testdescript = 'incx = 3, inexact result'
        CALL indivtest(testno, x, res, incxVal=3, nVal=2)
        DEALLOCATE (x)

! Test 23
! Vector of length =5 -- incx=2, n=3 -- vector contains
! Inf and NaN but such elements should not be accessed
        ALLOCATE (x(5))
        x = (/two,IEEE_VALUE(one,IEEE_POSITIVE_INF) , 3.0e0_dkind, &
             IEEE_VALUE(one,IEEE_QUIET_NAN) , 6.0e0_dkind /)
        res = expectedresult( (/.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./), &
          7.0e0_dkind)
        testno = testno + 1
        testdescript = 'incx = 2, Inf and qNan present in input vector'&
                       // ' but should not be accessed'
        CALL indivtest(testno, x, res, incxVal=2, nVal=3)
        DEALLOCATE (x)

      END SUBROUTINE runtests

    END MODULE dnrm2_testmod
