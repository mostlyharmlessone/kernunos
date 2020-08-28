      MODULE Background      
C     Define a kind parameter used for precision control.
      USE set_precision, ONLY : dkind, skind
C This set of routines contains lower-level operations that are used
C in the BLAS.  For completeness there are also replacement routines
C for the machine constant routines d1mach, r1mach and i1mach.
C The codes contained are:
C d1mach
C r1mach
C i1mach
C lsame
C XERBLA

      CONTAINS   
      DOUBLE PRECISION FUNCTION d1mach (i)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: i
C***BEGIN PROLOGUE  d1mach
C***PURPOSE  Return floating point machine dependent constants
C            using Fortran 95 elemental functions.
C***CATEGORY  R1
C***TYPE      DOUBLE PRECISION
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  Fox, P. A., (Bell Labs)
C           Hall, A. D., (Bell Labs)
C           Schryer, N. L., (Bell Labs)
C***DESCRIPTION
C
C   d1mach can be used to obtain machine-dependent parameters for the
C   local machine environment.  It is a function subprogram with one
C   (input) argument, and can be referenced as follows:
C
C        D = d1mach(i)
C
C   where I=1,...,5.  The (output) value of D above is determined by
C   the (input) value of I.  The results for various values of I are
C   discussed below.
C
C   d1mach( 1) = B**(EMIN-1), the smallest positive magnitude.
C   d1mach( 2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C   d1mach( 3) = B**(-T), the smallest relative spacing.
C   d1mach( 4) = B**(1-T), the largest relative spacing.
C   d1mach( 5) = LOG10(B)
C
C   Assume double precision numbers are represented in the T-digit,
C   base-B form
C
C              sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C   where 0 .LE. X(i) .LT. B for I=1,...,T, 0 .LT. X(1), and
C   EMIN .LE. E .LE. EMAX.
C
C   The values of B, T, EMIN and EMAX are provided in i1mach as
C   follows:
C   i1mach(10) = B, the base.
C   i1mach(14) = T, the number of base-B DIGITS.
C   i1mach(15) = EMIN, the smallest exponent E.
C   i1mach(16) = EMAX, the largest exponent E.
C

C***REFERENCES  P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
C                 a portable library, ACM Transactions on Mathematical
C                 Software 4, 2 (June 1978), pp. 177-188.

C***FIRST EXECUTABLE STATEMENT  d1mach

      SELECT CASE(i)
        CASE(1)
            d1mach=TINY(d1mach)
        CASE(2)
            d1mach=HUGE(d1mach)
        CASE(3)
            d1mach=EPSILON(d1mach)/RADIX(d1mach)
        CASE(4)
            d1mach=EPSILON(d1mach)
        CASE(5)
            d1mach=LOG10(REAL(RADIX(d1mach),KIND(d1mach)))
      END SELECT
 
      END FUNCTION d1mach
      
      REAL FUNCTION r1mach (i)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: i
C***BEGIN PROLOGUE  r1mach
C***PURPOSE  Return floating point machine dependent constants
C            in Fortran 95 elemental functions.
C***CATEGORY  R1
C***TYPE      SINGLE PRECISION
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  Fox, P. A., (Bell Labs)
C           Hall, A. D., (Bell Labs)
C           Schryer, N. L., (Bell Labs)
C***DESCRIPTION
C
C   r1mach can be used to obtain machine-dependent parameters for the
C   local machine environment.  It is a function subprogram with one
C   (input) argument, and can be referenced as follows:
C
C        A = r1mach(i)
C
C   where I=1,...,5.  The (output) value of A above is determined by
C   the (input) value of I.  The results for various values of I are
C   discussed below.
C
C   r1mach(1) = B**(EMIN-1), the smallest positive magnitude.
C   r1mach(2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
C   r1mach(3) = B**(-T), the smallest relative spacing.
C   r1mach(4) = B**(1-T), the largest relative spacing.
C   r1mach(5) = LOG10(B)
C
C   Assume single precision numbers are represented in the T-digit,
C   base-B form
C
C              sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C   where 0 .LE. X(i) .LT. B for I=1,...,T, 0 .LT. X(1), and
C   EMIN .LE. E .LE. EMAX.
C
C   The values of B, T, EMIN and EMAX are provided in i1mach as
C   follows:
C   i1mach(10) = B, the base.
C   i1mach(11) = T, the number of base-B DIGITS.
C   i1mach(12) = EMIN, the smallest exponent E.
C   i1mach(13) = EMAX, the largest exponent E.
C
C***REFERENCES  P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
C                 a portable library, ACM Transactions on Mathematical
C                 Software 4, 2 (June 1978), pp. 177-188.

C***FIRST EXECUTABLE STATEMENT  r1mach
C      r1mach = RMACH(i)
      SELECT CASE(i)
        CASE(1)
            r1mach=TINY(r1mach)
        CASE(2)
            r1mach=HUGE(r1mach)
        CASE(3)
            r1mach=EPSILON(r1mach)/RADIX(r1mach)
        CASE(4)
            r1mach=EPSILON(r1mach)
        CASE(5)
            r1mach=LOG10(REAL(RADIX(r1mach),KIND(r1mach)))
      END SELECT
      RETURN
C
      END FUNCTION r1mach
      
      INTEGER FUNCTION i1mach (i)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: I
C***BEGIN PROLOGUE  i1mach
C***PURPOSE  Return integer machine dependent constants
C            in Fortran 95 elemental functions.
C***CATEGORY  R1
C***TYPE      INTEGER (i1mach-I)
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  Fox, P. A., (Bell Labs)
C           Hall, A. D., (Bell Labs)
C           Schryer, N. L., (Bell Labs)
C***REFERENCES  P. A. Fox, A. D. Hall and N. L. Schryer, Framework for
C                 a portable library, ACM Transactions on Mathematical
C                 Software 4, 2 (June 1978), pp. 177-188.
C***ROUTINES CALLED  (NONE)
C***DESCRIPTION
C
C   i1mach can be used to obtain machine-dependent parameters for the
C   local machine environment.  It is a function subprogram with one
C   (input) argument and can be referenced as follows:
C
C        K = i1mach(i)
C
C   where I=1,...,16.  The (output) value of K above is determined by
C   the (input) value of I.  The results for various values of I are
C   discussed below.
C
C   I/O unit numbers:
C     i1mach( 1) = the standard input unit.
C     i1mach( 2) = the standard output unit.
C     i1mach( 3) = the standard punch unit.
C     i1mach( 4) = the standard error message unit.
C
C   Words:
C     i1mach( 5) = the number of bits per integer storage unit.
C     i1mach( 6) = the number of characters per integer storage unit.
C
C   Integers:
C     assume integers are represented in the S-digit, base-A form
C
C                sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C
C                where 0 .LE. X(i) .LT. A for I=0,...,S-1.
C     i1mach( 7) = A, the base.
C     i1mach( 8) = S, the number of base-A DIGITS.
C     i1mach( 9) = A**S - 1, the largest magnitude.
C
C   Floating-Point Numbers:
C     Assume floating-point numbers are represented in the T-digit,
C     base-B form
C                sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C
C                where 0 .LE. X(i) .LT. B for I=1,...,T,
C                0 .LT. X(1), and EMIN .LE. E .LE. EMAX.
C     i1mach(10) = B, the base.
C
C   Single-Precision:
C     i1mach(11) = T, the number of base-B DIGITS.
C     i1mach(12) = EMIN, the smallest exponent E.
C     i1mach(13) = EMAX, the largest exponent E.
C
C   Double-Precision:
C     i1mach(14) = T, the number of base-B DIGITS.
C     i1mach(15) = EMIN, the smallest exponent E.
C     i1mach(16) = EMAX, the largest exponent E.
C***END PROLOGUE  i1mach

      SELECT CASE(i)
        CASE(1)
            i1mach = 5
        CASE(2)
            i1mach = 6
        CASE(3)
            i1mach = 6
        CASE(4)
            i1mach = 6
            
        CASE(5)
            i1mach = DIGITS(i1mach)
        CASE(6)
C The number of chars per integer is based on 8 bits / char
C This elemental is not supported in Fortran because some languages
C require more bits/char.  So this is the only parameter that could
C require changing.        
            i1mach = DIGITS(i1mach)/8
            
        CASE(7)
            i1mach = RADIX(i1mach)
        CASE(8)
            i1mach = DIGITS(i1mach)          
        CASE(9)
            i1mach = HUGE(i1mach)
            
        CASE(10)
            i1mach = RADIX(d1mach(i))
            
        CASE(11)
            i1mach = DIGITS(r1mach(i))
        CASE(12)
            i1mach = MINEXPONENT(r1mach(i))
        CASE(13)
            i1mach = MAXEXPONENT(r1mach(i))
            
        CASE(14)
            i1mach = DIGITS(d1mach(i))
        CASE(15)
            i1mach = MINEXPONENT(d1mach(i))
        CASE(16)
            i1mach = MAXEXPONENT(d1mach(i))
      END SELECT

      END FUNCTION i1mach
      
      LOGICAL FUNCTION lsame(ca,cb)
C
C  -- LAPACK auxiliary routine (version 3.1) --
C     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
C     November 2006
C
C     .. Scalar Arguments ..
      CHARACTER, INTENT(IN) :: ca,cb
C     ..
C
C  Purpose
C  =======
C
C  lsame returns .TRUE. if CA is the same letter as CB regardless of
C  case.
C
C  Arguments
C  =========
C
C  CA      (input) CHARACTER*1
C
C  CB      (input) CHARACTER*1
C          CA and CB specify the single characters to be compared.
C
C =====================================================================
C
C     .. Intrinsic Functions ..
      INTRINSIC :: ICHAR
C     ..
C     .. Local Scalars ..
      INTEGER :: inta,intb,zcode
C     ..
C
C     Test if the characters are equal
C
      lsame = CA .EQ. CB
      IF (lsame) RETURN
C
C     Now test for equivalence if both characters are alphabetic.
C
      zcode = ICHAR('Z')
C
C     Use 'Z' rather than 'A' so that ASCII can be detected on Prime
C     machines, on which ICHAR returns a value with bit 8 set.
C     ICHAR('A') on Prime machines returns 193 which is the same as
C     ICHAR('A') on an EBCDIC machine.
C
      inta = ICHAR(ca)
      intb = ICHAR(cb)
C
      IF (zcode.EQ.90 .OR. zcode.EQ.122) THEN
C
C        ASCII is assumed - zcode is the ASCII code of either lower or
C        upper case 'Z'.
C
          IF (inta.GE.97 .AND. inta.LE.122) inta = inta - 32
          IF (intb.GE.97 .AND. intb.LE.122) intb = intb - 32
C
      ELSE IF (zcode.EQ.233 .OR. zcode.EQ.169) THEN
C
C        EBCDIC is assumed - zcode is the EBCDIC code of either lower or
C        upper case 'Z'.
C
          IF (inta.GE.129 .AND. inta.LE.137 .OR.
     +        inta.GE.145 .AND. inta.LE.153 .OR.
     +        inta.GE.162 .AND. inta.LE.169) inta = inta + 64
          IF (intb.GE.129 .AND. intb.LE.137 .OR.
     +        intb.GE.145 .AND. intb.LE.153 .OR.
     +        intb.GE.162 .AND. intb.LE.169) intb = intb + 64
C
      ELSE IF (zcode.EQ.218 .OR. zcode.EQ.250) THEN
C
C        ASCII is assumed, on Prime machines - zcode is the ASCII code
C        plus 128 of either lower or upper case 'Z'.
C
          IF (inta.GE.225 .AND. inta.LE.250) inta = inta - 32
          IF (intb.GE.225 .AND. intb.LE.250) intb = intb - 32
      END IF
      lsame = inta .EQ. intb
C
C     RETURN
C
C     End of lsame
C
      END FUNCTION lsame

      SUBROUTINE xerbla(srname,info)
C
C  -- LAPACK auxiliary routine (preliminary version) --
C     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
C     November 2006
C
C     .. Scalar Arguments ..
      INTEGER, INTENT(IN) :: info
      CHARACTER*6, INTENT(IN) :: srname
C     ..
C
C  Purpose
C  =======
C
C  XERBLA  is an error handler for the LAPACK routines.
C  It is called by an LAPACK routine if an input parameter has an
C  invalid value.  A message is printed and execution stops.
C
C  Installers may consider modifying the STOP statement in order to
C  call system-specific exception-handling facilities.
C
C  Arguments
C  =========
C
C  SRNAME  (input) CHARACTER*6
C          The name of the routine which called XERBLA.
C
C  INFO    (input) INTEGER
C          The position of the invalid parameter in the parameter list
C          of the calling routine.
C
C
      WRITE (*,FMT=9999) srname,info
C
      STOP
C
 9999 FORMAT (' ** On entry to ',A6,' parameter number ',I2,' had ',
     +       'an illegal value')
C
C     End of XERBLA
C
      END SUBROUTINE XERBLA

      END MODULE Background
      

