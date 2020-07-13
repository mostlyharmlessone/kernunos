    MODULE Airy_Module
      IMPLICIT NONE
! Generic Airy functions and their first derivatives
! The module is based on
! B.R. Fabijonas.  Algorithm 838: Airy Functions.  ACM Transactions 
!   on Mathematical Software Vol(No) 30(4), p. 491, 2004.

! Typical usage:
!     USE Airy_Module
!     REAL(SKIND) ABi(4), x
!     x=1.2; ABi = Airy(X)
! ABi(1:4) contains the function values [Ai(x),Ai'(x),Bi(x),Bi'(x)]^T.

      INTERFACE abi
        MODULE PROCEDURE sairy, dairy, cairy, zairy
      END INTERFACE

    CONTAINS
      FUNCTION sairy(x,flags) RESULT (abi)
        USE set_precision, ONLY : wp => skind
        USE airy_functions_real_single ! From ACM-TOMS/Calgo 838

!------Start BLOCK of common source for single and double precision.
        INCLUDE 'sourceReal.txt'
!------End BLOCK of common source for single and double precision. 

        WRITE (*,'(A/A, 1PE16.8, 2I6)') &
          'Exception for single precision Airy', &
          'Values of X, flags for AIRY_AI, AIRY_BI', x, ierra, ierrb
      END FUNCTION sairy

      FUNCTION dairy(x,flags) RESULT (abi)
        USE set_precision, ONLY : wp => dkind
        USE airy_functions_real_double ! From ACM-TOMS/Calgo 838

!------Start BLOCK of common source for single and double precision.       
        INCLUDE 'sourceReal.txt'
!------End BLOCK of common source for single and double precision. 

        WRITE (*,'(A/A, 1PE24.16, 2I6)') &
            'Exception for double precision Airy' &
          , 'Values of X, flags for AIRY_AI, AIRY_BI', x, ierra, ierrb
      END FUNCTION dairy

      FUNCTION cairy(z,flags) RESULT (abi)
        USE set_precision, ONLY : wp => skind
        USE airy_functions_complex_single ! From ACM-TOMS/Calgo 838

!------Start BLOCK of common source for single and double precision.
        INCLUDE 'sourceComplex.txt'
!------End BLOCK of common source for single and double precision. 

        WRITE (*,'(A/A/ 1P2E16.8, 3I6)') &
          'Exception for complex single precision Airy', &
          'Values of Z, flags for AIRY_AI', z, ierra, ierrb, ierrc
      END FUNCTION cairy

      FUNCTION zairy(z,flags) RESULT (abi)
        USE set_precision, ONLY : wp => dkind
        USE airy_functions_complex_double ! From ACM-TOMS/Calgo 838

!------Start BLOCK of common source for single and double precision.       
        INCLUDE 'sourceComplex.txt'
!------End BLOCK of common source for single and double precision. 

        WRITE (*,'(A/A/ 1P2E24.16, 3I6)') &
          'Exception for complex double precision Airy', &
          'Values of Z, flags for AIRY_AI', z, ierra, ierrb, ierrc
      END FUNCTION zairy
    END MODULE Airy_Module
