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
        REAL (wp), INTENT (IN) :: x
        INTEGER, OPTIONAL :: flags(2)
        REAL (wp) :: abi(4)

        INTEGER :: ierra, ierrb
        LOGICAL :: modify_switch

        modify_switch = .FALSE.

! From ACM-TOMS 838:
        CALL airy_ai(x,abi(1),abi(2),ierra,modify_switch)
        CALL airy_bi(x,abi(3),abi(4),ierrb,modify_switch)
        IF (present(flags)) THEN
          flags(1) = ierra
          flags(2) = ierrb
          RETURN
        END IF
        IF (ierra==0 .AND. ierrb==0) RETURN
!------End BLOCK of common source for single and double precision. 

        WRITE (*,'(A/A, 1PE16.8, 2I6)') &
          'Exception for single precision Airy', &
          'Values of X, flags for AIRY_AI, AIRY_BI', x, ierra, ierrb
      END FUNCTION sairy

      FUNCTION dairy(x,flags) RESULT (abi)
        USE set_precision, ONLY : wp => dkind
        USE airy_functions_real_double ! From ACM-TOMS/Calgo 838

!------Start BLOCK of common source for single and double precision.       
        REAL (wp), INTENT (IN) :: x
        INTEGER, OPTIONAL :: flags(2)
        REAL (wp) :: abi(4)

        INTEGER ierra, ierrb
        LOGICAL modify_switch

        modify_switch = .FALSE.

! From ACM-TOMS 838:
        CALL airy_ai(x,abi(1),abi(2),ierra,modify_switch)
        CALL airy_bi(x,abi(3),abi(4),ierrb,modify_switch)
        IF (present(flags)) THEN
          flags(1) = ierra
          flags(2) = ierrb
          RETURN
        END IF
        IF (ierra==0 .AND. ierrb==0) RETURN
!------End BLOCK of common source for single and double precision. 

        WRITE (*,'(A/A, 1PE24.16, 2I6)') &
            'Exception for double precision Airy' &
          , 'Values of X, flags for AIRY_AI, AIRY_BI', x, ierra, ierrb
      END FUNCTION dairy

      FUNCTION cairy(z,flags) RESULT (abi)
        USE set_precision, ONLY : wp => skind
        USE airy_functions_complex_single ! From ACM-TOMS/Calgo 838

!------Start BLOCK of common source for single and double precision.
        COMPLEX (wp), INTENT (IN) :: z
        INTEGER, OPTIONAL :: flags(3)
        COMPLEX (wp) :: abi(4)

        INTEGER ierra, ierrb, ierrc
        LOGICAL :: argument_switch = .FALSE., modify_switch = .FALSE.
        COMPLEX (wp) alpha, beta, gamma, bi1(2), bi2(2)
        REAL (wp) :: half = 0.5_wp, three = 3._wp

! From ACM-TOMS 838:
        CALL airy_ai(z,abi(1),abi(2),ierra,argument_switch,modify_switch)
! Use connection formulas for complex values to get Bi(z), Bi'(z). 
! Bi(z) = exp(-pi * i /6) * Ai(z * exp (-2 * pi * i /3)) +
!         exp( pi * i /6) * Ai(z * exp ( 2 * pi * i /3))      
        beta = -cmplx(half,sqrt(three)*half,wp)

        CALL airy_ai(z*beta,bi1(1),bi1(2),ierrb,argument_switch,modify_switch)
        CALL airy_ai(z*conjg(beta),bi2(1),bi2(2),ierrc,argument_switch, &
          modify_switch)
        alpha = cmplx(sqrt(three)*half,-half,wp)
        gamma = -conjg(alpha)
        abi(3) = alpha*bi1(1) + conjg(alpha)*bi2(1)
        abi(4) = gamma*bi1(2) + conjg(gamma)*bi2(2)
        IF (present(flags)) THEN
          flags(1) = ierra
          flags(2) = ierrb
          flags(3) = ierrc
          RETURN
        END IF
        IF (ierra==0 .AND. ierrb==0 .AND. ierrc==0) RETURN
!------End BLOCK of common source for single and double precision. 

        WRITE (*,'(A/A/ 1P2E16.8, 3I6)') &
          'Exception for complex single precision Airy', &
          'Values of Z, flags for AIRY_AI', z, ierra, ierrb, ierrc
      END FUNCTION cairy

      FUNCTION zairy(z,flags) RESULT (abi)
        USE set_precision, ONLY : wp => dkind
        USE airy_functions_complex_double ! From ACM-TOMS/Calgo 838

!------Start BLOCK of common source for single and double precision.       
        COMPLEX (wp), INTENT (IN) :: z
        INTEGER, OPTIONAL :: flags(3)
        COMPLEX (wp) :: abi(4)

        INTEGER ierra, ierrb, ierrc
        LOGICAL :: argument_switch = .FALSE., modify_switch = .FALSE.
        COMPLEX (wp) alpha, beta, gamma, bi1(2), bi2(2)
        REAL (wp) :: half = 0.5_wp, three = 3._wp
! From ACM-TOMS 838:
        CALL airy_ai(z,abi(1),abi(2),ierra,argument_switch,modify_switch)
! Use connection formulas for complex values to get Bi(z), Bi'(z). 
! Bi(z) = exp(-pi * i /6) * Ai(z * exp (-2 * pi * i /3)) +
!         exp( pi * i /6) * Ai(z * exp ( 2 * pi * i /3))
! Note: Specify DKIND using this intrinsic function
! or else it defaults to single precision.      
        beta = -cmplx(half,sqrt(three)*half,wp)

        CALL airy_ai(z*beta,bi1(1),bi1(2),ierrb,argument_switch,modify_switch)
        CALL airy_ai(z*conjg(beta),bi2(1),bi2(2),ierrc,argument_switch, &
          modify_switch)
        alpha = cmplx(sqrt(three)*half,-half,wp)
        gamma = -conjg(alpha)
        abi(3) = alpha*bi1(1) + conjg(alpha)*bi2(1)
        abi(4) = gamma*bi1(2) + conjg(gamma)*bi2(2)
        IF (present(flags)) THEN
          flags(1) = ierra
          flags(2) = ierrb
          flags(3) = ierrc
          RETURN
        END IF
        IF (ierra==0 .AND. ierrb==0 .AND. ierrc==0) RETURN
!------End BLOCK of common source for single and double precision. 

        WRITE (*,'(A/A/ 1P2E24.16, 3I6)') &
          'Exception for complex double precision Airy', &
          'Values of Z, flags for AIRY_AI', z, ierra, ierrb, ierrc
      END FUNCTION zairy
    END MODULE Airy_Module
