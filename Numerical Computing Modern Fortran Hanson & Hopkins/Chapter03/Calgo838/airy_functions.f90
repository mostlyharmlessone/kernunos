
!    version 1.0
!    built Fri Apr 16 18:03:05 CDT 2004

!  by B.R. Fabijonas
!     Department of Mathematics
!     Southern Methodist University
!     bfabi@smu.edu

!  see the file airy_README for an explanation
!***
!************************************************************************
!***
    MODULE airy_functions_real_single

      USE set_precision, ONLY: prd => skind
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: airy_ai, airy_bi, airy_ai_zero, airy_bi_zero
      PUBLIC :: airy_info, airy_aux, airy_aux_info
      INCLUDE 'airy_head.inc'
!*
!  interfaces
!*
      INTERFACE airy_ai
        MODULE PROCEDURE airy_air
      END INTERFACE
      INTERFACE airy_bi
        MODULE PROCEDURE airy_bir
      END INTERFACE
      INTERFACE airy_ai_zero
        MODULE PROCEDURE ai_zeror
        MODULE PROCEDURE ai_zerorv
      END INTERFACE
      INTERFACE airy_bi_zero
        MODULE PROCEDURE bi_zeror
        MODULE PROCEDURE bi_zerorv
      END INTERFACE
      INTERFACE airy_info
        MODULE PROCEDURE airy_paramsr
      END INTERFACE
      INTERFACE airy_aux
        MODULE PROCEDURE airy_auxr
      END INTERFACE
      INTERFACE airy_aux_info
        MODULE PROCEDURE airy_paramsr_aux
      END INTERFACE

    CONTAINS
      INCLUDE 'airy_real.inc'
      INCLUDE 'airy_parameters.inc'
    END MODULE airy_functions_real_single
!***
!************************************************************************
!***
    MODULE airy_functions_complex_single
      USE set_precision, ONLY: prd=>skind

      IMPLICIT NONE
      PRIVATE
      PUBLIC :: airy_ai, airy_ai_zero, airy_bi_zero
      INCLUDE 'airy_head.inc'
!*
!  interfaces
!*
      INTERFACE airy_ai
        MODULE PROCEDURE airy_aic
        MODULE PROCEDURE airy_ai_rayc
      END INTERFACE
      INTERFACE airy_ai_zero
        MODULE PROCEDURE ai_zeroc
        MODULE PROCEDURE ai_zerocv
      END INTERFACE
      INTERFACE airy_bi_zero
        MODULE PROCEDURE bi_zeroc
        MODULE PROCEDURE bi_zerocv
      END INTERFACE

    CONTAINS
      INCLUDE 'airy_complex.inc'
      INCLUDE 'airy_parameters.inc'
    END MODULE airy_functions_complex_single
!***
!************************************************************************
!***
    MODULE airy_functions_real_double
      USE set_precision, ONLY: prd=>dkind
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: airy_ai, airy_bi, airy_ai_zero, airy_bi_zero
      PUBLIC :: airy_info, airy_aux, airy_aux_info
      INCLUDE 'airy_head.inc'
!*
!  interfaces
!*
      INTERFACE airy_ai
        MODULE PROCEDURE airy_air
      END INTERFACE
      INTERFACE airy_bi
        MODULE PROCEDURE airy_bir
      END INTERFACE
      INTERFACE airy_ai_zero
        MODULE PROCEDURE ai_zeror
        MODULE PROCEDURE ai_zerorv
      END INTERFACE
      INTERFACE airy_bi_zero
        MODULE PROCEDURE bi_zeror
        MODULE PROCEDURE bi_zerorv
      END INTERFACE
      INTERFACE airy_info
        MODULE PROCEDURE airy_paramsr
      END INTERFACE
      INTERFACE airy_aux
        MODULE PROCEDURE airy_auxr
      END INTERFACE
      INTERFACE airy_aux_info
        MODULE PROCEDURE airy_paramsr_aux
      END INTERFACE

    CONTAINS
      INCLUDE 'airy_real.inc'
      INCLUDE 'airy_parameters.inc'
    END MODULE airy_functions_real_double
!***
!************************************************************************
!***
    MODULE airy_functions_complex_double
      USE set_precision, ONLY: prd=>dkind
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: airy_ai, airy_ai_zero, airy_bi_zero
      INCLUDE 'airy_head.inc'
!*
!  interfaces
!*
      INTERFACE airy_ai
        MODULE PROCEDURE airy_aic
        MODULE PROCEDURE airy_ai_rayc
      END INTERFACE
      INTERFACE airy_ai_zero
        MODULE PROCEDURE ai_zeroc
        MODULE PROCEDURE ai_zerocv
      END INTERFACE
      INTERFACE airy_bi_zero
        MODULE PROCEDURE bi_zeroc
        MODULE PROCEDURE bi_zerocv
      END INTERFACE

    CONTAINS
      INCLUDE 'airy_complex.inc'
      INCLUDE 'airy_parameters.inc'
    END MODULE airy_functions_complex_double
!***
!************************************************************************
!*** Comment out the rest of this file if compiler does not support
!    Quad precision.
!Module airy_functions_real_quad
!      USE set_precision, ONLY : prd=>qkind
!      Implicit None
!      Private
!      Public :: airy_ai, airy_bi, airy_ai_zero, airy_bi_zero
!      Public :: airy_info, airy_aux, airy_aux_info
!      Include 'airy_head.inc'
!!*
!!  interfaces
!!*
!      Interface airy_ai
!         Module Procedure airy_air
!      End Interface
!      Interface airy_bi
!         Module Procedure airy_bir
!      End Interface
!      Interface airy_ai_zero
!         Module Procedure ai_zeror
!         Module Procedure ai_zerorv
!      End Interface
!      Interface airy_bi_zero
!         Module Procedure bi_zeror
!         Module Procedure bi_zerorv
!      End Interface
!      Interface airy_info
!         Module Procedure airy_paramsr
!      End Interface
!      Interface airy_aux
!         Module Procedure airy_auxr
!      End Interface
!      Interface airy_aux_info
!         Module Procedure airy_paramsr_aux
!      End Interface
!Contains
!      Include 'airy_real.inc'
!      Include 'airy_parameters.inc'
!End Module airy_functions_real_quad
!!***
!!************************************************************************
!!***
!Module airy_functions_complex_quad
!      USE set_precision, ONLY : prd=>qkind
!      Implicit None
!      Private
!      Public :: airy_ai, airy_ai_zero, airy_bi_zero
!      Include 'airy_head.inc'
!!*
!!  interfaces
!!*
!      Interface airy_ai
!         Module Procedure airy_aic
!         Module Procedure airy_ai_rayc
!      End Interface
!      Interface airy_ai_zero
!         Module Procedure ai_zeroc
!         Module Procedure ai_zerocv
!      End Interface
!      Interface airy_bi_zero
!         Module Procedure bi_zeroc
!         Module Procedure bi_zerocv
!      End Interface
!Contains
!      Include 'airy_complex.inc'
!      Include 'airy_parameters.inc'
!End Module airy_functions_complex_quad
!!***
!!************************************************************************
!!***
