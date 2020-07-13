      MODULE airy
! Module that collects the generic definitions from
! the four airy function modules so that a single
! use statement may be used.
      USE airy_functions_real_single, ONLY : airy_ai, airy_ai_zero, airy_bi_zero, &
          airy_bi, airy_info, airy_aux, airy_aux_info
      USE airy_functions_real_double, ONLY : airy_ai, airy_ai_zero, airy_bi_zero, &
          airy_bi, airy_info, airy_aux, airy_aux_info
      USE airy_functions_complex_single, ONLY : airy_ai, airy_ai_zero, airy_bi_zero
      USE airy_functions_complex_double, ONLY : airy_ai, airy_ai_zero, airy_bi_zero
        
      PRIVATE
      PUBLIC :: airy_ai, airy_bi, airy_ai_zero, airy_bi_zero
      PUBLIC :: airy_info, airy_aux, airy_aux_info

      END MODULE airy
