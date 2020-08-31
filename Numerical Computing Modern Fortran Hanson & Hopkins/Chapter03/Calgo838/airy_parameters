
! begin file airy_functions_parameters
!    version 1.0  
!    edited 12/28/2001

    SUBROUTINE airy_paramsr(radius,max_step_size,n_terms_taylor,n_terms_asymp, &
        n_partitions)
      REAL (prd), INTENT (OUT) :: radius
      REAL (prd), INTENT (OUT), OPTIONAL :: max_step_size
      INTEGER, INTENT (OUT), OPTIONAL :: n_terms_taylor, n_terms_asymp, &
        n_partitions
!**************************************************************************!
! This subroutine returns to the user the parameters used specific to the
!   machine and compiler used
!**************************************************************************!
      IF ( .NOT. is_init_airy) CALL parameter_airy
      radius = r_min
      IF (present(max_step_size)) max_step_size = fstpsz
      IF (present(n_terms_taylor)) n_terms_taylor = n_taylor
      IF (present(n_terms_asymp)) n_terms_asymp = n_asymp
      IF (present(n_partitions)) n_partitions = n_parts
    END SUBROUTINE airy_paramsr

    SUBROUTINE parameter_airy
!***********************************************************************!
!  This subroutine computes and stores the global variables
!    r_lolimit
!    r_uplimit
!    r_min (rho)
!    n_asymp (S)
!    n_taylor (N)
!    n_parts (P)
!    fstpsz (h)
!  and the asymptotic expansion coefficients
!    ucoef (Eq. 2.6)
!    vcoef (Eq. 2.6)
!  Parenthetical notes refer to the ACM TOMS paper (submitted?).
!***********************************************************************!
      INTEGER m, i
      REAL (prd) eta, sfloat, xinew, xiold, chi, fun, dfun, basep
      REAL (prd), DIMENSION (0:2) :: lambda

!  required machine constants
      m = digits(one)
      basep = real(radix(one),prd)
      eta = epsilon(one)/basep

!  three global variables: the upper and lower limits on the 
!   computation region and the number of terms in the 
!   asymptotic expansions for large |z|
      r_lolimit = epsilon(eta)/abs(ai1zer)
      r_uplimit = exp(two_third*log(huge(eta)*0.95_prd))
      n_asymp = m*log(basep) + half

!  generate the coefficients for the asymptotic expansions for large |z|
      ALLOCATE (ucoef(n_asymp))
      ALLOCATE (vcoef(n_asymp))
      ucoef(1) = 5.0_prd/72.0_prd
      vcoef(1) = -7.0_prd/72.0_prd
      DO i = 1, n_asymp - 1
        ucoef(i+1) = real((6*i+5)*(6*i+1),prd)/real((i+1)*72,prd)*ucoef(i)
        vcoef(i+1) = -real(6*i+7,prd)/real(6*i+5,prd)*ucoef(i+1)
      END DO

!  determine the parameter r_min (rho) using Newton's method
      chi = one
      IF (mod(n_asymp,2)==0) THEN
        DO i = 1, n_asymp/2
          chi = chi*i/(i-half)
        END DO
      ELSE
        chi = pi*half
        DO i = 1, (n_asymp-1)/2
          chi = chi*(i+half)/i
        END DO
      END IF
      sfloat = real(n_asymp,prd)
      xinew = half*sfloat
      chi = m*log(basep) + log(abs(vcoef(n_asymp))) + log(two*chi) ! dummy
      fstpsz = 7.0_prd*pi/72.0_prd ! dummy
      DO
        xiold = xinew
        fun = sfloat*log(xiold) - fstpsz/xiold - chi
        dfun = sfloat/xiold + fstpsz/xiold**2
        xinew = xiold - fun/dfun
        IF (abs((xinew-xiold)/xinew)<four*eta) EXIT
      END DO

!  three of the global variables can now be computed
      r_min = (1.50_prd*xinew)**two_third
      n_parts = ceiling(r_min)
      fstpsz = r_min/n_parts

!  the number of terms in the taylor series
      lambda(0) = one/fstpsz
      lambda(1) = one
      lambda(2) = r_min*half*fstpsz
      i = 2
      DO
        i = i + 1
        lambda(mod(i,3)) = (r_min*lambda(mod(i-2,3))+fstpsz*lambda(mod(i-3, &
          3)))/real(i*(i-1),prd)*fstpsz**2
        IF (i*lambda(mod(i,3))<half*eta) EXIT
      END DO

!  the final global variable
      n_taylor = i

!     reset the logical and exit
      is_init_airy = .TRUE.

! the following are dummy statements to use variables in the
!   header file that pertain to the complex routines.
      arg_local = .FALSE.
      ALLOCATE (aigrid(1,1))
      DEALLOCATE (aigrid)
      z_global = dbizc(1)
      zeta_global = bizc(1)
      theta_global = zero

! the following are dummy statements to use variables in the
!   header file that pertain to the real routines.
      big_integrate_aux = .FALSE.
      ALLOCATE (m2grid(1,1))
      DEALLOCATE (m2grid)
      ALLOCATE (theta_grid(1))
      DEALLOCATE (theta_grid)
      x_global = dbizr(1)
      xi_global = bizr(1)
      x_global = daizr(1)
      xi_global = aizr(1)

    END SUBROUTINE parameter_airy

!  end file airy_functions_parameters







! begin file airy_aux_parameters
!    version 1.0  
!    edited 01/07/2002

!***
!************************************************************************
!***
    SUBROUTINE airy_paramsr_aux(radius,max_step_size,n_terms_taylor, &
        n_terms_asymp_mod,n_terms_asymp_phase,n_partitions)
      REAL (prd), INTENT (OUT) :: radius
      REAL (prd), INTENT (OUT), OPTIONAL :: max_step_size
      INTEGER, INTENT (OUT), OPTIONAL :: n_terms_taylor, n_terms_asymp_mod, &
        n_terms_asymp_phase, n_partitions
!*
! This PUBLIC subroutine returns to the user the parameters used for the
!   modulus and phase functions specific to the
!   machine and compiler used.
!*
      IF ( .NOT. is_aux_init_airy) CALL aux_parameter_airy
      radius = aux_min
      IF (present(max_step_size)) max_step_size = mstpsz
      IF (present(n_terms_taylor)) n_terms_taylor = n_taylor_aux
      IF (present(n_terms_asymp_mod)) n_terms_asymp_mod = n_asymp_mod
      IF (present(n_terms_asymp_phase)) n_terms_asymp_phase = n_asymp_phase
      IF (present(n_partitions)) n_partitions = n_parts_aux
    END SUBROUTINE airy_paramsr_aux
!***
!************************************************************************
!***
    SUBROUTINE aux_parameter_airy
!*
! This PRIVATE subroutine computes and stores the global variables
!    aux_min (hat rho)
!    n_asymp_phase (hat T)
!    n_asymp_mod (hat S)
!    n_taylor_aux (hat N)
!    n_parts_aux (hat P)
!    mstpsz (hat h)
!  and the asymptotic expansion coefficients
!    mcoef (Eq. 7.7)
!    ncoef (Eq. 7.8)
!  Parenthetical notes refer to the ACM TOMS paper (submitted?).
!*
      REAL (prd), DIMENSION (0:2) :: lambda
      REAL (prd) h, eta
      INTEGER i, m

! determine the compiler dependent constants
      IF ( .NOT. is_init_airy) CALL parameter_airy
      eta = epsilon(one)/real(radix(one),prd)
      m = digits(one)

! determine the number of terms needed in the asymptotic 
!   expansions for the modulus functions
      n_asymp_mod = half*m*log(two) - fourth*log(one*m) + half
      ALLOCATE (mcoef(n_asymp_mod))
      ALLOCATE (ncoef(n_asymp_mod))
      mcoef(1) = 5.0_prd/32.0_prd
      ncoef(1) = -1.40_prd*mcoef(1)
      DO i = 1, n_asymp_mod - 1
        mcoef(i+1) = real((6*i+5)*(6*i+3)*(6*i+1),prd)/real((i+1)*96,prd)* &
          mcoef(i)
        ncoef(i+1) = -real(6*i+7,prd)/real(6*i+5,prd)*mcoef(i+1)
      END DO

!  we now determine the parameter hat rho 
      aux_min = abs(ncoef(n_asymp_mod-1))/eta
      aux_min = aux_min**(one/real(3*(n_asymp_mod-1),prd))

!  determine the number of terms needed in asymptotic expansion 
!   of the phase functions
      i = 1
      DO
        IF (thetacoeff(i)/=zero .AND. i/=n_coeff_phase) THEN
          IF (two*max(abs(thetacoeff(i)),abs(phicoeff(i)))/aux_min**(3*i)/eta< &
            one) EXIT
          i = i + 1
        ELSE
          aux_min = aux_min + one
          i = 1
        END IF
      END DO
      n_asymp_phase = i

!  the number of terms in the taylor series
      h = aux_min/ceiling(aux_min)
      lambda(0) = one/h**2
      lambda(1) = one/h
      lambda(2) = half
      i = 3
      DO
        lambda(mod(i,3)) = h**4*(four*aux_min*(mod(i-2,3))*lambda(mod(i-2, &
          3))+lambda(mod(i-3,3))*(4*i-10)*h)/real(i*(i-1)*(i-2),prd)
        IF (two*i*(i-1)*lambda(mod(i,3))/eta<one) EXIT
        i = i + 1
      END DO

!  the machine specific constants are determined.
      n_taylor_aux = i
      n_parts_aux = ceiling(aux_min)
      mstpsz = h

! reset the logical and exit the routine
      is_aux_init_airy = .TRUE.
    END SUBROUTINE aux_parameter_airy
!***
!************************************************************************
!***

!  end file airy_aux_parameters

! begin file airy_zeros_parameters
!    version 1.0  
!    edited 01/03/2002

!***
!************************************************************************
!***
    SUBROUTINE zero_parameter_airy
!*
!  PRIVATE subroutine which determines the number of terms to use 
!  in the asymptotic expansion for the zeros  
!*
      INTEGER nn
      REAL (prd) tol, term1, term2

      IF ( .NOT. is_init_airy) CALL parameter_airy
      tol = epsilon(tol)/two
      DO nn = 1, n_coeff_zero
        term1 = ucoeff(nn)/(three_pi_ate*real(4*n_zeros-1,prd))**(2*nn)
        term2 = tcoeff(nn)/(three_pi_ate*real(4*n_zeros-3,prd))**(2*nn)
        IF (abs(two*max(term1,term2)/tol)<one) THEN
          n_asymp_zero = nn
          EXIT
        END IF
      END DO
      DO nn = 1, n_coeff_asso
        term1 = vcoeff(nn)/(three_pi_ate*real(4*n_zeros-1,prd))**(2*nn)
        term2 = wcoeff(nn)/(three_pi_ate*real(4*n_zeros-3,prd))**(2*nn)
        IF (abs(two*max(term1,term2)/tol)<one) THEN
          n_asymp_asso = nn
          EXIT
        END IF
      END DO

! reset the logical and exit the routine
      is_zero_init_airy = .TRUE.
    END SUBROUTINE zero_parameter_airy
!***
!************************************************************************
!***

!  end file airy_zeros_parameters
