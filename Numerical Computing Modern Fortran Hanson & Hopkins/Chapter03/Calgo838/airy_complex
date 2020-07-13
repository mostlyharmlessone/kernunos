
! begin file airy_functions_complex   
!    version 1.0  
!    edited 07/24/2002

!***
!************************************************************************
!***
    SUBROUTINE airy_aic(z,ai,dai,ierr,argument_switch,modify_switch)
      COMPLEX (prd), INTENT (IN) :: z
      COMPLEX (prd), INTENT (OUT) :: ai, dai
      INTEGER, INTENT (OUT) :: ierr
      LOGICAL, INTENT (IN), OPTIONAL :: argument_switch, modify_switch
!*
!  The following subroutine is the driver routine that computes the 
!   Airy function Ai(z) and its derivative for complex arguments.
!   The parameter PRD specifies the precision as determined by the 
!   module being used, e.g. single, double, quad.  The subroutines 
!   called are:
!     parameter_airy = determines the machine specific parameters
!     flowsc  = checks for over- and under- flow regions 
!     asympc  = asymptotic expansion (direct evaluation),
!     taylorc = taylor series expansion (integration).
!*
      INTEGER nn, iregion
      COMPLEX (prd) dg_z, dg_mz, g_z, g_mz, zeta_local, ai0s, ai1s
      COMPLEX (prd), DIMENSION (2,2) :: tsm

      IF ( .NOT. is_init_airy) CALL parameter_airy
      iflag = 0
      mod_local = .FALSE.
      arg_local = .FALSE.
      IF (present(modify_switch)) mod_local = modify_switch
      IF (present(argument_switch)) arg_local = argument_switch
      IF ( .NOT. arg_local) THEN
        r_global = abs(z) ! argument in rectangular form
        IF (r_global/=zero) THEN
          theta_global = atan2(aimag(z),real(z,prd))
        ELSE
          theta_global = zero
        END IF
        z_global = z
      ELSE
        r_global = z ! argument in polar form
        theta_global = aimag(z)
        IF (floor(abs(theta_global)/pi)/=0 .OR. r_global<zero) iflag = -10
        z_global = r_global*exp(ciunit*theta_global)
      END IF
      CALL flowsc
      IF (iflag==0 .OR. iflag==1) THEN
        zeta_global = two_third*z_global*sqrt(z_global)

!  choose the region where z lies according to the paper
        iregion = 0
        IF (r_global>=r_min) THEN
          IF (abs(theta_global)<=two_pi_third) THEN
            iregion = 1
          ELSE
            iregion = 2
          END IF
        ELSE
          IF (abs(theta_global)<=pi_thirds) THEN
            iregion = 3
            IF (r_global<=r_min/n_parts) iregion = 4
          ELSE
            iregion = 4
          END IF
        END IF

! call the appropriate subroutine(s) to evaluate Ai(z) and/or Ai'(z)
        SELECT CASE (iregion)
        CASE (1)
          CALL asympc(g_z,dg_z)
          ai0s = g_z
          ai1s = dg_z
        CASE (2)
          IF (iflag==1) THEN !  exp(2*zeta) < underflow limit
            CALL asympc(g_z,dg_z)
            ai0s = g_z
            ai1s = dg_z
          ELSE
            CALL asympc(g_z,dg_z,g_mz,dg_mz)
            IF (theta_global>zero) THEN
              ai0s = g_z + ciunit*g_mz*exp(two*zeta_global)
              ai1s = dg_z - ciunit*dg_mz*exp(two*zeta_global)
            ELSE
              ai0s = g_z - ciunit*g_mz*exp(two*zeta_global)
              ai1s = dg_z + ciunit*dg_mz*exp(two*zeta_global)
            END IF
          END IF
        CASE (3)
          nn = ceiling(n_parts*(one-r_global/r_min))
          CALL asympc(g_z,dg_z,r_in=r_min)
          CALL taylorc(nn,r_min,r_global,tsm)
          zeta_local = two_third*r_min*sqrt(r_min)*exp(ciunit*1.50_prd* &
            theta_global)
          zeta_local = zeta_global - zeta_local
          ai0s = exp(zeta_local)*(tsm(1,1)*g_z+tsm(1,2)*dg_z)
          ai1s = exp(zeta_local)*(tsm(2,1)*g_z+tsm(2,2)*dg_z)
        CASE (4)
          nn = ceiling(n_parts*r_global/r_min)
          CALL taylorc(nn,zero,r_global,tsm)
          ai0s = tsm(1,1)*ai0zer + tsm(1,2)*ai1zer
          ai1s = tsm(2,1)*ai0zer + tsm(2,2)*ai1zer
        END SELECT
        IF (iregion<=3 .AND. .NOT. mod_local) THEN
          ai0s = exp(-zeta_global)*ai0s
          ai1s = exp(-zeta_global)*ai1s
        ELSE IF (iregion==4 .AND. mod_local) THEN
          ai0s = exp(zeta_global)*ai0s
          ai1s = exp(zeta_global)*ai1s
        END IF
      END IF

!  error handling and return values
      SELECT CASE (iflag)
      CASE (:-1)
        ai = czero
        dai = czero
        ierr = iflag
      CASE (0:1)
        ai = ai0s
        dai = ai1s
        ierr = 0
      CASE (5)
        ai = cmplx(ai0zer,zero,prd) ! the value of Ai(0)
        dai = cmplx(ai1zer,zero,prd) ! the value of d(Ai(0))/dz
        ierr = 0
      CASE DEFAULT
        ai = czero
        dai = czero
        ierr = -2000
      END SELECT
    END SUBROUTINE airy_aic
!***
!************************************************************************
!***
    SUBROUTINE airy_ai_rayc(rin,zph,ai,dai,ierr,argument_switch,modify_switch)
      REAL (prd), INTENT (IN), DIMENSION (:) :: rin
      REAL (prd), INTENT (IN) :: zph
      COMPLEX (prd), INTENT (OUT), DIMENSION (:) :: ai, dai
      INTEGER, INTENT (OUT) :: ierr
      LOGICAL, INTENT (IN) :: argument_switch
      LOGICAL, INTENT (IN), OPTIONAL :: modify_switch
!*
!  This subroutine is the driver routine that computes the 
!   Airy function Ai(z) and its derivative for complex arguments
!   on rays directed from the origin.  The subroutines called are:
!     parameter_airy = determines the machine specific parameters
!     flowsc  = checks for over- and under- flow regions 
!     airy_aic  = asymptotic expansion (direct evaluation),
!*
      REAL (prd) h
      INTEGER iplace, i, j, iregion, iflag2
      COMPLEX (prd) ai0s, ai1s, g_z, dg_z, g_mz, dg_mz
      COMPLEX (prd), DIMENSION (2,2) :: tsm

      IF ( .NOT. is_init_airy) CALL parameter_airy
      iflag = 0
      iflag2 = 0
      IF (argument_switch .EQV. .FALSE.) THEN
        ai = czero
        dai = czero
        ierr = -10
        RETURN
      END IF
      arg_local = argument_switch
      mod_local = .FALSE.
      IF (present(modify_switch)) mod_local = modify_switch
      theta_global = zph

!  run the routine for each point on the ray
      DO j = 1, size(rin)
        r_global = rin(j)
        z_global = r_global*exp(ciunit*theta_global)
        IF (r_global<zero) iflag = -10
        CALL flowsc
        IF (iflag==0 .OR. iflag==1) THEN
          IF (j==1 .AND. minval(rin)<r_min) CALL biggrid
          zeta_global = two_third*z_global*sqrt(z_global)

!  choose the region where z lies according to the paper
          iregion = 0
          IF (r_global>=r_min) THEN
            IF (abs(theta_global)<=two_pi_third) THEN
              iregion = 1
            ELSE
              iregion = 2
            END IF
          ELSE
            IF (abs(theta_global)<=pi_thirds) THEN
              iregion = 3
            ELSE
              iregion = 4
            END IF
          END IF

!  call the appropriate subroutine to evaluate Ai(z) and/or Ai'(z)
          SELECT CASE (iregion)
          CASE (1)
            CALL asympc(g_z,dg_z)
            ai0s = g_z
            ai1s = dg_z
          CASE (2)
            IF (iflag==1) THEN !  exp(2*zeta) < underflow limit
              CALL asympc(g_z,dg_z)
              ai0s = g_z
              ai1s = dg_z
            ELSE
              CALL asympc(g_z,dg_z,g_mz,dg_mz)
              IF (theta_global>zero) THEN
                ai0s = g_z + ciunit*g_mz*exp(two*zeta_global)
                ai1s = dg_z - ciunit*dg_mz*exp(two*zeta_global)
              ELSE
                ai0s = g_z - ciunit*g_mz*exp(two*zeta_global)
                ai1s = dg_z + ciunit*dg_mz*exp(two*zeta_global)
              END IF
            END IF
          CASE (3:4)
            h = r_min/n_parts
            IF (iregion==3) THEN
              DO i = n_parts - 1, 0, -1
                IF (r_global>=h*i) THEN
                  iplace = i + 1
                  EXIT
                END IF
              END DO
            ELSE
              DO i = n_parts - 1, 0, -1
                IF (r_global>=h*i) THEN
                  iplace = i
                  EXIT
                END IF
              END DO
            END IF
            CALL taylorc(1,h*iplace,r_global,tsm)
            ai0s = tsm(1,1)*aigrid(1,iplace) + tsm(1,2)*aigrid(2,iplace)
            ai1s = tsm(2,1)*aigrid(1,iplace) + tsm(2,2)*aigrid(2,iplace)
          CASE DEFAULT
            ai0s = czero
            ai1s = czero
            iflag = -1000
          END SELECT
          IF ((iregion==1 .OR. iregion==2) .AND. .NOT. mod_local) THEN
            ai0s = exp(-zeta_global)*ai0s
            ai1s = exp(-zeta_global)*ai1s
          ELSE IF ((iregion==3 .OR. iregion==4) .AND. mod_local) THEN
            ai0s = exp(zeta_global)*ai0s
            ai1s = exp(zeta_global)*ai1s
          END IF
        END IF

!  error handling and return values
        SELECT CASE (iflag)
        CASE (:-1)
          ai(j) = czero
          dai(j) = czero
        CASE (0:1)
          ai(j) = ai0s
          dai(j) = ai1s
        CASE (5)
          ai(j) = cmplx(ai0zer,prd) ! the value of Ai(0)
          dai(j) = cmplx(ai1zer,prd) ! the value of d(Ai(0))/dz
        CASE DEFAULT
          ai(j) = czero
          dai(j) = czero
        END SELECT
        iflag2 = min(iflag2,iflag)
        iflag = 0
      END DO
      ierr = 0
      IF (iflag2<0) ierr = -20
    END SUBROUTINE airy_ai_rayc
!***
!************************************************************************
!***
    SUBROUTINE flowsc
!*
!  PRIVATE subroutine to check overflow and underflow regions
!*
      REAL (prd) tol, val

      IF (iflag/=0) RETURN

! underflow for small |z|
      IF (r_global<=r_lolimit) THEN
        iflag = 5
        RETURN
      END IF

! test to see if r**(3/2) is out of the range of floating point
      IF (r_global>=r_uplimit) THEN
        iflag = -6
        RETURN
      END IF

! underflow in exp(2*xi) for large |z| and 2*pi/3 < |arg(z)| < pi.
!   the value of the series is O(1), so once the difference between
!   exp(2*xi) and G_z is larger than epsilon(tol), then the 
!   contribution is negligible.  So we can make this tolerance 
!   significantly larger than underflow/overflow limit.  
      tol = min(log(huge(tol)),-log(tiny(tol))) - 15.0_prd
      val = abs(real(z_global*sqrt(z_global),prd))
      IF (abs(theta_global)>two_pi_third .AND. val>=0.75_prd*tol) THEN
        iflag = 1
      END IF
!   test to see if  multiplication by exp(=/-xi) for unscaled option
!         will over- or under-flow

!      tol = min( -log(two*sqrt(pi)*abs(z_global**0.25_prd)) - log(tiny(tol)), &
!	         -log(two*sqrt(pi)/abs(z_global**0.25_prd)) - log(tiny(tol)), &
!                  log(two*sqrt(pi)/abs(z_global**0.25_prd)) + log(huge(tol)), &
!                  log(two*sqrt(pi)*abs(z_global**0.25_prd)) + log(huge(tol)) ) &
!	         - 15.0_prd
      tol = sqrt(sqrt(abs(z_global)))
      tol = min(-log(two*sqrt(pi)*tol)-log(tiny(tol)),-log(two*sqrt( &
        pi)/tol)-log(tiny(tol)),log(two*sqrt(pi)/tol)+log(huge(tol)), &
        log(two*sqrt(pi)*tol)+log(huge(tol))) - 15.0_prd
      IF ( .NOT. mod_local) THEN
        IF (val>=1.50_prd*tol) THEN
          iflag = -7
          RETURN
        END IF
      END IF
    END SUBROUTINE flowsc
!***
!************************************************************************
!***
    SUBROUTINE asympc(g_z,dg_z,g_mz,dg_mz,r_in)
      COMPLEX (prd), INTENT (OUT) :: g_z, dg_z
      COMPLEX (prd), INTENT (OUT), OPTIONAL :: g_mz, dg_mz
      REAL (prd), INTENT (IN), OPTIONAL :: r_in
!*
!  PRIVATE subroutine to compute the asymptotic expansion for large |z|
!*
      INTEGER i
      COMPLEX (prd) zetar, z_local, ztemp

!  summation of asymptotic series is done using nested multiplication
!  Compute G(zeta), G(-zeta), G'(zeta), G'(-zeta)
      IF (present(r_in)) THEN
        z_local = r_in*exp(ciunit*theta_global)
        zetar = cunit/(two_third*z_local*sqrt(z_local))
      ELSE
        z_local = z_global
        zetar = cunit/zeta_global
      END IF
      g_z = cmplx(ucoef(n_asymp),zero,prd)
      dg_z = cmplx(vcoef(n_asymp),zero,prd)
      DO i = n_asymp - 1, 1, -1
        g_z = cmplx(ucoef(i),zero,prd) - zetar*g_z
        dg_z = cmplx(vcoef(i),zero,prd) - zetar*dg_z
      END DO
!      G_z   = (cunit - zetar*G_z)/(two_sqrpi*z_local**fourth)
!      dG_z  = -(cunit - zetar*dG_z)/two_sqrpi*z_local**fourth
      ztemp = sqrt(sqrt(z_local))
      g_z = (cunit-zetar*g_z)/(two_sqrpi*ztemp)
      dg_z = -(cunit-zetar*dg_z)/two_sqrpi*ztemp
      IF (present(g_mz)) THEN
        g_mz = cmplx(ucoef(n_asymp),zero,prd)
        dg_mz = cmplx(vcoef(n_asymp),zero,prd)
        DO i = n_asymp - 1, 1, -1
          g_mz = cmplx(ucoef(i),zero,prd) + zetar*g_mz
          dg_mz = cmplx(vcoef(i),zero,prd) + zetar*dg_mz
        END DO
!        G_mz   = (cunit + zetar*G_mz)/(two_sqrpi*z_local**fourth)
!        dG_mz  = -(cunit + zetar*dG_mz)/two_sqrpi*z_local**fourth
        ztemp = sqrt(sqrt(z_local))
        g_mz = (cunit+zetar*g_mz)/(two_sqrpi*ztemp)
        dg_mz = -(cunit+zetar*dg_mz)/two_sqrpi*ztemp
      END IF
    END SUBROUTINE asympc
!***
!************************************************************************
!***
    SUBROUTINE taylorc(nn,r_start,r_stop,tsm,index)
      INTEGER, INTENT (IN) :: nn
      REAL (prd), INTENT (IN) :: r_start, r_stop
      COMPLEX (prd), DIMENSION (2,2), INTENT (OUT) :: tsm
      INTEGER, OPTIONAL, INTENT (IN) :: index
!*
!  PRIVATE subroutine to integrate along a ray
!*
      INTEGER i, j
      COMPLEX (prd) h, xm, z_start, z_stop
      COMPLEX (prd), DIMENSION (0:n_taylor) :: pterm, qterm
      COMPLEX (prd), DIMENSION (nn,2,2) :: phi

      j = 0 ! local error indicator--trap small step size
      IF (r_start==zero .AND. abs(log(r_stop))<epsilon(one) .AND. abs(r_stop- &
        one)>two*epsilon(one)) j = 1
      IF (r_start/=zero .AND. r_stop/=zero) THEN
        IF (abs(log(r_start/r_stop))<epsilon(one)) j = 1
      END IF
      IF (j==1) THEN
        tsm = reshape((/cunit,czero,czero,cunit/),(/2,2/))
        RETURN
      END IF
      z_start = r_start*exp(ciunit*theta_global)
      z_stop = z_global
      IF (r_stop/=r_global) z_stop = r_stop*exp(ciunit*theta_global)
      h = (z_stop-z_start)/nn

! compute the Taylor series in each partition
      DO i = 1, nn
        xm = z_start + h*(i-1)

! compute the reduced derivatives:  
        pterm(0) = cunit
        pterm(1) = czero
        pterm(2) = xm*pterm(0)*half
        qterm(0) = czero
        qterm(1) = cunit
        qterm(2) = czero
        DO j = 3, n_taylor
          pterm(j) = (xm*pterm(j-2)+pterm(j-3))/real(j*j-j,prd)
          qterm(j) = (xm*qterm(j-2)+qterm(j-3))/real(j*j-j,prd)
        END DO

! now sum the series
        phi(i,1,1) = pterm(n_taylor)
        phi(i,2,1) = pterm(n_taylor)*n_taylor
        phi(i,1,2) = qterm(n_taylor)
        phi(i,2,2) = qterm(n_taylor)*n_taylor
        DO j = n_taylor - 1, 1, -1
          phi(i,1,1) = pterm(j) + h*phi(i,1,1)
          phi(i,2,1) = pterm(j)*j + h*phi(i,2,1)
          phi(i,1,2) = qterm(j) + h*phi(i,1,2)
          phi(i,2,2) = qterm(j)*j + h*phi(i,2,2)
        END DO
        phi(i,1,1) = pterm(0) + h*phi(i,1,1)
        phi(i,1,2) = qterm(0) + h*phi(i,1,2)
      END DO

! multiply all the matrices together to obtain the fundamental solution matrix
!   for the entire ray.
      DO i = 1, nn - 1
        phi(i+1,:,:) = matmul(phi(i+1,:,:),phi(i,:,:))
      END DO

! return values
      tsm(:,:) = phi(nn,:,:)

! populuate a vector full of function and derivative values that 
!   lie along a given value of theta
      IF (present(index)) THEN
        IF (abs(theta_global)<=pi_thirds) THEN
          DO i = 1, n_parts - 1
            aigrid(:,n_parts-i) = matmul(phi(i,:,:),aigrid(:,n_parts))
          END DO
        ELSE
          DO i = 1, n_parts - 1
            aigrid(:,i) = matmul(phi(i,:,:),aigrid(:,0))
          END DO
        END IF
      END IF
    END SUBROUTINE taylorc
!***
!************************************************************************
!***
    SUBROUTINE biggrid
!*
!  PRIVATE subroutine to generate the values of the Airy fucntions
!   at the grid points along a ray.
!*
      COMPLEX (prd), DIMENSION (2,2) :: tsm
      COMPLEX (prd) g_z, dg_z, g_mz, dg_mz, zeta_local

      IF ( .NOT. allocated(aigrid)) ALLOCATE (aigrid(2,0:n_parts))

! values at the origin are well known
      aigrid(1,0) = cmplx(ai0zer,zero,prd)
      aigrid(2,0) = cmplx(ai1zer,zero,prd)

! values on the circle are computed from the asymptotic expansions
      zeta_local = r_min*exp(ciunit*theta_global) ! dummy storage
      zeta_local = two_third*zeta_local*sqrt(zeta_local)
      IF (abs(theta_global)<=two_pi_third) THEN ! region (I)
        CALL asympc(g_z,dg_z,r_in=r_min)
        aigrid(1,n_parts) = g_z
        aigrid(2,n_parts) = dg_z
      ELSE ! region(II)
        CALL asympc(g_z,dg_z,g_mz,dg_mz,r_min)
        IF (theta_global>zero) THEN
          aigrid(1,n_parts) = (g_z+ciunit*g_mz*exp(two*zeta_local))
          aigrid(2,n_parts) = (dg_z-ciunit*dg_mz*exp(two*zeta_local))
        ELSE
          aigrid(1,n_parts) = (g_z-ciunit*g_mz*exp(two*zeta_local))
          aigrid(2,n_parts) = (dg_z+ciunit*dg_mz*exp(two*zeta_local))
        END IF
      END IF
      aigrid(:,n_parts) = exp(-zeta_local)*aigrid(:,n_parts)

! now integrate once and populate the vector of function and derivative
!   values on the grid
      IF (abs(theta_global)<=pi_thirds) THEN ! integrate towards the origin
        CALL taylorc(n_parts,r_min,zero,tsm,1)
      ELSE ! integrate away from the origin
        CALL taylorc(n_parts,zero,r_min,tsm,1)
      END IF
    END SUBROUTINE biggrid
!***
!************************************************************************
!***

!  end file airy_functions_complex

! begin file airy_zeros
!    version 1.0 
!    edited 07/24/2002

!***
!************************************************************************
!***
    SUBROUTINE ai_zeroc(n,ai_zero,ierr,ai_assoc,dai_zero,dai_assoc)
      INTEGER, INTENT (IN) :: n
      COMPLEX (prd), INTENT (OUT) :: ai_zero
      INTEGER, INTENT (OUT) :: ierr
      COMPLEX (prd), INTENT (OUT), OPTIONAL :: ai_assoc, dai_zero, dai_assoc
!*
!  PUBLIC subroutine which returns zeros with an error flag since  
!    Ai(z) and Ai'(z), do not have non-real zeros.   
!*
      ai_zero = czero
      IF (present(ai_assoc)) ai_assoc = czero
      IF (present(dai_zero)) dai_zero = czero
      IF (present(dai_assoc)) dai_assoc = czero
      ierr = n
      ierr = -3 ! the first is a dummy statement to use n 

    END SUBROUTINE ai_zeroc
!***
!************************************************************************
!***
    SUBROUTINE bi_zeroc(n,bi_zero,ierr,bi_assoc,dbi_zero,dbi_assoc)
      INTEGER, INTENT (IN) :: n
      COMPLEX (prd), INTENT (OUT) :: bi_zero
      INTEGER, INTENT (OUT) :: ierr
      COMPLEX (prd), INTENT (OUT), OPTIONAL :: bi_assoc, dbi_zero, dbi_assoc
!*
!  PUBLIC subroutine which is the driver routine that computes the 
!    zeros of Bi(z) and optionally of Bi'(z), and the associated 
!    values Bi'(beta_n) and Bi(beta_n') for scalar arguments.  The first 25   
!    zeros are stored in an array and the rest are computed by summing 
!    approriate asymptotic expansions.  The (PRIVATE) subroutines called are: 
!     parameters_airy = determines the machine specific parameters 
!     zero_parameters_airy = populates the vectors containing the  
!	stored zeros and the coefficients of the asymptotic expansions 
!     airy_bic = computes the associated function value for stored zeros 
!     ae_zero_c = sums the appropriate asymptotic expansion for the  
!	zeros of the Airy functions and their associated function values 
!*
      INTEGER itemp
      COMPLEX (prd) xbi, xdbi, lam

      IF ( .NOT. is_zero_init_airy) CALL zero_parameter_airy
      iflag = 0
      IF (n<=0) iflag = -3
      itemp = floor(0.25*(huge(itemp)-1))
      IF (n>=itemp) iflag = -4 ! zero requested cannot be computed
      IF (iflag<0) THEN
        bi_zero = zero
        IF (present(bi_assoc)) bi_assoc = czero
        IF (present(dbi_zero)) dbi_zero = czero
        IF (present(dbi_assoc)) dbi_assoc = czero
        ierr = iflag
        RETURN
      END IF
      IF (n<=n_zeros) THEN
        bi_zero = bizc(n)
        IF (present(bi_assoc)) THEN
          CALL airy_aic(bi_zero*exp(-ciunit*two_third*pi),lam,xdbi,ierr)
          CALL airy_aic(bi_zero*exp(ciunit*two_third*pi),lam,xbi,ierr)
          lam = 5.0_prd*pi*ciunit/six
          bi_assoc = exp(-lam)*xdbi + exp(lam)*xbi
        END IF
        IF (present(dbi_zero)) dbi_zero = dbizc(n)
        IF (present(dbi_assoc)) THEN
          CALL airy_aic(dbizc(n)*exp(-ciunit*two_third*pi),xdbi,lam,ierr)
          CALL airy_aic(dbizc(n)*exp(ciunit*two_third*pi),xbi,lam,ierr)
          lam = pi*ciunit/six
          dbi_assoc = exp(-lam)*xdbi + exp(lam)*xbi
        END IF
      ELSE
        lam = cmplx(three_pi_ate*real(4*n-1,prd),0.75_prd*log(2.0_prd),prd)
        CALL ae_zero_c(xbi,lam,1)
        bi_zero = exp(ciunit*pi/three)*xbi
        CALL ae_zero_c(xbi,lam,3)
        IF (present(bi_assoc)) THEN
          bi_assoc = sqrt(two)*exp(-ciunit*pi/six)*xbi
          IF (mod(n,2)==1) bi_assoc = -bi_assoc
        END IF
        IF (present(dbi_zero) .OR. present(dbi_assoc)) THEN
          lam = cmplx(three_pi_ate*real(4*n-3,prd),0.75_prd*log(2.0_prd),prd)
          CALL ae_zero_c(xbi,lam,2)
          IF (present(dbi_zero)) dbi_zero = exp(ciunit*pi/three)*xbi
          CALL ae_zero_c(xdbi,lam,4)
          IF (present(dbi_assoc)) THEN
            dbi_assoc = sqrt(two)*exp(ciunit*pi/six)*xdbi
            IF (mod(n-1,2)==1) dbi_assoc = -dbi_assoc
          END IF
        END IF
      END IF
    END SUBROUTINE bi_zeroc
!***
!************************************************************************
!***
    SUBROUTINE ae_zero_c(zr,lam,in)
      COMPLEX (prd), INTENT (OUT) :: zr
      COMPLEX (prd), INTENT (IN) :: lam
      INTEGER, INTENT (IN) :: in
!*
!  PRIVATE subroutine to sum the asymptotic expansions for the zeros 
!   of the Airy functions and their associated functions
!*
      COMPLEX (prd) zrsum, lamr
      INTEGER i

      lamr = one/lam**2
      SELECT CASE (in)
      CASE (1) ! sum the T(x) expansion
        zrsum = cmplx(tcoeff(n_asymp_zero),zero,prd)
        DO i = n_asymp_zero - 1, 0, -1
          zrsum = cmplx(tcoeff(i),zero,prd) + zrsum*lamr
        END DO
        zr = zrsum*lam**two_third
      CASE (2) ! sum the U(x) expansion
        zrsum = cmplx(ucoeff(n_asymp_zero),zero,prd)
        DO i = n_asymp_zero - 1, 0, -1
          zrsum = cmplx(ucoeff(i),zero,prd) + zrsum*lamr
        END DO
        zr = zrsum*lam**two_third
      CASE (3) ! sum the V(x) expansion
        zrsum = cmplx(vcoeff(n_asymp_asso),zero,prd)
        DO i = n_asymp_asso - 1, 0, -1
          zrsum = cmplx(vcoeff(i),zero,prd) + zrsum*lamr
        END DO
        zr = zrsum*lam**(half/three)/sqrpi
      CASE (4) ! sum the W(x) expansion
        zrsum = cmplx(wcoeff(n_asymp_asso),zero,prd)
        DO i = n_asymp_asso - 1, 0, -1
          zrsum = cmplx(wcoeff(i),zero,prd) + zrsum*lamr
        END DO
        zr = zrsum/lam**(half/three)/sqrpi
      END SELECT
    END SUBROUTINE ae_zero_c
!***
!************************************************************************
!***
    SUBROUTINE ai_zerocv(n,ai_zero,ierr,ai_assoc,dai_zero,dai_assoc)
      INTEGER, INTENT (IN) :: n
      COMPLEX (prd), INTENT (OUT), DIMENSION (:) :: ai_zero
      INTEGER, INTENT (OUT) :: ierr
      COMPLEX (prd), INTENT (OUT), DIMENSION (:), OPTIONAL :: ai_assoc, &
        dai_zero, dai_assoc
!*
!*
!  PUBLIC subroutine which returns zeros with an error flag since  
!    Ai(z) and Ai'(z), do not have non-real zeros.   
!*
      ai_zero = czero
      IF (present(ai_assoc)) ai_assoc = czero
      IF (present(dai_zero)) dai_zero = czero
      IF (present(dai_assoc)) dai_assoc = czero
      ierr = n
      ierr = -3 ! the first is a dummy statement to use n 


    END SUBROUTINE ai_zerocv
!***
!************************************************************************
!***
    SUBROUTINE bi_zerocv(n,bi_zero,ierr,bi_assoc,dbi_zero,dbi_assoc)
      INTEGER, INTENT (IN) :: n
      COMPLEX (prd), INTENT (OUT), DIMENSION (:) :: bi_zero
      INTEGER, INTENT (OUT) :: ierr
      COMPLEX (prd), INTENT (OUT), DIMENSION (:), OPTIONAL :: bi_assoc, &
        dbi_zero, dbi_assoc
!*
!*
!  PUBLIC subroutine which is the driver routine that computes the 
!    zeros of Bi(z) and optionally of Bi'(z), and the associated 
!    values Bi'(beta_n) and Bi(beta_n') for vector arguments.  The first 25   
!    zeros are stored in an array and the rest are computed by summing 
!    approriate asymptotic expansions.  The (PRIVATE) subroutines called are: 
!     parameters_airy = determines the machine specific parameters 
!     zero_parameters_airy = populates the vectors containing the  
!	stored zeros and the coefficients of the asymptotic expansions 
!     airy_bic = computes the associated function value for stored zeros 
!     ae_zero_cv = sums the appropriate asymptotic expansion for the  
!	zeros of the Airy functions and their associated function values 
!*
      INTEGER itemp, k, i, iflag_zero
      COMPLEX (prd) xbi, xdbi, lam1
      COMPLEX (prd), DIMENSION (:), ALLOCATABLE :: lam

      IF ( .NOT. is_zero_init_airy) CALL zero_parameter_airy
      iflag_zero = 0
      IF (n<=0) iflag_zero = -3
      itemp = floor(0.25*(huge(itemp)-1))
      IF (n>=itemp) iflag_zero = -4 ! zero requested cannot be computed
      k = size(bi_zero)

! check to see if the last zero requested is computable
!        and change the last zero returned accordingly.
      IF (n+k>=itemp .AND. iflag_zero/=-4) THEN
        DO i = n + k - 1, n, -1
          IF (i<itemp) THEN
            k = i - n
            iflag_zero = 50
            EXIT
          END IF
        END DO
      END IF
      IF (iflag_zero<0) THEN
        bi_zero(:) = czero
        IF (present(bi_assoc)) bi_assoc(:) = czero
        IF (present(dbi_zero)) dbi_zero(:) = czero
        IF (present(dbi_assoc)) dbi_assoc(:) = czero
        ierr = iflag_zero
        RETURN
      END IF
      IF (n<=n_zeros) THEN
        itemp = min(n+k,n_zeros)
        bi_zero(1:itemp-n+1) = bizc(n:itemp)
        IF (present(bi_assoc)) THEN
          DO i = n, itemp
            CALL airy_aic(bizc(i)*exp(-ciunit*two_third*pi),lam1,xdbi,ierr)
            CALL airy_aic(bizc(i)*exp(ciunit*two_third*pi),lam1,xbi,ierr)
            lam1 = 5.0_prd*pi*ciunit/six
            bi_assoc(i-n+1) = exp(-lam1)*xdbi + exp(lam1)*xbi
          END DO
        END IF
        IF (present(dbi_zero)) dbi_zero(1:itemp-n+1) = dbizc(n:itemp)
        IF (present(dbi_assoc)) THEN
          DO i = n, itemp
            CALL airy_aic(dbizc(i)*exp(-ciunit*two_third*pi),xdbi,lam1,ierr)
            CALL airy_aic(dbizc(i)*exp(ciunit*two_third*pi),xbi,lam1,ierr)
            lam1 = pi*ciunit/six
            dbi_assoc(i-n+1) = exp(-lam1)*xdbi + exp(lam1)*xbi
          END DO
        END IF
      END IF
      IF (n+k>n_zeros) THEN
        itemp = max(n,n_zeros+1)
        ALLOCATE (lam(itemp:n+k-1))
        DO i = itemp, n + k - 1
          lam(i) = cmplx(three_pi_ate*real(4*i-1,prd),0.75_prd*log(2.0_prd), &
            prd)
        END DO
        CALL ae_zero_cv(bi_zero(itemp-n+1:k),lam,1)
        bi_zero(itemp-n+1:k) = exp(ciunit*pi/three)*bi_zero(itemp-n+1:k)
        IF (present(bi_assoc)) THEN
          CALL ae_zero_cv(bi_assoc(itemp-n+1:k),lam,3)
          bi_assoc(itemp-n+1:k) = sqrt(two)*exp(-ciunit*pi/six)* &
            bi_assoc(itemp-n+1:k)
          DO i = itemp, n + k - 1
            IF (mod(i,2)==1) bi_assoc(i-n+1) = -bi_assoc(i-n+1)
          END DO
        END IF
        DEALLOCATE (lam)
        IF (present(dbi_zero) .OR. present(dbi_assoc)) THEN
          ALLOCATE (lam(itemp:n+k-1))
          DO i = itemp, n + k - 1
            lam(i) = cmplx(three_pi_ate*real(4*i-3,prd),0.75_prd*log(2.0_prd), &
              prd)
          END DO
          IF (present(dbi_zero)) THEN
            CALL ae_zero_cv(dbi_zero(itemp-n+1:k),lam,2)
            dbi_zero(itemp-n+1:k) = exp(ciunit*pi/three)*dbi_zero(itemp-n+1:k)
          END IF
          IF (present(dbi_assoc)) THEN
            CALL ae_zero_cv(dbi_assoc(itemp-n+1:k),lam,4)
            dbi_assoc(itemp-n+1:k) = sqrt(two)*exp(ciunit*pi/six)* &
              dbi_assoc(itemp-n+1:k)
            DO i = itemp, n + k - 1
              IF (mod(i-1,2)==1) dbi_assoc(i-n+1) = -dbi_assoc(i-n+1)
            END DO
          END IF
          DEALLOCATE (lam)
        END IF
      END IF
      ierr = iflag_zero
    END SUBROUTINE bi_zerocv
!***
!************************************************************************
!***
    SUBROUTINE ae_zero_cv(zr,lam,in)
      COMPLEX (prd), INTENT (OUT), DIMENSION (1:) :: zr
      COMPLEX (prd), INTENT (IN), DIMENSION (1:) :: lam
      INTEGER, INTENT (IN) :: in
      COMPLEX (prd), DIMENSION (size(zr)) :: zrsum, lamr
!*
!  PRIVATE subroutine to sum the asymptotic expansions for the zeros 
!   of the Airy functions and their associated functions for the 
!   vector case
!*
      INTEGER k, i, j

      k = size(zr)
      DO i = 1, k
        lamr(i) = cunit/lam(i)**2
      END DO
      IF (in==1) THEN ! sum the T(x) expansion
        DO j = 1, k
          zrsum(j) = cmplx(tcoeff(n_asymp_zero),zero,prd)
          DO i = n_asymp_zero - 1, 0, -1
            zrsum(j) = cmplx(tcoeff(i),zero,prd) + zrsum(j)*lamr(j)
          END DO
        END DO
        zr(:) = zrsum(:)*lam(:)**two_third
      ELSE IF (in==2) THEN ! sum the U(x) expansion
        DO j = 1, k
          zrsum(j) = cmplx(ucoeff(n_asymp_zero),zero,prd)
          DO i = n_asymp_zero, 0, -1
            zrsum(j) = cmplx(ucoeff(i),zero,prd) + zrsum(j)*lamr(j)
          END DO
        END DO
        zr(:) = zrsum(:)*lam(:)**two_third
      ELSE IF (in==3) THEN ! sum the V(x) expansion
        DO j = 1, k
          zrsum(j) = cmplx(vcoeff(n_asymp_asso),zero,prd)
          DO i = n_asymp_asso - 1, 0, -1
            zrsum(j) = cmplx(vcoeff(i),zero,prd) + zrsum(j)*lamr(j)
          END DO
        END DO
        zr(:) = zrsum(:)*lam(:)**(half/three)/sqrpi
      ELSE ! sum the W(x) expansion
        DO j = 1, k
          zrsum(j) = cmplx(wcoeff(n_asymp_asso),zero,prd)
          DO i = n_asymp_asso - 1, 0, -1
            zrsum(j) = cmplx(wcoeff(i),zero,prd) + zrsum(j)*lamr(j)
          END DO
        END DO
        zr(:) = zrsum(:)/lam(:)**(half/three)/sqrpi
      END IF
    END SUBROUTINE ae_zero_cv
!***
!************************************************************************
!***

!  end file airy_zeros
