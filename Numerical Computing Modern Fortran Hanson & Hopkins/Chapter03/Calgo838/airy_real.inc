
! begin file airy_functions_real   
!    version 1.0 
!    edited 07/24/2003

!***
!************************************************************************
!***
    SUBROUTINE airy_air(x,ai,dai,ierr,modify_switch)
      REAL (prd), INTENT (OUT) :: ai, dai
      INTEGER, INTENT (OUT) :: ierr
      REAL (prd), INTENT (IN) :: x
      LOGICAL, INTENT (IN), OPTIONAL :: modify_switch
!*
!  PUBLIC subroutine which is the driver routine that computes Ai(x)  
!    and its derivative.  The (PRIVATE) subroutines called are:
!     parameter_airy = determines the machine specific parameters
!     flows   = checks for over- and under- flow regions 
!     asymp1r = asymptotic expansion for x > 0 
!     asymp2r = asymptotic expansion for x < 0
!     taylorr = taylor series expansion (integration)
!*
      INTEGER nn, iregion
      REAL (prd) pxi, qxi, rxi, sxi, ai0s, ai1s, a, b, xi0
      REAL (prd), DIMENSION (2,2) :: tsm

!  check for some quick under- and over- flow regions
      IF ( .NOT. is_init_airy) CALL parameter_airy
      iflag = 0
      mod_local = .FALSE.
      IF (present(modify_switch)) mod_local = modify_switch
      r_global = abs(x)
      x_global = x
      IF (x_global<zero .AND. mod_local) ierr = -1
      CALL flows
      IF (iflag==0) THEN
        xi_global = two_third*r_global*sqrt(r_global)

!  choose the region where x lies and call the appropriate subroutine
!    to evaluate Ai(x) and/or Ai'(x)
        iregion = 0
        IF (r_global>=r_min) THEN
          IF (x_global>zero) THEN
            iregion = 1
          ELSE
            iregion = 2
          END IF
        ELSE
          IF (x_global>zero) THEN
            iregion = 3
          ELSE
            iregion = 4
          END IF
        END IF

!  call the appropriate subroutine(s) to evaluate Ai(x) and/or Ai'(x)
        SELECT CASE (iregion)
        CASE (1)
          ai1s = two*sqrpi ! dummy storage of number
!            ai0s = x_global**fourth      ! dummy storage of number
          ai0s = sqrt(sqrt(x_global)) ! dummy storage of number
          a = one/(ai1s*ai0s)
          b = -ai0s/ai1s
          iflag = 10 ! indicates Ai functions to asymp1r
          CALL asymp1r(ai0s,ai1s)
          ai0s = a*ai0s
          ai1s = b*ai1s
          IF ( .NOT. mod_local) THEN
            ai0s = exp(-xi_global)*ai0s
            ai1s = exp(-xi_global)*ai1s
          END IF
        CASE (2)
!            ai1s = r_global**fourth             ! dummy storage of number
          ai1s = sqrt(sqrt(r_global)) ! dummy storage of number
          a = one/(sqrpi*ai1s)
          b = ai1s/sqrpi
          CALL asymp2r(pxi,qxi,rxi,sxi)
          ai1s = xi_global - pi*fourth ! dummy storage of number
          ai0s = a*(pxi*cos(ai1s)+qxi*sin(ai1s))
          ai1s = b*(rxi*sin(ai1s)-sxi*cos(ai1s))
        CASE (3)
          nn = ceiling(n_parts*(one-r_global/r_min))
          ai1s = two*sqrpi !  dummy storage of number
!            ai0s  = r_min**fourth         !  dummy storage of number
          ai0s = sqrt(sqrt(r_min)) !  dummy storage of number
          a = one/(ai1s*ai0s)
          b = -ai0s/ai1s
          xi0 = two_third*r_min*sqrt(r_min)
          iflag = 10 ! indicates Ai function to subroutine asymp1r
          CALL asymp1r(ai0s,ai1s,xi0)
          CALL taylorr(nn,r_min,tsm)
          a = a*exp(-xi0)*ai0s
          b = b*exp(-xi0)*ai1s
          ai0s = tsm(1,1)*a + tsm(1,2)*b
          ai1s = tsm(2,1)*a + tsm(2,2)*b
          IF (mod_local) THEN
            ai0s = exp(xi_global)*ai0s
            ai1s = exp(xi_global)*ai1s
          END IF
        CASE (4)
          nn = ceiling(n_parts*r_global/r_min)
          CALL taylorr(nn,zero,tsm)
          ai0s = tsm(1,1)*ai0zer + tsm(1,2)*ai1zer
          ai1s = tsm(2,1)*ai0zer + tsm(2,2)*ai1zer
        END SELECT
      END IF

!  error handling and return values
      SELECT CASE (iflag)
      CASE (:-1)
        ai = zero
        dai = zero
        ierr = iflag
      CASE (0:2)
        ai = ai0s
        dai = ai1s
        ierr = 0
      CASE (5)
        ai = ai0zer ! the value of Ai(0)
        dai = ai1zer ! the value of d(Ai(0))/dx
        ierr = 0
      END SELECT
    END SUBROUTINE airy_air
!***
!************************************************************************
!***
    SUBROUTINE airy_bir(x,bi,dbi,ierr,modify_switch)
      REAL (prd), INTENT (OUT) :: bi, dbi
      INTEGER, INTENT (OUT) :: ierr
      REAL (prd), INTENT (IN) :: x
      LOGICAL, INTENT (IN), OPTIONAL :: modify_switch
!*
!  PUBLIC subroutine which is the driver routine that computes Bi(x) and 
!    its derivative.  The (PRIVATE) subroutines called are:
!     parameter_airy = determines the machine specific parameters
!     flows   = checks for over- and under- flow regions 
!     asymp1r = asymptotic expansion for x > 0
!     asymp2r = asymptotic expansion for x < 0
!     taylorr = taylor series expansion (integration)
!*
      INTEGER nn, iregion
      REAL (prd) pxi, qxi, rxi, sxi, bi0s, bi1s, a, b
      REAL (prd), DIMENSION (2,2) :: tsm

!  check for some quick under- and over- flow regions
      IF ( .NOT. is_init_airy) CALL parameter_airy
      iflag = 0
      mod_local = .FALSE.
      IF (present(modify_switch)) mod_local = modify_switch
      r_global = abs(x)
      x_global = x
      IF (x<zero .AND. mod_local) iflag = -1
      CALL flows
      IF (iflag==0) THEN
        xi_global = two_third*r_global*sqrt(r_global)

!  choose the region where x lies and call the appropriate subroutine
!    to evaluate Bi(x) and/or Bi'(x)
        iregion = 0
        IF (r_global>=r_min) THEN
          IF (x_global>zero) THEN
            iregion = 1
          ELSE
            iregion = 2
          END IF
        ELSE
          iregion = 3
        END IF
        SELECT CASE (iregion)
        CASE (1)
!            bi0s = x_global**fourth             ! dummy storage of number
          bi0s = sqrt(sqrt(x_global)) ! dummy storage of number
          a = one/(sqrpi*bi0s)
          b = bi0s/sqrpi
          CALL asymp1r(bi0s,bi1s)
          bi0s = a*bi0s
          bi1s = b*bi1s
          IF ( .NOT. mod_local) THEN
            bi0s = exp(xi_global)*bi0s
            bi1s = exp(xi_global)*bi1s
          END IF
        CASE (2)
!            bi1s = r_global**fourth             ! dummy storage of number
          bi1s = sqrt(sqrt(r_global)) ! dummy storage of number
          a = one/(sqrpi*bi1s)
          b = bi1s/sqrpi
          CALL asymp2r(pxi,qxi,rxi,sxi)
          bi1s = xi_global - pi*fourth ! dummy storage of number
          bi0s = a*(-pxi*sin(bi1s)+qxi*cos(bi1s))
          bi1s = b*(rxi*cos(bi1s)+sxi*sin(bi1s))
        CASE (3)
          nn = ceiling(n_parts*r_global/r_min)
          CALL taylorr(nn,zero,tsm)
          bi0s = tsm(1,1)*bi0zer + tsm(1,2)*bi1zer
          bi1s = tsm(2,1)*bi0zer + tsm(2,2)*bi1zer
          IF (mod_local) THEN
            bi0s = exp(-xi_global)*bi0s
            bi1s = exp(-xi_global)*bi1s
          END IF
        END SELECT
      END IF

!  error handling and return values
      SELECT CASE (iflag)
      CASE (:-1)
        bi = zero
        dbi = zero
        ierr = iflag
      CASE (0:2)
        bi = bi0s
        dbi = bi1s
        ierr = 0
      CASE (5)
        bi = bi0zer ! the value of Bi(0)
        dbi = bi1zer ! the value of d(Bi(0))/dx
        ierr = 0
      END SELECT
    END SUBROUTINE airy_bir
!***
!************************************************************************
!***
    SUBROUTINE flows
!*
!  PRIVATE subroutine to check overflow and underflow regions
!*
      REAL (prd) tol

      IF (iflag/=0) RETURN
! test for underflow for small |z|
      IF (r_global<=r_lolimit) THEN
        iflag = 5
        RETURN
      END IF

! test for overflow for large |z|
      IF (r_global>=r_uplimit) THEN
        iflag = -6
        RETURN
      END IF

! test the unscaled functions
      IF ( .NOT. mod_local .AND. x_global>zero) THEN

! test to see if multiplication by exp(-xi) will underflow
!         or if multiplication by exp(xi) for x>0 will over-flow
!      tol = min( -log(two_sqrpi*r_global**fourth) - log(tiny(tol)), &
!                 -log(two_sqrpi/r_global**fourth) - log(tiny(tol)), &
!                  log(two_sqrpi/r_global**fourth) + log(huge(tol)), &
!                  log(two_sqrpi*r_global**fourth) + log(huge(tol)) ) &
!                - 15.0_prd
        tol = sqrt(sqrt(r_global))
        tol = min(-log(two_sqrpi*tol)-log(tiny(tol)), &
          -log(two_sqrpi/tol)-log(tiny(tol)),log(two_sqrpi/tol)+log(huge( &
          tol)),log(two_sqrpi*tol)+log(huge(tol))) - 15.0_prd
        IF (r_global>=(1.50_prd*tol)**two_third) THEN
          iflag = -7
          RETURN
        END IF
      END IF
    END SUBROUTINE flows
!***
!************************************************************************
!***
    SUBROUTINE asymp1r(f1,f2,xi_in)
      REAL (prd), INTENT (OUT) :: f1, f2
      REAL (prd), INTENT (IN), OPTIONAL :: xi_in
!*
!  PRIVATE subroutine to compute the asymptotic expansions as x->infty
!*
      INTEGER i
      REAL (prd) xir

!  summation of asymptotic series is done using nested multiplication
!  Compute G(xi), G'(xi)
      f1 = ucoef(n_asymp)
      f2 = vcoef(n_asymp)
      IF (present(xi_in)) THEN
        xir = one/xi_in
      ELSE
        xir = one/xi_global
      END IF
      IF (iflag==10) xir = -xir ! Ai(x) is an alternating series
      DO i = n_asymp - 1, 1, -1
        f1 = ucoef(i) + xir*f1
        f2 = vcoef(i) + xir*f2
      END DO
      f1 = (one+xir*f1)
      f2 = (one+xir*f2)
      iflag = 0
    END SUBROUTINE asymp1r
!***
!************************************************************************
!***
    SUBROUTINE asymp2r(pxi,qxi,rxi,sxi)
      REAL (prd), INTENT (OUT) :: pxi, qxi, rxi, sxi
!*
!  PRIVATE subroutine to compute the asymptotic expansions as x->-infty
!*
      INTEGER i, itemp
      REAL (prd) xir

!  summation of asymptotic series is done using nested multiplication
!  Compute G(xi), G'(xi)
      itemp = floor(half*n_asymp-half)
      IF (mod(itemp,2)/=0) itemp = itemp - 1
      pxi = ucoef(itemp)
      qxi = ucoef(itemp-1)
      rxi = vcoef(itemp)
      sxi = vcoef(itemp-1)
      xir = one/xi_global**2
      DO i = itemp - 2, 2, -2
        pxi = ucoef(i) - xir*pxi
        rxi = vcoef(i) - xir*rxi
        qxi = ucoef(i-1) - xir*qxi
        sxi = vcoef(i-1) - xir*sxi
      END DO
      pxi = one - xir*pxi
      rxi = one - xir*rxi
      qxi = qxi/xi_global
      sxi = sxi/xi_global
    END SUBROUTINE asymp2r
!***
!************************************************************************
!***
    SUBROUTINE taylorr(nn,x_start,tsm)
      INTEGER, INTENT (IN) :: nn
      REAL (prd), INTENT (IN) :: x_start
      REAL (prd), DIMENSION (2,2), INTENT (OUT) :: tsm
!*
!  PRIVATE subroutine to integrate along a ray
!*
      INTEGER i, j
      REAL (prd) h, xm
      REAL (prd), DIMENSION (0:n_taylor) :: pterm, qterm
      REAL (prd), DIMENSION (nn,2,2) :: phi

      h = (x_global-x_start)/nn ! step size and integration direction

! compute the Taylor series in each partition
      DO i = 1, nn
        xm = (x_start+h*(i-1)) !  xm always appears with h**2

! compute the reduced derivatives:
        pterm(0) = one
        pterm(1) = zero
        pterm(2) = xm*pterm(0)*half
        qterm(0) = zero
        qterm(1) = one
        qterm(2) = zero
        DO j = 3, n_taylor

! compute the next Taylor series term for each partition.
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

! multiply all the matrices together
      DO i = 1, nn - 1
        phi(i+1,:,:) = matmul(phi(i+1,:,:),phi(i,:,:))
      END DO

! return values
      tsm(:,:) = phi(nn,:,:)
    END SUBROUTINE taylorr
!***
!************************************************************************
!***

!  end file airy_functions_real

! begin file airy_aux
!    version 1.0 
!    edited 07/25/2003

!***
!************************************************************************
!***
    SUBROUTINE airy_auxr(x,airy_mod,airy_phase,ierr,dairy_mod,dairy_phase)
      REAL (prd), INTENT (IN) :: x
      REAL (prd), INTENT (OUT) :: airy_mod, airy_phase
      REAL (prd), INTENT (OUT), OPTIONAL :: dairy_mod, dairy_phase
      INTEGER, INTENT (OUT) :: ierr
!*
!  PUBLIC subroutine is the driver routine that computes the modulus 
!    and phase functions for Ai(x) and Bi(x).  The (PRIVATE) subroutines 
!    called are:
!     parameter_airy = determines the machine specific parameters
!     aux_parameter_airy = determines the machine specific parameters
!        specific to the auxiliary functions
!     asymp_aux = asymptotic expansion for auxiliary functions
!     taylor_modulus = taylor series integration for modulus functions M(x)
!        and N(x)
!     taylor_phase = taylor series integration for phase functions theta(x)
!        and phi(x)
!*
      REAL (prd) m_inter, n_inter, theta_inter, phi_inter
      REAL (prd), DIMENSION (3) :: tsm

! initialize all parameters
      IF ( .NOT. is_aux_init_airy) CALL aux_parameter_airy

! check to see if the main grid has been allocated and populated.
      IF ( .NOT. big_integrate_aux) THEN
        CALL taylor_modulus(n_parts_aux,tsm)
        CALL taylor_phase(n_parts_aux,theta_inter)
      END IF
      iflag = 0
      x_global = x
      r_global = abs(x)
      CALL flows
      IF (x>zero) iflag = -2
      IF (iflag==0) THEN

!  choose the region where x lies and call the appropriate subroutine
!    to evaluate Ai(x) and/or Ai'(x)
RTEST:  IF (x_global<=-aux_min) THEN
          CALL asymp_aux(m_inter,n_inter,theta_inter,phi_inter)
        ELSE

!  integrate the odes to obtain the functions and phases values
!   using one step from the grid point to the desired point
          CALL taylor_modulus(1,tsm)
          CALL taylor_phase(1,theta_inter)
          m_inter = sqrt(tsm(1))
          IF (present(dairy_mod)) n_inter = sqrt(tsm(2)**2*fourth/tsm(1)+one/( &
            pi**2*tsm(1)))
          IF (present(dairy_phase)) phi_inter = theta_inter - &
            atan(two/tsm(2)/pi)
        END IF RTEST
      END IF

!  error handling and return values
      SELECT CASE (iflag)
      CASE (:-1)
        airy_mod = zero
        airy_phase = zero
        IF (present(dairy_mod)) dairy_mod = zero
        IF (present(dairy_phase)) dairy_phase = zero
        ierr = iflag
      CASE (0)
        airy_mod = m_inter
        airy_phase = theta_inter
        IF (present(dairy_mod)) dairy_mod = n_inter
        IF (present(dairy_phase)) dairy_phase = phi_inter
        ierr = 0
      CASE (5)
        airy_mod = sqrt(m2grid(0,1))
        airy_phase = pi/six
        IF (present(dairy_mod)) dairy_mod = sqrt(ai1zer**2+bi1zer**2)
        IF (present(dairy_phase)) dairy_phase = -pi/six
        ierr = 0
      END SELECT
    END SUBROUTINE airy_auxr
!***
!************************************************************************
!***
    SUBROUTINE asymp_aux(m_inter,n_inter,theta_inter,phi_inter)
      REAL (prd), INTENT (OUT) :: m_inter, n_inter, theta_inter, phi_inter
!*
!  PRIVATE subroutine to compute the asymptotic expansions as x->-infty
!*
      INTEGER i
      REAL (prd) darg1, darg2, darg3

!  summation of asymptotic series is done using nested multiplication
!  Compute the modulus functions
      darg1 = mcoef(n_asymp_mod)
      darg2 = ncoef(n_asymp_mod)
      darg3 = one/(x_global**3)
      DO i = n_asymp_mod - 1, 1, -1
        darg1 = mcoef(i) + darg3*darg1
        darg2 = ncoef(i) + darg3*darg2
      END DO
      darg1 = (one+darg3*darg1)/pi/sqrt(-x_global)
      darg2 = (one+darg3*darg2)*sqrt(-x_global)/pi
      m_inter = sqrt(darg1)
      n_inter = sqrt(darg2)

!  Compute the phase functions
      darg1 = thetacoeff(n_asymp_phase)
      darg2 = phicoeff(n_asymp_phase)
      DO i = n_asymp_phase - 1, 1, -1
        darg1 = thetacoeff(i) + darg3*darg1
        darg2 = phicoeff(i) + darg3*darg2
      END DO
      darg1 = (thetacoeff(0)+darg3*darg1)*two_third*sqrt(-x_global)**3
      darg2 = (phicoeff(0)+darg3*darg2)*two_third*sqrt(-x_global)**3
      theta_inter = pi*fourth + darg1
      phi_inter = -pi*fourth + darg2

    END SUBROUTINE asymp_aux
!***
!************************************************************************
!***
    SUBROUTINE taylor_modulus(nn,tsm)
      INTEGER, INTENT (IN) :: nn
      REAL (prd), DIMENSION (3), INTENT (OUT) :: tsm
!*
!  PRIVATE subroutine to integrate for the modulus function M^2(x)
!   and the corresponding phase function theta(x).
!   we check to see if the main grid has been computed and 
!   integrate from the closest partition point to the one desired.
!*
      INTEGER iplace, j, i
      REAL (prd) h, xright, xm
      REAL (prd), DIMENSION (0:n_taylor_aux) :: pterm, qterm, rterm
      REAL (prd), DIMENSION (nn,3,3) :: phi

      h = -aux_min/n_parts_aux
      IF (nn==1) THEN

! find the grid points between which x lies
        DO i = n_parts_aux - 1, 0, -1
          IF (x_global<=h*i) THEN
            xright = h*i
            iplace = i
            EXIT
          END IF
        END DO
        h = x_global - xright
      END IF

! compute the Taylor series in each partition
      DO i = 1, nn
        xm = h*(i-1)
        IF (nn==1) xm = xright

! compute the reduced derivatives:
        pterm(0) = one
        pterm(1) = zero
        pterm(2) = zero
        qterm(0) = zero
        qterm(1) = one
        qterm(2) = zero
        rterm(0) = zero
        rterm(1) = zero
        rterm(2) = half
        DO j = 3, n_taylor_aux
          pterm(j) = (pterm(j-3)*(4*j-10)+four*xm*(j-2)*pterm(j-2))/ &
            real(j*(j-1)*(j-2),prd)
          qterm(j) = (qterm(j-3)*(4*j-10)+four*xm*(j-2)*qterm(j-2))/ &
            real(j*(j-1)*(j-2),prd)
          rterm(j) = (rterm(j-3)*(4*j-10)+four*xm*(j-2)*rterm(j-2))/ &
            real(j*(j-1)*(j-2),prd)
        END DO

! sum the series
        phi(i,:,1) = pterm(n_taylor_aux)
        phi(i,:,2) = qterm(n_taylor_aux)
        phi(i,:,3) = rterm(n_taylor_aux)
        phi(i,2,:) = phi(i,2,:)*n_taylor_aux
        phi(i,3,:) = phi(i,3,:)*n_taylor_aux*(n_taylor_aux-1)
        DO j = n_taylor_aux - 1, 2, -1
          phi(i,1,1) = h*phi(i,1,1) + pterm(j)
          phi(i,2,1) = h*phi(i,2,1) + pterm(j)*j
          phi(i,3,1) = h*phi(i,3,1) + pterm(j)*j*(j-1)
          phi(i,1,2) = h*phi(i,1,2) + qterm(j)
          phi(i,2,2) = h*phi(i,2,2) + qterm(j)*j
          phi(i,3,2) = h*phi(i,3,2) + qterm(j)*j*(j-1)
          phi(i,1,3) = h*phi(i,1,3) + rterm(j)
          phi(i,2,3) = h*phi(i,2,3) + rterm(j)*j
          phi(i,3,3) = h*phi(i,3,3) + rterm(j)*j*(j-1)
        END DO
! j = 1 term
        phi(i,1,1) = h*phi(i,1,1) + pterm(1)
        phi(i,2,1) = h*phi(i,2,1) + pterm(1)
        phi(i,1,2) = h*phi(i,1,2) + qterm(1)
        phi(i,2,2) = h*phi(i,2,2) + qterm(1)
        phi(i,1,3) = h*phi(i,1,3) + rterm(1)
        phi(i,2,3) = h*phi(i,2,3) + rterm(1)
! j = 0 term
        phi(i,1,1) = h*phi(i,1,1) + pterm(0)
        phi(i,1,2) = h*phi(i,1,2) + qterm(0)
        phi(i,1,3) = h*phi(i,1,3) + rterm(0)
      END DO

! return values
      IF (nn==1) THEN
        tsm(:) = phi(1,:,1)*m2grid(iplace,1) + phi(1,:,2)*m2grid(iplace,2) + &
          phi(1,:,3)*m2grid(iplace,3)
      ELSE

! obtain the function, first, and second derivative 
!   values at each partition point and store them 
!   into a global module variable
        ALLOCATE (m2grid(0:n_parts_aux,3))

! initial conditions for M^2(0)
        m2grid(0,1) = ai0zer**2 + bi0zer**2
        m2grid(0,2) = two*(ai0zer*ai1zer+bi0zer*bi1zer)
        m2grid(0,3) = two*(ai1zer**2+bi1zer**2)

! build the rest of the two grids
        DO i = 1, n_parts_aux
          m2grid(i,:) = phi(i,:,1)*m2grid(i-1,1) + phi(i,:,2)*m2grid(i-1,2) + &
            phi(i,:,3)*m2grid(i-1,3)
        END DO
      END IF
    END SUBROUTINE taylor_modulus
!***
!************************************************************************
!***
    SUBROUTINE taylor_phase(nn,theta_inter)
      INTEGER, INTENT (IN) :: nn
      REAL (prd), INTENT (OUT) :: theta_inter
!*
!  PRIVATE subroutine to integrate for the phase functions M^2(x)
!   and the corresponding phase function theta(x).
!   we check to see if the main grid has been computed and 
!   integrate from the closest partition point to the one desired.
!*
      INTEGER iplace, j, i, k
      REAL (prd) h, xright, xm, xloc, darg
      REAL (prd), DIMENSION (0:n_taylor_aux) :: m2taylor, theta

      h = -aux_min/n_parts_aux
      IF (nn==1) THEN

! find the partition point closest to the desire value 
        DO i = n_parts_aux - 1, 0, -1
          IF (x_global<=h*i) THEN
            xright = h*i
            iplace = i
            EXIT
          END IF
        END DO
        h = x_global - xright
      ELSE

! compute and populate a vector of function values for 
!   theta(x) on the same grid points.  
!   build the Taylor series expansion for M^2(x) 
!   at each grid point, then calculate theta(x) from the 
!   first order ODE
        ALLOCATE (theta_grid(0:n_parts_aux))

! initial conditions for theta(0)
        theta_grid(0) = pi/six
      END IF

! compute the Taylor series for M^2 in each partition
      DO i = 1, nn
        IF (nn==1) THEN
          xm = xright
          xloc = x_global
        ELSE
          iplace = i - 1
          xm = h*(i-1)
          xloc = h*i
        END IF
        m2taylor(0) = m2grid(iplace,1)
        m2taylor(1) = m2grid(iplace,2)
        m2taylor(2) = m2grid(iplace,3)*half
        DO j = 3, n_taylor_aux
          m2taylor(j) = (four*(j-2)*xm*m2taylor(j-2)+m2taylor(j-3)*(4*j-10))/ &
            real(j*(j-1)*(j-2),prd)
        END DO

! now build the Taylor series for the phase function theta(x)
        theta(0) = theta_grid(iplace)
        theta(1) = -one/pi/m2taylor(0)
        theta(2) = -theta(1)*m2taylor(1)*half/m2taylor(0)
        DO k = 3, n_taylor_aux
          darg = zero
          DO j = 0, k - 2
            darg = darg + theta(j+1)*(j+1)*m2taylor(k-j-1)
          END DO
          theta(k) = -darg/m2taylor(0)/real(k,prd)
        END DO

! now sum the Taylor series to obtain the value of theta.
        darg = theta(n_taylor_aux)
        DO j = n_taylor_aux - 1, 1, -1
          darg = h*darg + theta(j)
        END DO
        theta_inter = h*darg + theta(0)
        IF (nn==n_parts_aux) theta_grid(i) = theta_inter
      END DO

! reset the logical and exit
      IF (nn==n_parts_aux) big_integrate_aux = .TRUE.
    END SUBROUTINE taylor_phase
!***
!************************************************************************
!***

!  end file airy_aux


! begin file airy_zeros
!    version 1.0 
!    edited 07/24/2002

!***
!************************************************************************
!***
    SUBROUTINE ai_zeror(n,ai_zero,ierr,ai_assoc,dai_zero,dai_assoc)
      INTEGER, INTENT (IN) :: n
      REAL (prd), INTENT (OUT) :: ai_zero
      INTEGER, INTENT (OUT) :: ierr
      REAL (prd), INTENT (OUT), OPTIONAL :: ai_assoc, dai_zero, dai_assoc
!*
!  PUBLIC subroutine which is the driver routine that computes the 
!    zeros of Ai(x) and optionally of Ai'(x), and the associated 
!    values Ai'(a_n) and Ai(a_n') for scalar arguments.  The first 25 
!    zeros are stored in an array and the rest are computed by summing 
!    approriate asymptotic expansions.  The (PRIVATE) subroutines called are: 
!     parameters_airy = determines the machine specific parameters 
!     zero_parameters_airy = populates the vectors containing the  
!	stored zeros and the coefficients of the asymptotic expansions 
!     airy_air = computes the associated function value for stored zeros 
!     ae_zero_r = sums the appropriate asymptotic expansion for the  
!	zeros of the Airy functions and their associated function values
!*
      INTEGER itemp
      REAL (prd) xai, xdai, lam

      IF ( .NOT. is_zero_init_airy) CALL zero_parameter_airy
      iflag = 0
      IF (n<=0) iflag = -3
      itemp = floor(0.25*(huge(itemp)-1))
      IF (n>=itemp) iflag = -4 ! zero requested cannot be computed
      IF (iflag<0) THEN
        ai_zero = zero
        IF (present(ai_assoc)) ai_assoc = zero
        IF (present(dai_zero)) dai_zero = zero
        IF (present(dai_assoc)) dai_assoc = zero
        ierr = iflag
        RETURN
      END IF
      IF (n<=n_zeros) THEN
        ai_zero = aizr(n)
        IF (present(ai_assoc)) THEN
          CALL airy_air(aizr(n),xai,xdai,ierr)
          ai_assoc = xdai
        END IF
        IF (present(dai_zero)) dai_zero = daizr(n)
        IF (present(dai_assoc)) THEN
          CALL airy_air(daizr(n),xai,xdai,ierr)
          dai_assoc = xai
        END IF
      ELSE
        lam = three_pi_ate*real(4*n-1,prd)
        CALL ae_zero_r(xai,lam,1)
        ai_zero = -xai
        CALL ae_zero_r(xai,lam,3)
        IF (present(ai_assoc)) THEN
          ai_assoc = xai
          IF (mod(n-1,2)==1) ai_assoc = -ai_assoc
        END IF
        IF (present(dai_zero) .OR. present(dai_assoc)) THEN
          lam = three_pi_ate*(4*n-3)
          CALL ae_zero_r(xai,lam,2)
          IF (present(dai_zero)) dai_zero = -xai
          CALL ae_zero_r(xdai,lam,4)
          IF (present(dai_assoc)) THEN
            dai_assoc = xdai
            IF (mod(n-1,2)==1) dai_assoc = -dai_assoc
          END IF
        END IF
      END IF
    END SUBROUTINE ai_zeror
!***
!************************************************************************
!***
    SUBROUTINE bi_zeror(n,bi_zero,ierr,bi_assoc,dbi_zero,dbi_assoc)
      INTEGER, INTENT (IN) :: n
      REAL (prd), INTENT (OUT) :: bi_zero
      INTEGER, INTENT (OUT) :: ierr
      REAL (prd), INTENT (OUT), OPTIONAL :: bi_assoc, dbi_zero, dbi_assoc
!*
!  PUBLIC subroutine which is the driver routine that computes the   
!    zeros of Bi(x) and optionally of Bi'(x), and the associated 
!    values Bi'(b_n) and Bi(b_n') for scalar arguments.  The first 25  
!    zeros are stored in an array and the rest are computed by summing 
!    approriate asymptotic expansions.  The (PRIVATE) subroutines called are: 
!     parameters_airy = determines the machine specific parameters 
!     zero_parameters_airy = populates the vectors containing the  
!	stored zeros and the coefficients of the asymptotic expansions 
!     airy_bir = computes the associated function value for stored zeros 
!     ae_zero_r = sums the appropriate asymptotic expansion for the  
!	zeros of the Airy functions and their associated function values 
!*
      INTEGER itemp
      REAL (prd) xbi, xdbi, lam

      IF ( .NOT. is_zero_init_airy) CALL zero_parameter_airy
      iflag = 0
      IF (n<=0) iflag = -3
      itemp = floor(0.25*(huge(itemp)-1))
      IF (n>=itemp) iflag = -4 ! zero requested cannot be computed
      IF (iflag<0) THEN
        bi_zero = zero
        IF (present(bi_assoc)) bi_assoc = zero
        IF (present(dbi_zero)) dbi_zero = zero
        IF (present(dbi_assoc)) dbi_assoc = zero
        ierr = iflag
        RETURN
      END IF
      IF (n<=n_zeros) THEN
        bi_zero = bizr(n)
        IF (present(bi_assoc)) THEN
          CALL airy_bir(bi_zero,xbi,xdbi,ierr)
          bi_assoc = xdbi
        END IF
        IF (present(dbi_zero)) dbi_zero = dbizr(n)
        IF (present(dbi_assoc)) THEN
          CALL airy_bir(dbizr(n),xbi,xdbi,ierr)
          dbi_assoc = xbi
        END IF
      ELSE
        lam = three_pi_ate*real(4*n-3,prd)
        CALL ae_zero_r(xbi,lam,1)
        bi_zero = -xbi
        CALL ae_zero_r(xbi,lam,3)
        IF (present(bi_assoc)) THEN
          bi_assoc = xbi
          IF (mod(n-1,2)==1) bi_assoc = -bi_assoc
        END IF
        IF (present(dbi_zero) .OR. present(dbi_assoc)) THEN
          lam = three_pi_ate*(4*n-1)
          CALL ae_zero_r(xbi,lam,2)
          IF (present(dbi_zero)) dbi_zero = -xbi
          CALL ae_zero_r(xdbi,lam,4)
          IF (present(dbi_assoc)) THEN
            dbi_assoc = xdbi
            IF (mod(n,2)==1) dbi_assoc = -dbi_assoc
          END IF
        END IF
      END IF
    END SUBROUTINE bi_zeror
!***
!************************************************************************
!***
    SUBROUTINE ae_zero_r(zr,lam,in)
      REAL (prd), INTENT (OUT) :: zr
      REAL (prd), INTENT (IN) :: lam
      INTEGER, INTENT (IN) :: in
!*
!  PRIVATE subroutine to sum the asymptotic expansions for the zeros 
!   of the Airy functions and their associated functions
!*
      REAL (prd) zrsum, lamr
      INTEGER i

      lamr = one/lam**2
      SELECT CASE (in)
      CASE (1) ! sum the T(x) expansion
        zrsum = tcoeff(n_asymp_zero)
        DO i = n_asymp_zero - 1, 0, -1
          zrsum = tcoeff(i) + zrsum*lamr
        END DO
        zr = zrsum*lam**two_third
      CASE (2) ! sum the U(x) expansion
        zrsum = ucoeff(n_asymp_zero)
        DO i = n_asymp_zero - 1, 0, -1
          zrsum = ucoeff(i) + zrsum*lamr
        END DO
        zr = zrsum*lam**two_third
      CASE (3) ! sum the V(x) expansion
        zrsum = vcoeff(n_asymp_asso)
        DO i = n_asymp_asso - 1, 0, -1
          zrsum = vcoeff(i) + zrsum*lamr
        END DO
        zr = zrsum*lam**(half/three)/sqrpi
      CASE (4) ! sum the W(x) expansion
        zrsum = wcoeff(n_asymp_asso)
        DO i = n_asymp_asso - 1, 0, -1
          zrsum = wcoeff(i) + zrsum*lamr
        END DO
        zr = zrsum/lam**(half/three)/sqrpi
      END SELECT
    END SUBROUTINE ae_zero_r
!***
!************************************************************************
!***
    SUBROUTINE ai_zerorv(n,ai_zero,ierr,ai_assoc,dai_zero,dai_assoc)
      INTEGER, INTENT (IN) :: n
      REAL (prd), INTENT (OUT), DIMENSION (:) :: ai_zero
      INTEGER, INTENT (OUT) :: ierr
      REAL (prd), INTENT (OUT), DIMENSION (:), OPTIONAL :: ai_assoc, dai_zero, &
        dai_assoc
!*
!*
!  PUBLIC subroutine which is the driver routine that computes the 
!    zeros of Ai(x) and optionally of Ai'(x), and the associated 
!    values Ai'(a_n) and Ai(a_n') for vector arguments.  The first 25 
!    zeros are stored in an array and the rest are computed by summing 
!    approriate asymptotic expansions.  The (PRIVATE) subroutines called are: 
!     parameters_airy = determines the machine specific parameters 
!     zero_parameters_airy = populates the vectors containing the  
!	stored zeros and the coefficients of the asymptotic expansions 
!     airy_air = computes the associated function value for stored zeros 
!     ae_zero_rv = sums the appropriate asymptotic expansion for the  
!	zeros of the Airy functions and their associated function values
!*
      INTEGER itemp, k, i, iflag_zero
      REAL (prd) xai, xdai
      REAL (prd), DIMENSION (:), ALLOCATABLE :: lam


      IF ( .NOT. is_zero_init_airy) CALL zero_parameter_airy
      iflag_zero = 0
      IF (n<=0) iflag_zero = -3
      itemp = floor(0.25*(huge(itemp)-1))
      IF (n>=itemp) iflag_zero = -4 ! zero requested cannot be computed
      k = size(ai_zero)

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
        ai_zero(:) = zero
        IF (present(ai_assoc)) ai_assoc(:) = zero
        IF (present(dai_zero)) dai_zero(:) = zero
        IF (present(dai_assoc)) dai_assoc(:) = zero
        ierr = iflag_zero
        RETURN
      END IF
      IF (n<=n_zeros) THEN
        itemp = min(n+k,n_zeros)
        ai_zero(1:itemp-n+1) = aizr(n:itemp)
        IF (present(ai_assoc)) THEN
          DO i = n, itemp
            CALL airy_air(aizr(i),xai,xdai,ierr)
            ai_assoc(i-n+1) = xdai
          END DO
        END IF
        IF (present(dai_zero)) dai_zero(1:itemp-n+1) = daizr(n:itemp)
        IF (present(dai_assoc)) THEN
          DO i = n, itemp
            CALL airy_air(daizr(i),xai,xdai,ierr)
            dai_assoc(i-n+1) = xai
          END DO
        END IF
      END IF
      IF (n+k>n_zeros) THEN
        itemp = max(n,n_zeros+1)
        ALLOCATE (lam(itemp:n+k-1))
        DO i = itemp, n + k - 1
          lam(i) = three_pi_ate*real(4*i-1,prd)
        END DO
        CALL ae_zero_rv(ai_zero(itemp-n+1:k),lam,1)
        ai_zero(itemp-n+1:k) = -ai_zero(itemp-n+1:k)
        IF (present(ai_assoc)) THEN
          CALL ae_zero_rv(ai_assoc(itemp-n+1:k),lam,3)
          DO i = itemp, n + k - 1
            IF (mod(i-1,2)==1) ai_assoc(i-n+1) = -ai_assoc(i-n+1)
          END DO
        END IF
        DEALLOCATE (lam)
        IF (present(dai_zero) .OR. present(dai_assoc)) THEN
          ALLOCATE (lam(itemp:n+k-1))
          DO i = itemp, n + k - 1
            lam(i) = three_pi_ate*real(4*i-3,prd)
          END DO
          IF (present(dai_zero)) THEN
            CALL ae_zero_rv(dai_zero(itemp-n+1:k),lam,2)
            dai_zero(itemp-n+1:k) = -dai_zero(itemp-n+1:k)
          END IF
          IF (present(dai_assoc)) THEN
            CALL ae_zero_rv(dai_assoc(itemp-n+1:k),lam,4)
            DO i = itemp, n + k - 1
              IF (mod(i-1,2)==1) dai_assoc(i-n+1) = -dai_assoc(i-n+1)
            END DO
          END IF
          DEALLOCATE (lam)
        END IF
      END IF
      ierr = iflag_zero
    END SUBROUTINE ai_zerorv
!***
!************************************************************************
!***
    SUBROUTINE bi_zerorv(n,bi_zero,ierr,bi_assoc,dbi_zero,dbi_assoc)
      INTEGER, INTENT (IN) :: n
      REAL (prd), INTENT (OUT), DIMENSION (:) :: bi_zero
      INTEGER, INTENT (OUT) :: ierr
      REAL (prd), INTENT (OUT), DIMENSION (:), OPTIONAL :: bi_assoc, dbi_zero, &
        dbi_assoc
!*
!*
!  PUBLIC subroutine which is the driver routine that computes the   
!    zeros of Bi(x) and optionally of Bi'(x), and the associated 
!    values Bi'(b_n) and Bi(b_n') for vector arguments.  The first 25  
!    zeros are stored in an array and the rest are computed by summing 
!    approriate asymptotic expansions.  The (PRIVATE) subroutines called are: 
!     parameters_airy = determines the machine specific parameters 
!     zero_parameters_airy = populates the vectors containing the  
!	stored zeros and the coefficients of the asymptotic expansions 
!     airy_bir = computes the associated function value for stored zeros 
!     ae_zero_rv = sums the appropriate asymptotic expansion for the  
!	zeros of the Airy functions and their associated function values 
!*
      INTEGER itemp, k, i, iflag_zero
      REAL (prd) xbi, xdbi
      REAL (prd), DIMENSION (:), ALLOCATABLE :: lam

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
        bi_zero(:) = zero
        IF (present(bi_assoc)) bi_assoc(:) = zero
        IF (present(dbi_zero)) dbi_zero(:) = zero
        IF (present(dbi_assoc)) dbi_assoc(:) = zero
        ierr = iflag_zero
        RETURN
      END IF
      IF (n<=n_zeros) THEN
        itemp = min(n+k,n_zeros)
        bi_zero(1:itemp-n+1) = bizr(n:itemp)
        IF (present(bi_assoc)) THEN
          DO i = n, itemp
            CALL airy_bir(bizr(i),xbi,xdbi,ierr)
            bi_assoc(i-n+1) = xdbi
          END DO
        END IF
        IF (present(dbi_zero)) dbi_zero(1:itemp-n+1) = dbizr(n:itemp)
        IF (present(dbi_assoc)) THEN
          DO i = n, itemp
            CALL airy_bir(dbizr(i),xbi,xdbi,ierr)
            dbi_assoc(i-n+1) = xbi
          END DO
        END IF
      END IF
      IF (n+k>n_zeros) THEN
        itemp = max(n,n_zeros+1)
        ALLOCATE (lam(itemp:n+k-1))
        DO i = itemp, n + k - 1
          lam(i) = three_pi_ate*real(4*i-3,prd)
        END DO
        CALL ae_zero_rv(bi_zero(itemp-n+1:k),lam,1)
        bi_zero(itemp-n+1:k) = -bi_zero(itemp-n+1:k)
        IF (present(bi_assoc)) THEN
          CALL ae_zero_rv(bi_assoc(itemp-n+1:k),lam,3)
          DO i = itemp, n + k - 1
            IF (mod(i-1,2)==1) bi_assoc(i-n+1) = -bi_assoc(i-n+1)
          END DO
        END IF
        DEALLOCATE (lam)
        IF (present(dbi_zero) .OR. present(dbi_assoc)) THEN
          ALLOCATE (lam(itemp:n+k-1))
          DO i = itemp, n + k - 1
            lam(i) = three_pi_ate*real(4*i-1,prd)
          END DO
          IF (present(dbi_zero)) THEN
            CALL ae_zero_rv(dbi_zero(itemp-n+1:k),lam,2)
            dbi_zero(itemp-n+1:k) = -dbi_zero(itemp-n+1:k)
          END IF
          IF (present(dbi_assoc)) THEN
            CALL ae_zero_rv(dbi_assoc(itemp-n+1:k),lam,4)
            DO i = itemp, n + k - 1
              IF (mod(i,2)==1) dbi_assoc(i-n+1) = -dbi_assoc(i-n+1)
            END DO
          END IF
          DEALLOCATE (lam)
        END IF
      END IF
      ierr = iflag_zero
    END SUBROUTINE bi_zerorv
!***
!************************************************************************
!***
    SUBROUTINE ae_zero_rv(zr,lam,in)
      REAL (prd), INTENT (OUT), DIMENSION (1:) :: zr
      REAL (prd), INTENT (IN), DIMENSION (1:) :: lam
      INTEGER, INTENT (IN) :: in
      REAL (prd), DIMENSION (size(zr)) :: zrsum, lamr
!*
!  PRIVATE subroutine to sum the asymptotic expansions for the zeros 
!   of the Airy functions and their associated functions for the 
!   vector case
!*
      INTEGER k, i, j

      k = size(zr)
      DO i = 1, k
        lamr(i) = one/lam(i)**2
      END DO
      IF (in==1) THEN ! sum the T(x) expansion
        DO j = 1, k
          zrsum(j) = tcoeff(n_asymp_zero)
          DO i = n_asymp_zero - 1, 0, -1
            zrsum(j) = tcoeff(i) + zrsum(j)*lamr(j)
          END DO
        END DO
        zr(:) = zrsum(:)*lam(:)**two_third
      ELSE IF (in==2) THEN ! sum the U(x) expansion
        DO j = 1, k
          zrsum(j) = ucoeff(n_asymp_zero)
          DO i = n_asymp_zero, 0, -1
            zrsum(j) = ucoeff(i) + zrsum(j)*lamr(j)
          END DO
        END DO
        zr(:) = zrsum(:)*lam(:)**two_third
      ELSE IF (in==3) THEN ! sum the V(x) expansion
        DO j = 1, k
          zrsum(j) = vcoeff(n_asymp_asso)
          DO i = n_asymp_asso - 1, 0, -1
            zrsum(j) = vcoeff(i) + zrsum(j)*lamr(j)
          END DO
        END DO
        zr(:) = zrsum(:)*lam(:)**(half/three)/sqrpi
      ELSE ! sum the W(x) expansion
        DO j = 1, k
          zrsum(j) = wcoeff(n_asymp_asso)
          DO i = n_asymp_asso - 1, 0, -1
            zrsum(j) = wcoeff(i) + zrsum(j)*lamr(j)
          END DO
        END DO
        zr(:) = zrsum(:)/lam(:)**(half/three)/sqrpi
      END IF
    END SUBROUTINE ae_zero_rv
!***
!************************************************************************
!***

!  end file airy_zeros
