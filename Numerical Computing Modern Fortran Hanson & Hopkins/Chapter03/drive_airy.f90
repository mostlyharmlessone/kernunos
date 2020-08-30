    PROGRAM drive_airy
! Test the Wronskian relationship for a fundamental matrix solution
! of Airy's equation, w'' - xw = 0:  
! With the 2 by 2 matrix
!         | Ai(z)   Bi(z) | 
! Phi(z)= |               |
!         | Ai'(z)  Bi'(z)|,
! we check that, for all z, Wronskian = W(z)= det(Phi(z))= 1/pi. 

      USE set_precision, ONLY : dkind, skind
      USE Airy_Module
      IMPLICIT NONE

      REAL (skind), PARAMETER :: sone = 1.0E0_skind, &
         sfour = 4.0E0_skind, spi = sfour*ATAN(sone), &
         szero = 0.0E0_skind
      REAL (dkind), PARAMETER :: done = 1.0E0_dkind, &
         dfour = 4.0E0_dkind, dpi = dfour*ATAN(done), &
         dzero = 0.0E0_dkind
      REAL (skind) :: maxserr, maxcerr, sx, sy(4), sr, st
      REAL (dkind) :: maxderr, maxzerr, dx, dy(4), dr, dt
      COMPLEX (skind) :: cz, cy(4), cr
      COMPLEX (dkind) :: zz, zy(4), zr
      INTEGER :: i, nvals = 1000

      maxserr = szero
      maxcerr = szero
      maxderr = dzero
      maxzerr = dzero
! Verify the Wronskian value:
      DO i = 1, nvals
        CALL RANDOM_NUMBER(sx)
        CALL RANDOM_NUMBER(dx)

        CALL RANDOM_NUMBER(st)
        CALL RANDOM_NUMBER(dt)

        cz = CMPLX(sx,st,skind)
        zz = CMPLX(dx,dt,dkind)

! Use generic calls to function routines.
! No scaling is used.
        sy = abi(sx)
        dy = abi(dx)
        cy = abi(cz)
        zy = abi(zz)
! Check Wronskian value for each precision and data type:
        sr = sy(1)*sy(4) - sy(2)*sy(3) - sone/spi
        dr = dy(1)*dy(4) - dy(2)*dy(3) - done/dpi
        cr = cy(1)*cy(4) - cy(2)*cy(3) - sone/spi
        zr = zy(1)*zy(4) - zy(2)*zy(3) - done/dpi

        maxserr = MAX(maxserr,ABS(sr))
        maxderr = MAX(maxderr,ABS(dr))
        maxcerr = MAX(maxcerr,ABS(cr))
        maxzerr = MAX(maxzerr,ABS(zr))
      END DO
! Compute errors in units of the underlying precision:
      maxserr = MAX(maxserr,ABS(sr))/sone/spi/EPSILON(maxserr)
      maxderr = MAX(maxderr,ABS(dr))/done/dpi/EPSILON(maxderr)
      maxcerr = MAX(maxcerr,ABS(cr))/sone/spi/EPSILON(maxcerr)
      maxzerr = MAX(maxzerr,ABS(zr))/done/dpi/EPSILON(maxderr)

! Output results
      WRITE (*,'(A/A,I6/A/)') &
        'Airy ODE Wronskian - absolute units of relative error', &
        'Computed with NVALS random (0,1) values, using NVALS = ', nvals, &
        'Complex values are in the unit square, (0,1) x (0,1).'
      WRITE (*,'(A, T30,F6.2)') 'Single Precision: ', maxserr
      WRITE (*,'(A, T30,F6.2)') 'Double Precision: ', maxderr
      WRITE (*,'(A, T30,F6.2)') 'COMPLEX Single Precision: ', maxcerr
      WRITE (*,'(A, T30,F6.2)') 'COMPLEX Double Precision: ', maxzerr
    END PROGRAM drive_airy
