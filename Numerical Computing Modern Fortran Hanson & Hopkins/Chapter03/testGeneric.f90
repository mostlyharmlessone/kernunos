    PROGRAM testGeneric
! Test program to illustrate the use of the generic definitions
! for airy_ai and airy_bi provided in the module airy
!
! We call airy_ai for all four valid argument types and
! airy_bi for just the two real precisions defined by the
! CALGO algorithm 838 code
      USE set_precision, ONLY : dkind, skind
      USE airy, ONLY: airy_ai, airy_bi
      IMPLICIT NONE

      REAL (skind), PARAMETER :: sone = 1.0E0_skind, &
         sfour = 4.0E0_skind, spi = sfour*ATAN(sone)
      REAL (dkind), PARAMETER :: done = 1.0E0_dkind, &
         dfour = 4.0E0_dkind, dpi = dfour*ATAN(done)
      REAL (skind) :: sy(4)
      REAL (dkind) :: dy(4), maxdrel
      COMPLEX (skind) :: cz, cy(2)
      COMPLEX (dkind) :: zz, zy(2)
      INTEGER :: ierr
! Use a single parameter value (pi in this case); compute the
! four values in both precisions and then compute the maximum
! relative error  between the returned values. This error should
! be close to the single precision machine epsilon.
      CALL airy_ai(spi, sy(1),sy(2),ierr)
      CALL airy_bi(spi, sy(3),sy(4),ierr)
      WRITE (*,'(A,  e14.6)') 'Single precision argument: ', spi
      WRITE (*,'(A, 4e14.6)') 'Ai and Bi values         : ', sy(1:4)
      CALL airy_ai(dpi, dy(1),dy(2),ierr)
      CALL airy_bi(dpi, dy(3),dy(4),ierr)
      WRITE (*,'(A,  e14.6)') 'Double precision argument: ', dpi
      WRITE (*,'(A, 4e14.6)') 'Ai and Bi values         : ', dy(1:4)
      maxdrel = MAXVAL(ABS((dy-sy)/dy))
      WRITE (*,'(A, e14.6)') 'Maximum relative difference; ', &
                             maxdrel
      WRITE(*, '()')

! Use a single parameter value ((pi,one) in this case); compute the
! two values in both precisions and then compute the maximum
! relative error in the real and imaginary parts of the returned values.
! This error should be close to the single precision machine epsilon.
      cz = CMPLX(spi,sone,KIND=skind)
      CALL airy_ai(cz, cy(1),cy(2),ierr)
      WRITE (*,'(A, 2e14.6)') 'Complex single precision argument: ', cz
      WRITE (*,'(A, 4e14.6)') 'Ai values (complex)              : ', cy(1:2)
      zz = CMPLX(dpi,done,KIND=dkind)
      CALL airy_ai(zz, zy(1),zy(2),ierr)
      WRITE (*,'(A, 2e14.6)') 'Complex double precision argument: ', zz
      WRITE (*,'(A, 4e14.6)') 'Ai values (complex)              : ', zy(1:2)
      sy(1:2) = REAL(cy,kind=skind)
      sy(3:4) = AIMAG(cy)
      dy(1:2) = REAL(zy,kind=dkind)
      dy(3:4) = AIMAG(zy)
      maxdrel = MAXVAL(ABS((dy-sy)/dy))
      WRITE (*,'(A, e14.6)') 'Maximum relative difference; ', &
                             maxdrel

    END PROGRAM testGeneric
