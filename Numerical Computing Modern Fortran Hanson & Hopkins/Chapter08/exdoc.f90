MODULE NORMAL_PDF
! This demonstrates the use of QAG2003 by integrating
! the function s*exp(-(x-mu)**2/(2*sigma**2)), 
! s=1/sqrt(2*pi*sigma**2).  The integration limits are
! from mu-1 to mu+1.  
USE SET_QUADPACK_DATA, ONLY : QUADPACKBASE, WP, FS,&
    QAG2003, ZERO, ONE, TWO

! Extend the base class so that mu and sigma are passed
! to the evaluation function, FMODULE(x(:), EXTEND).
TYPE, EXTENDS(QUADPACKBASE) :: EXTYPE
   REAL(WP) :: MU=ZERO
   REAL(WP) :: SIGMA=ONE
END TYPE EXTYPE 
  
CONTAINS
! Define the integrand function and use the
! passed values of mu, sigma:
FUNCTION FMODULE(X,EX)RESULT(Y)
    IMPLICIT NONE
    REAL(WP), INTENT(IN) :: X(:)
    CLASS(QUADPACKBASE), INTENT(INOUT) :: EX
    REAL(WP) :: Y(SIZE(X))
! This is the integrand constant scale factor,
! s=1/sqrt(2*pi).        
    REAL(WP) :: S=ONE/sqrt(8._WP * ATAN(ONE))

       SELECT TYPE (EX)
       TYPE IS (EXTYPE)
           ASSOCIATE &
           (MU    => EX % MU,&
            SIGMA => EX % SIGMA)
            Y=(S/SIGMA)*exp(-(X-MU)**2/(TWO*SIGMA**2))
           END ASSOCIATE
       END SELECT
END FUNCTION  
END MODULE

PROGRAM DOCUMENT_EXAMPLE
USE NORMAL_PDF, ONLY : QUADPACKBASE, WP, FS,&
    QAG2003, ONE, TWO, EXTYPE, FMODULE
! Define a variable of the extended type:
TYPE(EXTYPE) T
! These are the values of mu, sigma used:
REAL(WP) :: MU1=ONE, SIGMA1=TWO
REAL(WP) Approx1, Approx2, Approx3

! Since FEXTERNAL is not a module routine,
! nor a contained routine, 
! it is necessary to specify its abstract interface.
PROCEDURE(FS) :: FEXTERNAL
! This improves readability, by simplifying the
! symbols associated with derived types.
ASSOCIATE &
  (MU   => T % MU,&
  SIGMA => T % SIGMA)
! Evaluate the integral with three methods of
! defining the routine that gives values of
! the integrand function.
  
! Place values of mu and sigma in the extended type
! components:
  MU    = MU1
  SIGMA = SIGMA1
! Approximate the integral using a module that
! contains the function FMODULE.  
  CALL QAG2003(FMODULE, T, MU-ONE, MU+ONE, Approx1)
! A second alternate packaging is to contain the
! evaluation function FCONTAINED in this
! program unit.  
  CALL QAG2003(FCONTAINED, T, MU-ONE, MU+ONE, Approx2)

! A third packaging method is to use 
! an external function:
  CALL QAG2003(FEXTERNAL, T, MU-ONE, MU+ONE, Approx3)
  IF (T % IER == 0 .and. &
      (Approx1==Approx2) .and. (Approx1==Approx3) )&
      WRITE(*,'(A,F5.2,A,F5.2/A,F7.3)')&
      " Normal cumulative for mu = ",MU,&
      " sigma = ",SIGMA,&
      "  within limits [mu-1,m+1] is = ",Approx1
  END ASSOCIATE

CONTAINS
! Define the integrand function and use the
! passed values of mu, sigma:
FUNCTION FCONTAINED(X,EX)RESULT(Y)
    IMPLICIT NONE
    REAL(WP), INTENT(IN) :: X(:)
    CLASS(QUADPACKBASE), INTENT(INOUT) :: EX
    REAL(WP) :: Y(SIZE(X))
! This is the integrand constant scale factor,
! s=1/sqrt(2*pi).     
    REAL(WP) :: S=ONE/sqrt(8._WP * ATAN(ONE))

       SELECT TYPE (EX)
       TYPE IS (EXTYPE)
           ASSOCIATE &
           (MU    => EX % MU,&
            SIGMA => EX % SIGMA)
            Y=(S/SIGMA)*exp(-(X-MU)**2/(TWO*SIGMA**2))
           END ASSOCIATE
       END SELECT
END FUNCTION      
END PROGRAM DOCUMENT_EXAMPLE  

! Define the integrand function externally and use the
! passed values of mu, sigma:
FUNCTION FEXTERNAL(X,EX)RESULT(Y)
! Note that it is necessary to use-associate the
! defined extended base class and other symbols.    
    USE NORMAL_PDF, ONLY : QUADPACKBASE, WP, FS,&
        QAG2003, ONE, TWO, EXTYPE
        IMPLICIT NONE
        REAL(WP), INTENT(IN) :: X(:)
        CLASS(QUADPACKBASE), INTENT(INOUT) :: EX
        REAL(WP) :: Y(SIZE(X))
! This is the integrand constant scale factor,
! s=1/sqrt(2*pi).     
        REAL(WP) :: S=ONE/sqrt(8._WP * ATAN(ONE))

           SELECT TYPE (EX)
           TYPE IS (EXTYPE)
               ASSOCIATE &
               (MU    => EX % MU,&
                SIGMA => EX % SIGMA)
                Y=(S/SIGMA)*exp(-(X-MU)**2/(TWO*SIGMA**2))
               END ASSOCIATE
           END SELECT
END FUNCTION

! Expected results:    
! Normal cumulative for mu =  1.00 sigma =  2.00
!  within limits [mu-1,m+1] is =   0.383
