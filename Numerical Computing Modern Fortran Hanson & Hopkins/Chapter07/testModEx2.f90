    MODULE testModEx2
      USE set_precision, ONLY : wp
      USE quadpack2003, ONLY : quadpackbase, qag2003, fs

      IMPLICIT NONE
      REAL (wp), PARAMETER :: one = 1.0E0_wp, zero = 0.0E0_wp

! The Type Extension declaration: MY_2DQUAD_DATA goes here.
      TYPE, EXTENDS (quadpackbase) :: my_2dquad_data
        REAL (wp) :: alpha, beta
        PROCEDURE(limit), POINTER, NOPASS :: g
        PROCEDURE(fs), POINTER, NOPASS :: f
! Used for recursion:
        INTEGER :: inner_integral_number = -1
        REAL (wp) :: y_i
      END TYPE my_2dquad_data

! This is the interface for the upper limit function.
      ABSTRACT INTERFACE 
      FUNCTION limit(xval) RESULT (g)
        IMPORT wp
        REAL (wp), INTENT (IN) :: xval
        REAL (wp) :: g
      END FUNCTION limit
      END INTERFACE

    CONTAINS

! This limit function defines integration over the curved region 
!         [-g(x),g(x)=1-x**2], -1 <= x <= 1.
      FUNCTION my_limit(xval) RESULT (gval)
        REAL (wp), INTENT (IN) :: xval
        REAL (wp) gval

        gval = one - xval**2
      END FUNCTION my_limit

      RECURSIVE FUNCTION my_2df(xval,extend) RESULT (yval)
        REAL (wp), INTENT (IN) :: xval(:)
        REAL (wp) :: yval(SIZE(xval))
        REAL (wp), ALLOCATABLE :: p(:)
        REAL (wp), EXTERNAL, POINTER :: gval
        INTEGER :: i
        CLASS (quadpackbase), INTENT (INOUT) :: extend
        TYPE (my_2dquad_data), ALLOCATABLE :: extend_inner(:)
        PROCEDURE(fs), POINTER :: ff

        INTEGER, PARAMETER :: inner = 1, outer = 2

        SELECT TYPE(EXTEND)
        TYPE IS (my_2dquad_data)
        ASSOCIATE(alpha => extend%alpha,&
                  beta  => extend%beta,&
                  yy  => extend%y_i,&
                  iin  => extend%inner_integral_number)                                            

          SELECT CASE (iin)
          CASE (inner)
! This is the evaluation of the inner integrand, a
! vector at each call.  Note the fixed value y_i
! in each evaluation that also uses the array xval(:).
            yval = one/SQRT(one+alpha*xval**2+beta*yy**2)

          CASE (outer)
            gval => extend%g
            ALLOCATE (extend_inner(SIZE(xval)),p(SIZE(xval)))
! Next line insures that all integrands get values 
! of alpha, beta in their copy of the extended type.
            extend_inner(:)%alpha = alpha
            extend_inner(:)%beta = beta
! Set current value of one variable and then force further calls
! to evaluate the integrand as a vector function.

            extend_inner(:)%y_i = xval(:)
            DO i = 1, SIZE(xval)
              p(i) = gval(xval(i))
            END DO
            ff => extend%f
!OMP PARALLEL DO
            DO i = 1, SIZE(xval)
! This flag makes the recursive call execute CASE(0) for each inner
! integral.  This evaluates the integrand for the inner integral.
              extend_inner(i)%inner_integral_number = inner
! This evaluates the integrand for the outer integral.  It is
! necessary to assign the pointer extend%fptr to the integrand
! function itself.  Otherwise a missing routine will be reported at
! link time.
              CALL qag2003(ff,extend_inner(i),-p(i),p(i),yval(i))
            END DO
!OMP END PARALLEL DO
          END SELECT ! Case
        END ASSOCIATE
        END SELECT ! Type
      END FUNCTION my_2df
    END MODULE testModEx2

