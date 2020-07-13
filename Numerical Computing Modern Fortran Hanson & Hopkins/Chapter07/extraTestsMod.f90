! This is a sample usage of the routine QAG2003 that is written to
! allow an extended data type to be passed to the user-written
! code for the integrand function.  The following module is 
! typically provided or written by the user of the quadrature.

    MODULE extraTestsMod
      USE set_precision, ONLY : wp
      USE quadpack2003, ONLY : fs, quadpackbase, qag2003
      IMPLICIT NONE

      REAL(wp), PARAMETER :: zero = 0.0E0_wp, half = 0.5E0_wp, &
                             one = 1.0E0_wp, two = 2.0E0_wp

! This type extends the base class or derived type that
! is passed through the integrator to the evaluation
! function.  The form of the extended type can be any
! legal combination of intrinsic or user types.

! See Metcalf, et al. p. 271-273
      TYPE, EXTENDS (quadpackbase) :: my_quad_data
! The integrated function here is cos(OMEGA * sin(x)),
! and the interval of integration is [0, PI].
! Extending the base type allows passing OMEGA to the
! function evaluation code without using module data
! or COMMON.

        REAL (wp) :: omega
        REAL (wp) :: s
      END TYPE my_quad_data

      TYPE, EXTENDS (quadpackbase) :: my_2dquad
! The integrated function is 1/sqrt(1+x**2+y**2).
! Extending the base type allows passing information
! to support one level of recursion.
        PROCEDURE(limit), POINTER, NOPASS :: g
        PROCEDURE(fs), POINTER, NOPASS :: f
        INTEGER :: inner_integral_number = 1
        REAL (wp) x_i
      END TYPE my_2dquad

! This is the interface for the upper limit function.
      ABSTRACT INTERFACE 
        FUNCTION limit(x) RESULT (p)
          IMPORT WP
          REAL (wp) :: x, p
        END FUNCTION
      END INTERFACE

    CONTAINS
! This is a single integrand function evaluation code.
! It selects the type that contains the value OMEGA.
      FUNCTION my_f(x,extend) RESULT (y)
      IMPLICIT NONE
      REAL (wp), INTENT (IN) :: x(:)
      REAL (wp) :: y(size(x))

      CLASS (quadpackbase), INTENT (INOUT) :: extend

        SELECT TYPE (EXTEND)
! Other data types than the extended one are ignored:
        TYPE IS (my_quad_data)
          ASSOCIATE(nf => extend%nfunction_evaluations,&
                    nv => extend%nvector_evaluations,&
                    omega => extend%omega)
          y = cos(omega*sin(x))
          nf = nf + size(x)
          nv = nv + 1
          END ASSOCIATE
        END SELECT
      END FUNCTION
! This an alternate norm for use with vector integration.
! The norm here is RMS or root mean square.   
      FUNCTION rms(e,extend) RESULT (norm)
      IMPLICIT NONE
      REAL (wp), INTENT (IN) :: e(:)
      REAL (wp) :: norm

      CLASS (quadpackbase), INTENT (INOUT) :: extend

        SELECT TYPE (EXTEND)
! Other data types than the extended one are ignored:
        TYPE IS (my_quad_data)
          norm = sqrt(sum(e**2)/real(size(e),wp))
        END SELECT
      END FUNCTION


! This is a double integral evaluation code.
! It is a recursive function that at the first level
! again calls the quadrature routine.  At the lowest level
! it evaluates the integrand.
      RECURSIVE FUNCTION my_2df(x,extend) RESULT (y)
      REAL (wp), INTENT (IN) :: x(:)
      REAL (wp) :: y(size(x))

      REAL (wp) :: p
      INTEGER :: i
      CLASS (quadpackbase), INTENT (INOUT) :: extend
      PROCEDURE(fs), POINTER :: ff

      SELECT TYPE (EXTEND)
      TYPE IS (my_2dquad)
        ASSOCIATE(iin => extend%inner_integral_number,&
                  xi  => extend%x_i,&
                  nf  => extend%nfunction_evaluations,&
                  nv  => extend%nvector_evaluations)


        SELECT CASE (iin)

        CASE (0)
! This is the evaluation of the inner integrand, a
! vector at each call.  Note the fixed value XI
! in each evaluation using X(:) as a vector. 
          y = one/sqrt(one+xi**2+x**2)
          nf = nf + size(x)
          nv = nv + 1

        CASE (1)
          DO i = 1, size(x)
! Set current value of one variable and
! then force further calls to evaluate the integrand
! as a vector function.           
            xi = x(i)
! This flag makes the recursive call execute CASE(0)
! for each inner integral.
            iin = 0
            ff => extend%f
! This allows curved limits for the inner integral.
            p = extend%g(xi)
            CALL qag2003(ff,extend,-p,p,y(i))
! This resets the flag after an inner integration is finished.
            iin = 1
          END DO
        END SELECT
        END ASSOCIATE           
      END SELECT 
      END FUNCTION

! This is the integrand function for the infinite
! integral.   
      FUNCTION my_inf_integrand(x,extend) RESULT (y)
      REAL (wp), INTENT (IN) :: x(:)
      REAL (wp) :: y(size(x))

      CLASS (quadpackbase), INTENT (INOUT) :: extend
        ASSOCIATE(nf => extend%nfunction_evaluations,&
                  nv => extend%nvector_evaluations)
 
        y = log(x)/(one+100.E0_wp*x**2)
        nf = nf + size(x)
        nv = nv + 1
        END ASSOCIATE
      END FUNCTION
! This is the integrand function for the Laplace
! transform integral.   
      FUNCTION my_laplaces(x,extend) RESULT (y)
      REAL (wp), INTENT (IN) :: x(:)

      CLASS (quadpackbase), INTENT (INOUT) :: extend
      REAL (wp) :: y(size(x))
        SELECT TYPE (EXTEND)
        TYPE IS (my_quad_data)
! Kernel of transform for 1:       
          y = exp(-extend%s*x)
        END SELECT
      END FUNCTION
! This is the integrand function for the Laplace
! transform integral.   
      FUNCTION my_laplacev(x,extend) RESULT (y)
      REAL (wp), INTENT (IN) :: x(:)

      CLASS (quadpackbase), INTENT (INOUT) :: extend
      REAL (wp) :: y(size(x),extend%ncomponents)
        SELECT TYPE (EXTEND)
        TYPE IS (my_quad_data)
! Kernel of transform for 1, t, t^2:       
          y(:,1) = exp(-extend%s*x)
          y(:,2) = x*y(:,1)
          y(:,3) = half*x*y(:,2)
          extend%nvector_evaluations = extend%nvector_evaluations + 1
        END SELECT
      END FUNCTION

! This is the specific upper limit function used as
! an example.  Change this to have a different region
! of integration.
      FUNCTION upper(x) RESULT (p)
      REAL (wp) x, p

        p = one
      END FUNCTION

! This alternate limit function defines integration 
! over the curved region [-g(x),g(x)=1-x**2], -1 <= x <= 1.
      FUNCTION curved(x) RESULT (p)
      REAL (wp) x, p

        p = one - x**2
      END FUNCTION

    END MODULE extraTestsMod
