    MODULE testModEx1fs
      USE set_precision, ONLY : wp
      USE quadpack2003, ONLY : quadpackbase, qag2003

      IMPLICIT NONE
      REAL (wp), PARAMETER :: one = 1.0E0_wp, zero = 0.0E0_wp

! The type extension declaration: my_quad_data goes here.
      TYPE, EXTENDS (quadpackbase) :: my_quad_data
! The integrated function is cos(omega * sin(x)),
! and the interval of integration is [0, PI].
        REAL (wp) :: omega
      END TYPE my_quad_data

    CONTAINS
! The integrand function MY_F goes here.
! This is a single integrand function evaluation code.
! It selects the type that contains the value OMEGA.
      FUNCTION my_f(xval,extend) RESULT (yval)
        IMPLICIT NONE
        REAL (wp), INTENT (IN) :: xval(:)

        CLASS (quadpackbase), INTENT (INOUT) :: extend
        REAL (wp) :: yval(SIZE(xval))

        SELECT TYPE (extend)
! Any other data types than the extended one are ignored:
          TYPE IS (my_quad_data)
          ASSOCIATE(omega => extend%omega)
            yval = COS(omega*SIN(xval))
! Can also maintain totals of evaluations here using
! components of EXTEND.
          END ASSOCIATE
        END SELECT

      END FUNCTION my_f

! Expected results
! Integral (PI*J_0(i)) is  =  3.141593, with i =    0
! Integral (PI*J_0(i)) is  =  2.403939, with i =    1
! Integral (PI*J_0(i)) is  =  0.703374, with i =    2
! Integral (PI*J_0(i)) is  = -0.816977, with i =    3
! Integral (PI*J_0(i)) is  = -1.247683, with i =    4
! Integral (PI*J_0(i)) is  = -0.557937, with i =    5
! ...
! ...
! Integral (PI*J_0(i)) is  =  0.257020, with i =   95
! Integral (PI*J_0(i)) is  =  0.145564, with i =   96
! Integral (PI*J_0(i)) is  = -0.097875, with i =   97
! Integral (PI*J_0(i)) is  = -0.249292, with i =   98
! Integral (PI*J_0(i)) is  = -0.171136, with i =   99
! Integral (PI*J_0(i)) is  =  0.062787, with i =  100

      END MODULE testModEx1fs
