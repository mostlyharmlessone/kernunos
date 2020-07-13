    PROGRAM extraTests
      USE extraTestsMod, ONLY : my_quad_data, my_2dquad, my_laplacev, &
                                my_f, my_laplaces, my_inf_integrand, &
                                my_2df, upper, rms, curved
      USE quadpack2003, ONLY : qag2003, quadpackbase
      USE set_precision, ONLY : wp
      REAL (wp), PARAMETER :: one = 1.0E0_wp, four = 4.0E0_wp, &
                              zero = 0.0E0_wp, two = 2.0E0_wp, &
                              three = 3.0E0_wp
      REAL (wp) :: pi = four*ATAN(one), approx, twod, up, trans(3)
      TYPE (my_quad_data) :: mine
      TYPE (my_2dquad) :: my2d
      TYPE (quadpackbase) :: nothing

      ASSOCIATE(key => mine%key, ier => mine%ier, &
                 nf => mine%nfunction_evaluations, &
                 nv => mine%nvector_evaluations, &
                 omega => mine%omega)
! Possibly switch quadrature-sampling density.
      key = 6 ! This is the default.

! Define the data for this integral.
! This is part of the extended data type.
      omega = 100.D0

! Compute the approximate integral.
! Exact result is PI*J_0(100), J_0 = Bessel function.    
      CALL qag2003(my_f,mine,zero,pi,approx)
      WRITE (*,*) 'Integral (PI*J_0(100)) is  = ', approx
      WRITE (*,*) 'Result flag from integration  = ', ier
      WRITE (*,*) 'Formula class, evaluations = ', key, ',', nf
      WRITE (*,*) 'Formula class, vector evaluations = ', key, ',', nv
      WRITE (*,*) ' '

      END ASSOCIATE

      ASSOCIATE(key => my2d%key, ier => my2d%ier, &
                nf  => my2d%nfunction_evaluations, &
                nv  => my2d%nvector_evaluations ,&
                intno => my2d%inner_integral_number)

! Flag to start outer integration.  This call
! is recursive so this value guides whether an inner
! integral or the function integrand is evaluated.               
      intno = 1
! Set the upper limit function of the inner integral.
      my2d%g => upper
! Set the inner evaluation function to the outer one. 
      my2d%f => my_2df
! Set absolute error for some inner integrals.
      my2d%epsabs = epsilon(zero)

! Compute double integral.  The evaluation function
! MY_2DF recursively calls QAG2003 for its inner values.
! The integrated function is 1/sqrt(1+x**2+y**2).
      CALL qag2003(my_2df,my2d,-one,one,approx)

      WRITE (*,*) 'Double Integral (4*log(2+sqrt(3)-2*PI/6) is  = ', approx
      twod = four*log(two+sqrt(three)) - two*pi/three
      WRITE (*,*) 'Relative error is  = ', (approx-twod)/twod


      WRITE (*,*) 'Result flag from integration = ', ier
      WRITE (*,*) 'Formula class, evaluations = ', key, ',', nf
      WRITE (*,*) ' '

! Set the upper limit function of the inner integral.
! This next time the region is curved.  The limits of the inner
! integral are [-g(x),g(x)=1-x**2], -1 <= x <= 1.
      my2d%g => curved
      nv = 0
      nf = 0
! Compute double integral over curved region.
      CALL qag2003(my_2df,my2d,-one,one,approx)

      WRITE (*,*) 'Double Integral over curved region is  = ', approx
      WRITE (*,*) 'Result flag from integration = ', ier
      WRITE (*,*) 'Formula class, evaluations = ', key, ',', nf
      WRITE (*,*) ' '
 END ASSOCIATE

! Compute infinite integral.  The evaluation function calls
! MY_INF_INTEGRAND.  The integral is for the integrand h(x)=
! log(X)/(1+100* X ** 2).
      up = 1000*log(huge(up))
      ASSOCIATE( nv  => nothing%nvector_evaluations,&
                 ier => nothing%IER )
      CALL qag2003(my_inf_integrand,nothing,zero,up,approx)
      WRITE (*,*) 'Infinite Integral is  = ', approx
      WRITE (*,*) 'Result flag from integration = ', ier
      WRITE (*,*) 'Vector evaluations = ', nv
      WRITE (*,*) ' '
      END ASSOCIATE

      mine%s = two
! Compute Laplace transform of the constatnt function 1 for real values
! of MINE%S but in scalar form.
      up = -log(epsilon(one))/mine%s
      CALL qag2003(my_laplaces,mine,zero,up,approx)
      WRITE (*,*) 'Laplace Transform  = ', approx
      WRITE (*,*) 'Result flag from integration = ', mine%ier
! Compute Laplace transform of 1,t,(1/2)*t^2 for real values of 
! tranform parameter, s.  Use vector integration.

! Use an RMS norm instead of default max norm.
      mine%enorm => rms

      CALL qag2003(my_laplacev,mine,zero,up,trans)
      WRITE (*,*) 'Laplace Transform  = ', trans
      WRITE (*,*) 'Result flag from integration = ', mine%ier
      WRITE (*,*) 'Matrix evaluations = ', mine%nvector_evaluations
    END PROGRAM extraTests
! Expected Results,
! Single Precision:
 !Integral (PI*J_0(100)) is  =   6.278740049149326E-002
 !Result flag from integration  =            0
 !Formula class, evaluations =            6 ,        1525
 !Formula class, vector evaluations =            6 ,          25
 !
 !Double Integral (4*log(2+sqrt(3)-2*PI/6) is  =    3.17343648530607
 !Relative error is  =   0.000000000000000E+000
 !Result flag from integration =            0
 !Formula class, evaluations =            6 ,          61
 !
 !Double Integral over curved region is  =    2.25797353331653
 !Result flag from integration =            0
 !Formula class, evaluations =            6 ,          61
 !
 !Infinite Integral is  =  -0.361689424516046
 !Result flag from integration =            0
 !Vector evaluations =          173
 !
 !Laplace Transform  =   0.500000000000000
 !Result flag from integration =            0
 !Laplace Transform  =   0.500000000000000       0.249999999999998
 ! 0.124999999999981
 !Result flag from integration =            0
 !Matrix evaluations =            2
