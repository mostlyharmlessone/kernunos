PROGRAM testEx2
  USE set_precision, ONLY : wp
  USE quadpack2003, ONLY : qag2003
  USE testModEx2, ONLY : my_2df, my_2dquad_data, my_limit

  IMPLICIT NONE
  INTEGER, PARAMETER :: outer = 2
  REAL (wp), PARAMETER :: one = 1.0E0_wp
  TYPE (my_2dquad_data) :: my_data
  REAL (wp) :: intapprox, lowLimit=-one, upLimit=one

  ASSOCIATE(alpha => my_data%alpha,&
            beta => my_data%beta)
! The inner integrals specify absolute error requests 
  my_data%epsabs = EPSILON(one)
! Define the problem dependent parameter values
  alpha = one
  beta = one
! Point to the inner integral limit function   
  my_data%g => my_limit
! Point to the recursive integrand function   
  my_data%f => my_2df
! Initialize the integral type to outer -- recursion
! will take care of the rest
  my_data%inner_integral_number = outer

  CALL qag2003(my_2df, my_data, lowLimit, upLimit, intapprox)

  WRITE (*,'(''ALPHA, BETA, MY_PHI(ALPHA,BETA) = '', 3F9.6)') &
    alpha, beta, intapprox
  END ASSOCIATE

! Expected Result:
! ALPHA, BETA, MY_PHI(ALPHA,BETA) =  1.000000 1.000000 2.257974

END PROGRAM testEx2
