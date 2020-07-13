       PROGRAM testType
       USE brentTypes, ONLY : zeroin, brentargs
       USE set_precision, ONLY : wp
       TYPE(brentargs) :: typeBrent
       EXTERNAL f
       REAL(wp) :: f, z
! Define the end points of the interval and use
! the default tolerance
       typeBrent%ax = 2.0E0_wp
       typeBrent%bx = 3.0E0_wp
       z = zeroin(typeBrent, f)
       WRITE(*, '('' Root = '', f15.10)')z
       END PROGRAM testType

       FUNCTION f(x) RESULT(res)
       USE set_precision, ONLY : wp
       REAL(wp), INTENT(IN) :: x
       REAL(wp) :: res
       res = x*(x*x-2.0E0_wp)-5.0E0_wp
       END FUNCTION f
