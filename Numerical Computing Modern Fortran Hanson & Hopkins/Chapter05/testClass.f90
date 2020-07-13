       PROGRAM testClass
       USE brentClass, ONLY : zeroin
       USE brentExtraArgs, ONLY: brentArgsExtra, extraF
       USE set_precision, ONLY : wp
       TYPE(brentArgsExtra) :: extraBrent
       REAL(wp) :: z
! Define the end points of the interval and use
! the default tolerance
       extraBrent%ax = 2.0E0_wp
       extraBrent%bx = 3.0E0_wp
       extraBrent%param = 5.0E0_wp
       z = zeroin(extraBrent, extraF)
       WRITE(*, '('' Root = '', f15.10)')z
       END PROGRAM testClass
