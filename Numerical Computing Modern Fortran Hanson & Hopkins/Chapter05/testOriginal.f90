       PROGRAM testOriginal
       EXTERNAL f, zeroin
       DOUBLE PRECISION f, zeroin

       DOUBLE PRECISION ax, bx, tol, z

       ax = 2.0D0
       bx = 3.0D0
       tol = SQRT(EPSILON(1.0D0)) 

       z = zeroin(ax, bx, f, tol)

       WRITE(*, '('' Root = '', f15.10)')z
       END

       DOUBLE PRECISION FUNCTION f(x)
       DOUBLE PRECISION x
       f = x*(x*x-2.0D0)-5.0D0
       END
