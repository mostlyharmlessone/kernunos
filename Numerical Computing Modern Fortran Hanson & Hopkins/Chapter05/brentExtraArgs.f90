       MODULE brentExtraArgs

       USE set_precision, ONLY : wp
       USE brentclass, ONLY : brentArgs

       TYPE, EXTENDS(brentArgs) :: brentArgsExtra
         REAL(wp) :: param
       END TYPE brentArgsExtra
       
       CONTAINS

       FUNCTION extraF(x, extraArgs)
       REAL(wp), INTENT(IN) :: x
       REAL(wp) :: extraF
       CLASS(brentArgs) :: extraArgs
       SELECT TYPE (extraArgs)
         TYPE IS (brentArgsExtra)
           extraF = x*(x*x - 2.0d0) - extraArgs%param
       END SELECT 
       END FUNCTION extraF

       END MODULE brentExtraArgs
