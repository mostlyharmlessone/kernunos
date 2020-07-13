       MODULE brentExtraArgsFinalize 
! This program unit is written by the user of ZEROIN.
       USE set_precision, ONLY : wp
       USE brentclass, ONLY : brentArgs

       TYPE, EXTENDS(brentArgs) :: brentArgsExtra
         REAL(wp) :: param
         INTEGER  :: unitNumber = 13
         INTEGER  :: evaluations = 0

         CONTAINS
         FINAL :: close_evaluation_file
       END TYPE brentArgsExtra
       
       CONTAINS

       FUNCTION extraF(x, extraArgs)
       REAL(wp), INTENT(IN) :: x
       REAL(wp) :: extraF
       CLASS(brentArgs) :: extraArgs
       SELECT TYPE (extraArgs)
         TYPE IS (brentArgsExtra)
           ASSOCIATE(param => extraArgs%param,&
                     unit => extraArgs%unitNumber,&
                     number => extraArgs%evaluations)
              extraF = x*(x*x-2.0d0)-param
              number = number+1
              WRITE(unit,'(2F15.10)') x, extraF
           END ASSOCIATE
       END SELECT 

       END FUNCTION extraF
! This routine is called when extraArgs is deleted by
! a deallocate().
       SUBROUTINE close_evaluation_file(extraArgs)
         TYPE(brentArgsExtra) extraArgs
         ASSOCIATE(unit => extraArgs%unitNumber)
           WRITE(unit,'(''Number of function evaluations:'',I5)') &
             extraArgs%evaluations
           CLOSE(unit)
         END ASSOCIATE
       END SUBROUTINE close_evaluation_file
       END MODULE brentExtraArgsFinalize

