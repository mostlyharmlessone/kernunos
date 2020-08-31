    SUBROUTINE exchange(i,j)
! Exchange the contents of the ith and jth elements
    INTEGER, INTENT(IN) :: i,j
    REAL(wp) :: t
    t = a(i)
    a(i) = a(j)
    a(j) = t
    END SUBROUTINE exchange
  
    LOGICAL FUNCTION compare(i,j)
! Compare the contents of the ith and jth elements
! This determines the final sorting order.
! Code for ascending order.
    INTEGER, INTENT(IN) :: i,j
    compare = a(i) < a(j)
    END FUNCTION compare
  
    SUBROUTINE compex(i,j)
    INTEGER, INTENT(IN) :: i,j
    IF(compare(j,i)) CALL exchange(i,j)
    END SUBROUTINE compex
  
    SUBROUTINE moveValue(i,j)
! Overwrite the contents of the jth element
! with the contents of the ith element
    INTEGER, INTENT(IN) :: i,j
    a(i) = a(j)
    END SUBROUTINE moveValue
  
! The next three subprograms are used to store, 
! compare against and restore a particular element.
    LOGICAL FUNCTION compareValue(j)
    INTEGER, INTENT(IN) :: j
    compareValue = savedVal < a(j)
    END FUNCTION compareValue
  
    SUBROUTINE saveValue(i)
    INTEGER, INTENT(IN) :: i
    savedVal = a(i)
    END SUBROUTINE saveValue
  
    SUBROUTINE restoreValue(i)
    INTEGER, INTENT(IN) :: i
    a(i) = savedVal
    END SUBROUTINE restoreValue
