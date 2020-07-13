MODULE quicksort

  USE set_precision, ONLY: wp
  USE sortdef, ONLY: sortbase, exchange, compare, compareValue, &
                     saveValue, restoreValue, moveValue

CONTAINS

  SUBROUTINE qsort(sortdata)
  CLASS(sortbase), INTENT(INOUT) :: sortdata
  INTEGER :: left, right

  left = sortdata%left
  right = sortdata%right

  CALL quicksort(left, right)
  CALL insertion

  CONTAINS
  
    SUBROUTINE insertion
  
    INTEGER :: i, j
  
    ASSOCIATE (left=>sortdata%left, right=>sortdata%right)
    DO i = left+1, right
      CALL compex(left, i)
    END DO
  
    DO i = left+2, right
      j = i
      CALL sortdata%saveValue(i)
      DO WHILE(sortdata%compareValue(j-1))
        CALL sortdata%moveValue(j, j-1)
        j = j-1
      END DO
  
      CALL sortdata%restoreValue(j)
    END DO
    END ASSOCIATE
  
    END SUBROUTINE insertion
  
    RECURSIVE SUBROUTINE quicksort(left, right)
    INTEGER, INTENT(IN) :: left, right
  
    INTEGER, PARAMETER:: switchsorts=10
    INTEGER :: i
  
    IF((right-left) > switchsorts) THEN
      CALL sortdata%exchange((right+left)/2, (right-1))
      CALL compex(left, right-1)
      CALL compex(right, left)
      CALL compex(right-1, right)
      i = partition(left+1, right-1)
      CALL quicksort(left, i-1)
      CALL quicksort(i+1, right)
    END IF
  
    END SUBROUTINE quicksort
  
    FUNCTION partition(left, right) RESULT(i)
    INTEGER, INTENT(IN) :: left, right
    INTEGER :: i
  
    INTEGER :: j
  
    i = left - 1
    j = right
    CALL  sortdata%saveValue(right)
  
    DO
      DO
        i = i+1
        IF(i>right) EXIT
        IF(sortdata%compareValue(i)) EXIT
      END DO
  
      DO
        j = j-1
        IF(.NOT.sortdata%compareValue(j) .OR. j==left) EXIT
      END DO
  
      IF(i>= j) EXIT
      CALL sortdata%exchange(i,j)
  
    END DO
  
    CALL sortdata%exchange(i, right)
  
    END FUNCTION partition

    SUBROUTINE compex(i,j)
    INTEGER, INTENT(IN) :: i,j

      IF(sortdata%compare(j,i)) &
               CALL sortdata%exchange(i,j)

    END SUBROUTINE compex
  END SUBROUTINE qsort

END MODULE quicksort

