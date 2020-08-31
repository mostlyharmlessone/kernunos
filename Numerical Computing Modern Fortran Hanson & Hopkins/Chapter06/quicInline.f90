MODULE quicInline
USE set_precision, ONLY : wp

CONTAINS
  SUBROUTINE qsort(a, left, right)
  INTEGER, INTENT(IN) :: left, right
  REAL(wp), INTENT(INOUT) :: a(left:right)
  REAL(wp) :: saved_val

  CALL quicksort(left, right)
  CALL insertion

  CONTAINS

    
    SUBROUTINE insertion
    INTEGER :: i, j
    DOUBLE PRECISION :: t

    DO i = left+1, right
      IF (a(i) < a(left)) THEN
        t = a(i)
        a(i) = a(left)
        a(left) = t
      END IF
    END DO

    DO i = left+2, right
      j = i
      saved_val = a(i)
      DO WHILE(saved_val < a(j-1))
        a(j) = a(j-1)
        j = j-1
      END DO

      a(j) = saved_val
    END DO

    END SUBROUTINE insertion

    RECURSIVE SUBROUTINE quicksort(left, right)
    INTEGER, INTENT(IN) :: left, right

    INTEGER, PARAMETER:: switchsorts=10
    INTEGER :: i
    DOUBLE PRECISION :: t

    IF((right-left) > switchsorts) THEN
      t = a((right+left)/2)
      a((right+left)/2) = a(right-1)
      a(right-1) = t
      IF (a(right-1) < a(left)) THEN
        t=a(left)
        a(left) = a(right-1)
        a(right-1) = t
      END IF
      IF (a(left) < a(right)) THEN
        t=a(right)
        a(right) = a(left)
        a(left) = t
      END IF
      IF (a(right) < a(right-1)) THEN
        t=a(right-1)
        a(right-1) = a(right)
        a(right) = t
      END IF
      i = partition(left+1, right-1)
      CALL quicksort(left, i-1)
      CALL quicksort(i+1, right)
    END IF

    END SUBROUTINE quicksort

    FUNCTION partition(left, right) RESULT(i)
    INTEGER, INTENT(IN) :: left, right
    INTEGER :: i, j
    DOUBLE PRECISION :: t

    i = left - 1
    j = right
    saved_val = a(right)

    DO
      DO
        i = i+1
        IF(i>right) EXIT
        IF(saved_val < a(i)) EXIT
      END DO

      DO
        j = j-1
        IF(.NOT.(saved_val < a(j)) .OR. j==left) EXIT
      END DO

      IF(i>= j) EXIT
      t = a(i)
      a(i) = a(j)
      a(j) = t

    END DO

    t = a(i)
    a(i) = a(right)
    a(right) = t

    END FUNCTION partition
  END SUBROUTINE qsort
END MODULE quicInline
