MODULE sortdef

USE set_precision, ONLY: wp

  TYPE sortbase
    REAL(wp), ALLOCATABLE :: items(:)
    REAL(wp) :: saved_val
    INTEGER :: left, right
    CONTAINS
      PROCEDURE :: exchange
      PROCEDURE :: compare
      PROCEDURE :: compareValue
      PROCEDURE :: saveValue
      PROCEDURE :: restoreValue
      PROCEDURE :: moveValue
  END TYPE

CONTAINS

  FUNCTION compare(sortdata,i,j)
  CLASS(sortbase), INTENT(INOUT) :: sortdata
  LOGICAL :: compare
  INTEGER, INTENT(IN) :: i,j

  ASSOCIATE(a=>sortdata%items)
    compare = a(i) < a(j)
  END ASSOCIATE

  END FUNCTION compare

  FUNCTION compareValue(sortdata,j)
  CLASS(sortbase), INTENT(INOUT) :: sortdata
  INTEGER, INTENT(IN) :: j
  LOGICAL :: compareValue

  ASSOCIATE(a=>sortdata%items, saved_val=>sortdata%saved_val)
    compareValue = saved_val < a(j)
  END ASSOCIATE

  END FUNCTION compareValue


  SUBROUTINE saveValue(sortdata,i)
  CLASS(sortbase), INTENT(INOUT) :: sortdata
  INTEGER, INTENT(IN) :: i

  ASSOCIATE(a=>sortdata%items, saved_val=>sortdata%saved_val)
    saved_val = a(i)
  END ASSOCIATE

  END SUBROUTINE saveValue

  SUBROUTINE moveValue(sortdata,i,j)
  CLASS(sortbase), INTENT(INOUT) :: sortdata
  INTEGER, INTENT(IN) :: i,j

  ASSOCIATE(a=>sortdata%items)
    a(i) = a(j)
  END ASSOCIATE

  END SUBROUTINE moveValue

  SUBROUTINE restoreValue(sortdata, i)
  CLASS(sortbase), INTENT(INOUT) :: sortdata
  INTEGER, INTENT(IN) :: i

  ASSOCIATE(a=>sortdata%items, saved_val=>sortdata%saved_val)
    a(i) = saved_val
  END ASSOCIATE

  END SUBROUTINE restoreValue

  SUBROUTINE exchange(sortdata,i,j)
  CLASS(sortbase), INTENT(INOUT) :: sortdata
  INTEGER, INTENT(IN) :: i,j

  REAL(wp) :: t
  ASSOCIATE(a=>sortdata%items)
    t = a(i)
    a(i) = a(j)
    a(j) = t
  END ASSOCIATE

  END SUBROUTINE exchange

END MODULE sortdef
