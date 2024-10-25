      MODULE sparseSort
        USE sortElements, ONLY: abstractSortbase

        TYPE, EXTENDS(abstractSortbase) :: sparseData
          INTEGER, POINTER :: colPtr(:)
          INTEGER, POINTER :: indexPtr(:)
        CONTAINS
          PROCEDURE :: exchange => spExchange
          PROCEDURE :: compare => spCompare
          PROCEDURE :: compareValue => spCompareValue
          PROCEDURE :: saveValue => spSaveValue
          PROCEDURE :: restoreValue => spRestoreValue
          PROCEDURE :: moveValue => spMoveValue
        END TYPE sparseData

        INTEGER :: savedValue, savedIndex

      CONTAINS
        SUBROUTINE spExchange(elem, i, j)
        CLASS(sparseData), INTENT(INOUT) :: elem
        INTEGER, INTENT(IN) :: i, j
        INTEGER :: temp
          ASSOCIATE(inds=>elem%indexPtr)
          temp = inds(i)
          inds(i) = inds(j)
          inds(j) = temp
          END ASSOCIATE
        END SUBROUTINE spExchange
        
        LOGICAL FUNCTION spCompare(elem, i, j)
        CLASS(sparseData), INTENT(IN) :: elem
        INTEGER, INTENT(IN) :: i, j
          ASSOCIATE(vals=>elem%colPtr, inds=>elem%indexPtr)
          spCompare = (vals(inds(i)) < vals(inds(j)))
          END ASSOCIATE
        END FUNCTION spCompare
        
        LOGICAL FUNCTION spCompareValue(elem, j)
        CLASS(sparseData), INTENT(IN) :: elem
        INTEGER, INTENT(IN) :: j
          ASSOCIATE(vals=>elem%colPtr,inds=>elem%indexPtr)
          spCompareValue = (savedValue < vals(inds(j)))
          END ASSOCIATE
        END FUNCTION spCompareValue

        SUBROUTINE spSaveValue(elem, j)
        CLASS(sparseData), INTENT(IN) :: elem
        INTEGER, INTENT(IN) :: j
          ASSOCIATE(vals=>elem%colPtr,inds=>elem%indexPtr)
          savedIndex = inds(j)
          savedValue = vals(savedIndex)
          END ASSOCIATE
        END SUBROUTINE spSaveValue

        SUBROUTINE spRestoreValue(elem, i)
        CLASS(sparseData), INTENT(INOUT) :: elem
        INTEGER, INTENT(IN) :: i
          ASSOCIATE(inds=>elem%indexPtr)
          inds(i) = savedIndex
          END ASSOCIATE
        END SUBROUTINE spRestoreValue
        
        SUBROUTINE spMoveValue(elem, i, j)
        CLASS(sparseData), INTENT(INOUT) :: elem
        INTEGER, INTENT(IN) :: i, j
          ASSOCIATE(inds=>elem%indexPtr)
          inds(i) = inds(j)
          END ASSOCIATE
        END SUBROUTINE spMoveValue
      END MODULE sparseSort
