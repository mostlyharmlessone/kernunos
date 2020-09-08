      MODULE concrete
        USE set_precision, ONLY : wp
        USE sortElements, ONLY: abstractSortbase

        TYPE, EXTENDS(abstractSortbase) :: dpdata
          REAL(wp), ALLOCATABLE :: items(:)
        CONTAINS
          PROCEDURE :: exchange => exchangeDP
          PROCEDURE :: compare => compareDP
          PROCEDURE :: compareValue => compareValueDP
          PROCEDURE :: saveValue => saveValueDP
          PROCEDURE :: restoreValue => restoreValueDP
          PROCEDURE :: moveValue => moveValueDP
        END TYPE dpdata

        REAL(wp) :: savedValue

      CONTAINS
        SUBROUTINE exchangeDP(elem, i, j)
        CLASS(dpdata), INTENT(INOUT) :: elem
        INTEGER, INTENT(IN) :: i, j
        REAL(wp) :: temp
          ASSOCIATE(vals=>elem%items)
          temp = vals(i)
          vals(i) = vals(j)
          vals(j) = temp
          END ASSOCIATE
        END SUBROUTINE exchangeDP
        
        LOGICAL FUNCTION compareDP(elem, i, j)
        CLASS(dpdata), INTENT(IN) :: elem
        INTEGER, INTENT(IN) :: i, j
          ASSOCIATE(vals=>elem%items)
          compareDP = (vals(i) < vals(j))
          END ASSOCIATE
        END FUNCTION compareDP
        
        LOGICAL FUNCTION compareValueDP(elem, j)
        CLASS(dpdata), INTENT(IN) :: elem
        INTEGER, INTENT(IN) :: j
          ASSOCIATE(vals=>elem%items)
          compareValueDP = (savedValue < vals(j))
          END ASSOCIATE
        END FUNCTION compareValueDP

        SUBROUTINE saveValueDP(elem, j)
        CLASS(dpdata), INTENT(IN) :: elem
        INTEGER, INTENT(IN) :: j
          ASSOCIATE(vals=>elem%items)
          savedValue = vals(j)
          END ASSOCIATE
        END SUBROUTINE saveValueDP

        SUBROUTINE restoreValueDP(elem, i)
        CLASS(dpdata), INTENT(INOUT) :: elem
        INTEGER, INTENT(IN) :: i
          ASSOCIATE(vals=>elem%items)
          vals(i) = savedValue
          END ASSOCIATE
        END SUBROUTINE restoreValueDP
        
        SUBROUTINE moveValueDP(elem, i, j)
        CLASS(dpdata), INTENT(INOUT) :: elem
        INTEGER, INTENT(IN) :: i, j
          ASSOCIATE(vals=>elem%items)
          vals(i) = vals(j)
          END ASSOCIATE
        END SUBROUTINE moveValueDP
      END MODULE concrete
