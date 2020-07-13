      MODULE sort_elements

        TYPE, ABSTRACT ::  abstractsortbase
          INTEGER :: left, right

        CONTAINS
          PROCEDURE(exchangeElements), DEFERRED :: exchange
          PROCEDURE(compareElements), DEFERRED :: compare
          PROCEDURE(compareElementValue), DEFERRED :: compareValue
          PROCEDURE(saveElementValue), DEFERRED :: saveValue
          PROCEDURE(restoreElementValue), DEFERRED :: restoreValue
          PROCEDURE(moveElementValue), DEFERRED :: moveValue
        END TYPE abstractsortbase

        ABSTRACT INTERFACE
          SUBROUTINE exchangeElements(elem, i, j)
          IMPORT :: abstractsortbase
          CLASS(abstractsortbase), INTENT(INOUT) :: elem
          INTEGER, INTENT(IN) :: i, j
          END SUBROUTINE
          LOGICAL FUNCTION compareElements(elem, i, j)
          IMPORT :: abstractsortbase
          CLASS(abstractsortbase), INTENT(IN) :: elem
          INTEGER, INTENT(IN) :: i, j
          END FUNCTION
          LOGICAL FUNCTION compareElementValue(elem,j)
          IMPORT :: abstractsortbase
          CLASS(abstractsortbase), INTENT(IN) :: elem
          INTEGER, INTENT(IN) :: j
          END FUNCTION
          SUBROUTINE saveElementValue(elem,j)
          IMPORT :: abstractsortbase
          CLASS(abstractsortbase), INTENT(IN) :: elem
          INTEGER, INTENT(IN) :: j
          END SUBROUTINE
          SUBROUTINE restoreElementValue(elem,i)
          IMPORT :: abstractsortbase
          CLASS(abstractsortbase), INTENT(INOUT) :: elem
          INTEGER, INTENT(IN) :: i
          END SUBROUTINE
          SUBROUTINE moveElementValue(elem, i, j)
          IMPORT :: abstractsortbase
          CLASS(abstractsortbase), INTENT(INOUT) :: elem
          INTEGER, INTENT(IN) :: i, j
          END SUBROUTINE
        END INTERFACE

      CONTAINS
      ...
      ...
      END MODULE sort_elements
