MODULE usersort

  USE sortdef, ONLY: sortbase

  TYPE, EXTENDS(sortbase) :: sortbaseup
  CONTAINS
    PROCEDURE :: compare => mycompare
    PROCEDURE :: compareValue => mycompareValue
  END TYPE
  
CONTAINS
  FUNCTION mycompare(sortdata,i,j)
  CLASS(sortbaseup), INTENT(INOUT) :: sortdata
  LOGICAL :: mycompare
  INTEGER, INTENT(IN) :: i,j
! Descending in-situ sorting  
  ASSOCIATE(a=>sortdata%items)
    mycompare = a(i) < a(j)
  END ASSOCIATE
    
  END FUNCTION mycompare
  
  FUNCTION mycompareValue(sortdata,j)
  CLASS(sortbaseup), INTENT(INOUT) :: sortdata
  LOGICAL :: mycompareValue
  INTEGER, INTENT(IN) :: j
! Descending in-situ sorting  
  ASSOCIATE(a=>sortdata%items, saved_val=>sortdata%saved_val)
    mycompareValue = saved_val < a(j)
  END ASSOCIATE 
  
  END FUNCTION mycompareValue

END MODULE usersort
