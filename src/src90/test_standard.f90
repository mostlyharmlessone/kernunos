program example_sort_index

#ifdef fortran_stdlib
  use stdlib_sorting, only: sort_index
#endif

  use set_precision, only: wp
  implicit none

  INTERFACE
   SUBROUTINE heapsort(RealVector,IndexVector,n)
    USE set_precision, ONLY : wp
    INTEGER, INTENT(IN) :: n
    REAL(wp), INTENT(INOUT) :: RealVector(n)
    INTEGER, INTENT(INOUT) :: IndexVector(n)
   END SUBROUTINE
  END INTERFACE

  real(wp) :: array(7)
  integer :: index(7)

  array = [5, 4, 3, 1, 10, 4, 9]
  index = [1, 2, 3, 4, 5, 6 ,7]

#ifdef fortran_stdlib
  call sort_index(array, index)

  print *, array   !print [1, 3, 4, 4, 5, 9, 10]
  print *, index   !print [4, 3, 2, 6, 1, 7, 5]
#endif

  call heapsort(array, index, 7)

  print *, array   !print [1, 3, 4, 4, 5, 9, 10]
  print *, index   !print [4, 3, 2, 6, 1, 7, 5]

end program example_sort_index