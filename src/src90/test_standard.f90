! https://stdlib.fortran-lang.org/page/specs/stdlib_sparse.html#sparse-matrix-derived-types
! https://stdlib.fortran-lang.org/page/specs/stdlib_sorting.html#sort_index-creates-an-array-of-sorting-indices-for-an-input-array-while-also-sorting-the-array

program example_sort_index

#ifdef fortran_stdlib
  use stdlib_sorting, only: sort_index
  use stdlib_linalg_constants, only: dp
  use stdlib_sparse
! testing conversions
  use sparseAssign
  use sparseTypes
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

#ifdef fortran_stdlib
  integer :: row(10), col(10)
  real(dp) :: data(10)
  type(COO_dp_type) :: COO
  type(CSR_dp_type) :: CSR
  type(ELL_dp_type) :: ELL
  real(dp) :: mat(2, 2)
  real(dp), allocatable :: dense_matrix(:, :)
  integer :: locdof(2)
  TYPE (dpCSRSparseMatrix) :: dpCSR
#endif


  real(wp) :: array(7)
  integer :: index(7),i,j

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

#ifdef fortran_stdlib
  ! Initial data
  row = [1,1,2,2,3,3,3,4,4,4]
  col = [1,5,1,2,2,3,4,1,3,4]
  data = real([9,-3,4,7,8,-1,8,4,5,6] , kind = dp ) !wp works here

  mat(:, 1) = [1._dp, 2._dp]
  mat(:, 2) = [2._dp, 1._dp]
  allocate (dense_matrix(5, 5), source=0._dp)
  do i = 0, 3
     dense_matrix(1 + i:2 + i, 1 + i:2 + i) = dense_matrix(1 + i:2 + i, 1 + i:2 + i) + mat
  end do

  print *, 'Original Matrix'
  do j = 1, 5
     print '(5f8.1)', dense_matrix(j, :)
  end do

  ! Create a CSR matrix from triplet
  call from_ijv(CSR,row,col,data)
  print *, 'CSR'
  print '(A,5I8)',    'rowptr :', CSR%rowptr
  print '(A,10I8)',   'col    :', CSR%col
  print '(A,10f8.1)', 'data   :', CSR%data




  call dcsr_eq_dcsr_std(dpCSR,CSR)


  ! Create an ELL matrix from triplet
  call from_ijv(ELL,row,col,data)
  print *, 'ELL'
  print *, ' index        |         data'
  do i = 1, ELL%nrows
      print '(3I4,1x,3f8.1)', ELL%index(i,:) , ELL%data(i,:)
  end do

  ! Create a COO matrix from triplet
  call from_ijv(COO,row,col,data)
  print *, 'COO'
  print *, '  i,  j,    v'
  do i = 1, COO%nnz
      print '(2I4,f8.1)', COO%index(:,i), COO%data(i)
  end do

  call coo2csr(COO,CSR)

  print *, 'COO from CSR'
  print *, '  i,  j,    v'
  do i = 1, COO%nnz
      print '(2I4,f8.1)', COO%index(:,i), COO%data(i)
  end do

! Initialize CSR data and reset dense reference matrix
  call dense2coo(dense_matrix, COO)
  dense_matrix = 0._dp

print *, 'COO from dense'
print *, '  i,  j,    v'
do i = 1, COO%nnz
    print '(2I4,f8.1)', COO%index(:,i), COO%data(i)
end do

#endif

end program example_sort_index