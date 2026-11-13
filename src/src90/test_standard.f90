! https://stdlib.fortran-lang.org/page/specs/stdlib_sparse.html#sparse-matrix-derived-types
! https://stdlib.fortran-lang.org/page/specs/stdlib_sorting.html#sort_index-creates-an-array-of-sorting-indices-for-an-input-array-while-also-sorting-the-array

program example_sort_index

  use stdlib_sorting, only: sort_index
  use stdlib_sparse
  use stdlib_system, only: dir_name,base_name
! testing conversions
  use sparseAssign
  use sparseTypes
  use sparseUtils
  use sparseops
  use special_fct

  use set_precision, only: wp
  implicit none

  integer :: row(10), col(10)
  real(wp) :: data(10),xx
  type(COO_dp_type) :: COO, COO_T
  type(CSR_dp_type) :: CSR, CSR_T, CSR2
  real(wp) :: mat(2, 2)
  real(wp), allocatable :: dense_matrix(:, :),dense_matrix_T(:, :),dense_matrix2(:, :)
  TYPE (dpCSRSparseMatrix) :: dpCSR, dpCSR_T, dp_CSR2
  TYPE (dpTripletList) :: dptriplist
  TYPE (dpTripletList) :: dptriplist_T

  real(wp) :: array(7)
  integer :: index(7),i,j,nblines

  real(wp),allocatable :: vector(:),vector2(:)
  CHARACTER(len=4096) :: new_path
  CHARACTER(:),save, ALLOCATABLE :: file_name, path_name, full_name

    new_path="C:/Users/amdeb/kernunos/data/ED0000010002L.CAB"

    new_path="C:\Users\amdeb\kernunos\data\ED0000010002L.CAB"

    new_path="/MyFileWithoutASeparator"
    new_path="MyFileWithoutASeparator/"
    new_path="MyFileWithoutASeparator"

    nblines=len(trim(new_path))
    allocate(CHARACTER(nblines) :: file_name)
    allocate(CHARACTER(nblines) :: path_name)
    allocate(CHARACTER(nblines) :: full_name)

    full_name=trim(new_path)
    path_name=dir_name(full_name)
    file_name=base_name(full_name)

    write(*,*) "full_name ",full_name
    write(*,*) "path_name ",path_name
    write(*,*) "file_name ",file_name

  stop

  array = [5, 4, 3, 1, 10, 4, 9]
  index = [1, 2, 3, 4, 5, 6 ,7]


  call sort_index(array, index)

  print *, array   !print [1, 3, 4, 4, 5, 9, 10]
  print *, index   !print [4, 3, 2, 6, 1, 7, 5]

  call heapsort(array, index, 7)

  print *, array   !print [1, 3, 4, 4, 5, 9, 10]
  print *, index   !print [4, 3, 2, 6, 1, 7, 5]

  ! Initial data
  row = [1,1,2,2,3,3,3,4,4,4]
  col = [1,5,1,2,2,3,4,1,3,4]
  data = real([9,-3,4,7,8,-1,8,4,5,6] , kind = wp )

  ! Create a CSR matrix from triplet
  call from_ijv(CSR,row,col,data)
  print *, 'CSR'
  print *,'nrows,ncols,nnz',CSR%nrows,CSR%ncols,CSR%nnz
  print '(A,5I8)',    'rowptr :', CSR%rowptr
  print '(A,10I8)',   'col    :', CSR%col
  print '(A,10f8.1)', 'data   :', CSR%data

call CSR%malloc(4,5,10)
CSR%data(:)   = real([9,-3,4,7,8,-1,8,4,5,6])
CSR%col(:)    = [1,5,1,2,2,3,4,1,3,4]
CSR%rowptr(:) = [1,3,5,8,11]

print *, 'CSR by definition'
print *,'nrows,ncols,nnz',CSR%nrows,CSR%ncols,CSR%nnz
print '(A,5I8)',    'rowptr :', CSR%rowptr
print '(A,10I8)',   'col    :', CSR%col
print '(A,10f8.1)', 'data   :', CSR%data

do i=1,10
  dptriplist = dpTriplet(row(i),col(i),data(i))
end do
  dptriplist_T=.t. dptriplist

  dpCSR = dptriplist
  dpCSR_T = dptriplist_T

  call printDpCSRSparseMatrix(dpCSR)
  call printDpCSRSparseMatrix(dpCSR_T)

  dense_matrix = dpCSR
  dense_matrix_T = dpCSR_T

print *, 'converted from dpcsr first'
do j = 1, size(dense_matrix,1)
   print '(5f8.1)', dense_matrix(j, :)
end do
print *, 'converted from dpcsr_T first'
do j = 1, size(dense_matrix_T,1)
   print '(5f8.1)', dense_matrix_T(j, :)
end do

allocate(vector(size(dense_matrix,2)),vector2(size(dense_matrix,1)))
do i=1,size(dense_matrix,2)
vector(i) = 3.0*i
end do
print *, size(dense_matrix,1),size(dense_matrix,2)
print *, vector(:)
print *, matmul(dense_matrix,vector)
vector2(:) =dense_matrix .p. vector
print *, vector2(:)

vector2 = dpCSR .p. vector
print *, vector2(:)

call spmv(CSR,vector,vector2)
print *, vector2(:)
vector2 = CSR .p. vector
print *, vector2(:)

deallocate(vector,vector2)


allocate(vector(size(dense_matrix_T,2)),vector2(size(dense_matrix_T,1)))
do i=1,size(dense_matrix_T,2)
vector(i) = 3.0*i
end do
print *, size(dense_matrix_T,1),size(dense_matrix_T,2)
print *, vector(:)
vector2 = dense_matrix_T .p. vector
print *, vector2(:)

vector2 = dpCSR_T .p. vector
print *, vector2(:)


deallocate(vector,vector2)


stop



dense_matrix2=matmul(dense_matrix_T,dense_matrix)

print *, 'ATA via matmul'
do j = 1, size(dense_matrix2,1)
   print '(5f8.1)', dense_matrix2(j, :)
end do

allocate(vector(size(dense_matrix2,1)))
do i=1,size(dense_matrix2,1)
vector(i) = 3.0*i
end do
print *, size(dense_matrix2,1),size(dense_matrix2,2)
print *, vector(:)
vector = dense_matrix2 .p. vector
print *, vector(:)
deallocate(vector)

dp_CSR2 = dpCSR_T .p. dpCSR
call printDpCSRSparseMatrix(dp_CSR2)

dense_matrix2 = dp_CSR2

print *, 'ATA via not standard csr'
do j = 1, size(dense_matrix2,1)
   print '(5f8.1)', dense_matrix2(j, :)
end do

allocate(vector(size(dense_matrix2,1)))
do i=1,size(dense_matrix2,1)
vector(i) = 3.0*i
end do
print *, size(dense_matrix2,1),size(dense_matrix2,2)
print *, vector(:)
vector = dense_matrix2 .p. vector
print *, vector(:)
deallocate(vector)

dense_matrix2=matmul(dense_matrix,dense_matrix_T)

print *, 'AAT via matmul'
do j = 1, size(dense_matrix2,1)
   print '(5f8.1)', dense_matrix2(j, :)
end do

allocate(vector(size(dense_matrix2,1)))
do i=1,size(dense_matrix2,1)
vector(i) = 3.0*i
end do
print *, size(dense_matrix2,1),size(dense_matrix2,2)
print *, vector(:)
vector = dense_matrix2 .p. vector
print *, vector(:)
deallocate(vector)

dp_CSR2 = dpCSR .p. dpCSR_T
 call printDpCSRSparseMatrix(dp_CSR2)

  dense_matrix2 = dp_CSR2

  print *, 'ATA via csr'
  do j = 1, size(dense_matrix2,1)
     print '(5f8.1)', dense_matrix2(j, :)
  end do

allocate(vector(size(dense_matrix2,1)))
do i=1,size(dense_matrix2,1)
vector(i) = 3.0*i
end do
print *, size(dense_matrix2,1),size(dense_matrix2,2)
print *, vector(:)
vector = dense_matrix2 .p. vector
print *, vector(:)
deallocate(vector)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call csr2dense(CSR,dense_matrix)

print *, 'converted from csr'
do j = 1, size(dense_matrix,1)
   print '(5f8.1)', dense_matrix(j, :)
end do

!dense_matrix_T = .t. dense_matrix

print *, 'CSR'
print *,'nrows,ncols,nnz',CSR%nrows,CSR%ncols,CSR%nnz
print '(A,5I8)',    'rowptr :', CSR%rowptr
print '(A,10I8)',   'col    :', CSR%col
print '(A,10f8.1)', 'data   :', CSR%data

call csr2dense(CSR,dense_matrix)

print *, 'converted from csr'
do j = 1, size(dense_matrix,1)
   print '(5f8.1)', dense_matrix(j, :)
end do

 CSR_T = .t. CSR

call CSR_T%malloc(5,4,10)
CSR_T%data(:)   = real([9,-3,4,7,8,-1,8,4,5,6])
CSR_T%col(:)    = [1,2,4,2,3,3,4,3,4,1]
CSR_T%rowptr(:) = [1,4,6,8,10,11]

print *, 'CSR_T'
print *,'nrows,ncols,nnz',CSR_T%nrows,CSR_T%ncols,CSR_T%nnz
print '(A,6I8)',    'rowptr :', CSR_T%rowptr
print '(A,10I8)',   'col    :', CSR_T%col
print '(A,10f8.1)', 'data   :', CSR_T%data

 call csr2dense(CSR_T,dense_matrix_T)

!these are element by element multiplication, not matmul equivalents

CSR2 = CSR * CSR_T
dense_matrix2 = CSR2

print *, 'converted after csr'
do j = 1, size(dense_matrix2,1)
  print '(5f8.1)', dense_matrix2(j, :)
end do

CSR2 = CSR_T * CSR
dense_matrix2 = CSR2

print *, 'converted after csr'
do j = 1, size(dense_matrix2,1)
  print '(5f8.1)', dense_matrix2(j, :)
end do


do i=1,10
  dptriplist = dpTriplet(row(i),col(i),data(i))
end do

call printDpTripletList(dptriplist)

write(*,*) 'transpose'
 dptriplist_T=.t. dptriplist

call printDpTripletList(dptriplist_T)

! Create a COO matrix from triplet
!call from_ijv(COO,row,col,data)
COO = dptriplist

print *, 'COO'
print *, '  i,  j,    v'
do i = 1, COO%nnz
    print '(2I4,f8.1)', COO%index(:,i), COO%data(i)
end do

COO_T = .t. COO

print *, 'COO_T'
print *, '  i,  j,    v'
do i = 1, COO_T%nnz
    print '(2I4,f8.1)', COO_T%index(:,i), COO_T%data(i)
end do
COO_T=0


COO_T = dptriplist_T

print *, 'COO_T'
print *, '  i,  j,    v'
do i = 1, COO_T%nnz
    print '(2I4,f8.1)', COO_T%index(:,i), COO_T%data(i)
end do

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




end program example_sort_index