module special_fct

USE set_precision, ONLY : wp

INTERFACE OPERATOR (.p.) ! binary operator summation convention/tensors
!   a .p. b returns scalar sum matrices; rank 0 of a(i,j)*b(i,j) a,b rank 2
!   a .p. b returns scalar sum of vectors; rank 0 of a(i)*b(i) a,b rank 1
!   a .p. b returns vector sum(j) rank 1 of a(i)*b(i,j) a,b rank 1,2
!   a .p. b returns vector sum(i) rank 1 of a(i,j)*b(j) a,b rank 2,1
 MODULE PROCEDURE sum_of_sum_matrix_by_matrix, sum_of_vector_by_matrix, &
                  sum_of_matrix_by_vector, sum_of_vector_by_vector
END INTERFACE

CONTAINS
 
! vector & matrix operations

! scalar matrix (inner) product
function sum_of_sum_matrix_by_matrix(array1,array2) result(dL2)
 REAL (wp), INTENT (IN) :: array1(:,:),array2(:,:)
 real(wp) ::  v3(size(array1,1))
 v3=[(1,i=1,size(array1,1))]
 dL2=dot_product(v3,matmul(v3,array1*array2))
end function sum_of_sum_matrix_by_matrix

! scalar vector (inner) product
function sum_of_vector_by_vector(v1,v2) result(dL2)
 REAL (wp), INTENT (IN) :: v1(:),v2(:)
 REAL (wp) :: dL2
 dl2=dot_product(v1,v2)
end function sum_of_vector_by_vector

! vector x matrix (inner) product
function sum_of_vector_by_matrix(v1,array2) result(v2)
 REAL (wp), INTENT (IN) :: v1(:),array2(:,:)
 REAL (wp) :: v2(SIZE(array2,1))
 v2=matmul(v1,array2)
end function sum_of_vector_by_matrix

! matrix x vector (inner) product
function sum_of_matrix_by_vector(array1,v2) result(v1)
 REAL (wp), INTENT (IN) :: v2(:),array1(:,:)
 REAL (wp) :: v1(SIZE(array1,2))
 v1=matmul(array1,v2)
end function sum_of_matrix_by_vector

! epsilon & factorial functions

function eps2(m) result(e) !eps2(0)=2, eps2(m)=1 m /=0
 INTEGER :: e
 INTEGER, INTENT(IN) :: m
 if (m == 0) then
  e=2
 else
  e=1
 endif    
end function eps2

recursive function fact(n)  result(f) ! classic recursive factorial
 INTEGER :: f
 INTEGER, INTENT(IN) :: n
 if (n < 0) then
  write(*,*) 'No negative n in fact(n): ',n
  stop
 endif
  if (ABS(n) > 100) then
  write(*,*) 'too large factorial: ',n
  stop
 endif
 if (n == 0) then
   f = 1
 else
   f = n * fact(n-1)
 endif
end function fact

function binomial(n,k)  result (m)
 INTEGER :: m
 INTEGER, INTENT(IN) :: n,k
! m = fact(n)/(fact(k)*fact(n-k)) ! inefficient
 if (k > (n-k)) then
   m = pfact(n,k)/pfact(n-k,0)
  else
   m = pfact(n,n-k)/pfact(k,0)
 endif
end function binomial

function pfact(n,k)  result(f) ! partial factorial k+1 to n: pfact(n,1)=pfact(n,0)=fact(n)
 INTEGER :: f,i
 INTEGER, INTENT(IN) :: n,k
 if (n < 0 .OR. k < 0) then
  write(*,*) 'No n < 0 or k < 0 in pfact(n): ',n
  stop
 endif
 if (n < k) then
  write(*,*) 'n < k in pfact(n): ',n,k
  stop
 endif
  if (ABS(n) > 100) then
  write(*,*) 'too large factorial in pfact: ',n
  stop
 endif
 if ((n-k) == 0) then
  f = 1
 else
  f = 1
   do i=k+1,n                     ! do loop factorial: k+1 to n
    f = f*i
   end do
 endif
end function pfact

end module special_fct

