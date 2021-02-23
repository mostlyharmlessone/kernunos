module zernicke

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


!!!!!!!
! this section removed from cornea_arrays.f90

!function Normalize(b) result(a) ! puts b on unit circle
! TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
! TYPE(wpRadSlopeMatrix) :: a
! real(wp) :: rBo
! integer :: M1,N1,i,j 
! N1=size(b%r,2) !N1=N 
! M1=size(b%r,1) !M1=MM
! allocate (a%r(MM,N),a%Zp(MM,N),a%Zp2(MM,N),&
!            a%Zt2(MM,N),a%thta(MM),a%MV(MM))
! rBo=-1E30
!  do i=1,N1 
!   do j=1,M1
!    if (ABS(b%r(j,i)) >= rBo) rBo=ABS(b%r(j,i)) !find maximum radius
!   end do
!  end do
!  a%r(:,:)=b%r(:,:)/rBo
!  a%Zp(:,:)=b%Zp(:,:)/rBo 
!  a%thta(:)=b%thta(:) 
!end function Normalize

!G(rho,phi),F(rho,phi)
!L2 inner product/norm <F,G>=integral{(F*G*rho)(drho)(dphi)}
! Sum over integers (m,n)( (Sum over points MM,N elevation(i=1,MM, J=1,N)*Z(at knots(MM,N),m,n) )
! compute Zmn for m,n at knots -> array or big array, matrix multiply with elevation array scaled to unit circle

!Zernicke coefficients on UNIT circle 0<rho<1, 0<phi<2pi
!G(rho,phi)=Sum(m,n){amnZmn(rho,phi)+bmnZ-mn(rho,phi)} can be expressed where
!amn=(2n+2)/(eps(m)*PI)<G,Z+mn>
!bmn=(2n+2)/(eps(m)*PI)<G,Z-mn>

!function ZernickeC(a,n2,m2) result(c) 
! TYPE(wpRadSlopeMatrix),INTENT(IN) :: a ! has to be normalized to unit circle
! INTEGER,INTENT(IN) :: n2,m2 ! denotes zernicke coefficient indices
! N1=size(a%r,2) !N1=N 
! M1=size(a%r,1) !M1=MM
!  do i=1,N1 
!    do j=1,M1
!     b(j,i)=Zern(n2,m2,a%r(j,i),a%thta(j))*a%r(j,i)
!    end do
!  end do
! c = (2*n2+2)/(eps2(m2)*PI) * (a%Zp .p. b) 
! end function ZernickeC

!!!!!!!!!!!!

function Zern(n,m,rho,phi) result(zed)
real(wp),INTENT(IN) :: rho,phi
INTEGER,INTENT(IN) :: m,n
 !write(*,*) m,n 
 if (n >= ABS(m)) then ! n>=m>=0 m=0 only for cos variation
  if (m >= 0) then
   zed=RZern(n,m,rho)*cos(m*phi)
  else
   zed=RZern(n,ABS(m),rho)*sin(ABS(m)*phi)
  endif
 else
  write(*,*) 'Illegal n,m in Z:',n,m
  stop
 endif
end function Zern

function RZern(n,m,rho) result(radk)  !r(m,n,rho)=sum(k,0,(n-m)/2) sum over k=0,(n-m)/2
real(wp),INTENT(IN) :: rho
INTEGER,INTENT(IN) :: m,n
  if (rho == 1) then
   radk=1
   return
  endif
  radk=0
  if (mod(n-m,2) == 0) then
  do k=0,(n-m)/2
   radk=radk+(rho**(n-(2*k)))*(fact(n-k)*(-1)**k)/(fact(k)*fact((n+m)/2-k)*fact((n-m)/2-k)) 
  end do
  else
   radk=0
  endif
end function RZern
  
! vector & matrix operations

!! scalar matrix (inner) product
!function sum_of_sum_matrix_by_matrix(array1,array2) result(dL2)
! REAL (wp), INTENT (IN) :: array1(:,:),array2(:,:)
! REAL (wp) :: dL2
! integer :: M1,N1
! M1=size(array1,1)
! N1=size(array1,2)
! if (size(array2,1) /= M1) write (*,*) 'rank error'
! if (size(array2,2) /= N1) write (*,*) 'rank error'
! dL2 = 0
! do i=1,M1
!  do j=1,N1
!  dL2=dL2+array1(i,j)*array2(i,j)
!  end do
! end do 
!end function sum_of_sum_matrix_by_matrix

function sum_of_sum_matrix_by_matrix(array1,array2) result(dL2)
 REAL (wp), INTENT (IN) :: array1(:,:),array2(:,:)
 real(wp) ::  v3(size(array1,1))
 v3=[(1,i=1,size(array1,1))]
 dL2=dot_product(v3,matmul(v3,array1*array2))
end function sum_of_sum_matrix_by_matrix

!! scalar vector (inner) product
!function sum_of_vector_by_vector(v1,v2) result(dL2)
! REAL (wp), INTENT (IN) :: v1(:),v2(:)
! REAL (wp) :: dL2
! integer :: M1
! M1=size(v1)
! if (size(v2) /= M1) write (*,*) 'rank error'
! dL2 = 0
! do i=1,M1
!  dL2=dL2+v1(i)*v2(i)
! end do 
!end function sum_of_vector_by_vector

function sum_of_vector_by_vector(v1,v2) result(dL2)
 REAL (wp), INTENT (IN) :: v1(:),v2(:)
 REAL (wp) :: dL2
 dl2=dot_product(v1,v2)
end function sum_of_vector_by_vector

!! vector x matrix (inner) product
!function sum_of_vector_by_matrix(v1,array2) result(v2)
! REAL (wp), INTENT (IN) :: v1(:),array2(:,:)
! REAL (wp) :: v2(SIZE(array2,1))
! integer :: M1,N1
! M1=size(v1)
! N1=size(array2,1)
! if (size(array2,2) /= M1) write (*,*) 'rank error'
! do i=1,N1
! v2(i)=0
!  do j=1,M1
!  v2(i)=v2(i)+v1(j)*array2(j,i)
!  end do
! end do 
!end function sum_of_vector_by_matrix

! vector x matrix (inner) product
function sum_of_vector_by_matrix(v1,array2) result(v2)
 REAL (wp), INTENT (IN) :: v1(:),array2(:,:)
 REAL (wp) :: v2(SIZE(array2,1))
 v2=matmul(v1,array2)
end function sum_of_vector_by_matrix

!! matrix x vector (inner) product
!function sum_of_matrix_by_vector(array1,v2) result(v1)
! REAL (wp), INTENT (IN) :: v2(:),array1(:,:)
! REAL (wp) :: v1(SIZE(array1,2))
! integer :: M1,N1
! M1=size(v2)
! N1=size(array1,2)
! if (size(array1,1) /= M1) write (*,*) 'rank error'
! do i=1,N1
! v1(i)=0
!  do j=1,M1
!  v1(i)=v1(i)+v2(j)*array1(i,j)
!  end do
! end do 
!end function sum_of_matrix_by_vector

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

recursive function fact(n)  result(f) ! factorial
 INTEGER :: f
 INTEGER, INTENT(IN) :: n
 if (n < 0) then
  write(*,*) 'Illegal negative n in Factorial(n): ',n
  stop
 endif
  if (ABS(n) > 100) then
  write(*,*) 'Runaway factorial: ',n
  stop
 endif
 if (n == 0) then
   f = 1
 else
   f = n * fact(n-1)
 endif
end function fact


end module zernicke

