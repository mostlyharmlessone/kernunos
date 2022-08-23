module special_fct

use set_precision, ONLY : wp
use ISO_FORTRAN_ENV, only: INT8,INT16,INT32,REAL32
use, intrinsic ::  ieee_arithmetic

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
 real(wp) ::  v3(size(array1,1)),dL2
 integer :: i
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

!! REAL32 functions for STL facet calcs

! vector vector (cross) product (REAL32  and dimension 3)
function cross_product(v1, v2) result(v3)
  real(REAL32), INTENT(IN) :: v1(3), v2(3)
  real(REAL32) :: v3(3)
  v3(1) = v1(2) * v2(3) - v1(3) * v2(2)
  v3(2) = v1(3) * v2(1) - v1(1) * v2(3)
  v3(3) = v1(1) * v2(2) - v1(2) * v2(1)
end function cross_product

! surface normal vector (REAL32 and dimension 3)
function surface_normal(v1, v2, v3) result(v4)
  real(REAL32), INTENT(IN) :: v1(3), v2(3), v3(3) ! vertices of triangle
  real(REAL32) :: v4(3)
  v4(1) = (v2(2)-v1(2)) * (v3(3)-v1(3)) - (v2(3)-v1(3)) * (v3(2)-v1(2))
  v4(2) = (v2(3)-v1(3)) * (v3(1)-v1(1)) - (v2(1)-v1(1)) * (v3(3)-v1(3))
  v4(3) = (v2(1)-v1(1)) * (v3(2)-v1(2)) - (v2(2)-v1(2)) * (v3(1)-v1(1))
end function surface_normal

!! color functions

! convert values to rgb 2 color (red to blue) heatmap
! input 3 scalars, output integer(kind=2) vector
! https://stackoverflow.com/questions/20792445/calculate-rgb-value-for-a-range-of-values-to-create-heat-map    
function rgb2(x,minimum, maximum) result(rgbv)
 REAL (wp), INTENT (IN) :: minimum,maximum,x
 REAL (wp) :: ratio
 INTEGER(int16) :: rgbv(3) ! rgbv={r,g,b}
 INTEGER(int16), PARAMETER :: zero=0
    ratio = 2 * (x-minimum) / (maximum - minimum)
    rgbv(3) = max(zero, int(255*(1 - ratio),2))
    rgbv(1) = max(zero, int(255*(ratio - 1),2))
    rgbv(2) = int(255,2) - rgbv(3) - rgbv(1)
end function rgb2

! convert values to rgb 5 color (red to blue) heatmap
! input 3 scalars, output integer(kind=2) vector
! http://www.andrewnoske.com/wiki/Code_-_heatmaps_and_color_gradients  
! https://stackoverflow.com/questions/3708307/how-to-initialize-two-dimensional-arrays-in-fortran 
function rgb5(x,minimum, maximum) result(rgbv)
 REAL (wp), INTENT (IN) :: minimum,maximum,x
 REAL (wp) :: ratio,fract
 INTEGER(int16) :: nc,rgbv(3),idx1,idx2 ! rgbv={r,g,b}
! color={{0,0,255},{0,255,255},{0,255,0},{255,255,0},{255,0,0}}
!! INTEGER(int16) :: color(3,5)=reshape( (/ 0, 0, 255, &      !blue
!!                                                       0, 255, 255, &    !cyan 
!!                                                       0, 255, 0, &      !green
!!                                                       255, 255, 0, &    !yellow
!!                                                       255, 0, 0 /), &   !red
!!                                           (/3,5/)  )
! color={{255,0,0},{255,255,0},{0,255,0},{0,255,255},{0,0,255}}
 INTEGER(int16) :: color(3,5)=reshape( (/              255, 0, 0, &      !red
                                                       255, 255, 0, &    !yellow
                                                       0, 255, 0, &      !green
                                                       0, 255, 255, &    !cyan 
                                                       0, 0, 255/), &    !blue                                                         
                                           (/3,5/)  )
  nc=5
! A static array of 5 colors:  (blue, cyan, green, yellow, red) using full rgb for each.
! desired color will be between idx1,idx2 in "color".
  ratio =  (x-minimum) / (maximum - minimum) 
  if (ratio < 1 .and. ratio > 0 ) then 
   fract= 0 ! Fraction between "idx1" and "idx2" where our value is.
  
   if (ratio <= 0) then
       idx1 = 1 ; idx2 = 1                   ! accounts for an input <=0
   else
    if (ratio >= 1) then
     idx1 = nc ; idx2 = nc                ! accounts for an input >=1
    else
     ratio = ratio * (nc-1)                  
     idx1  = floor(ratio)+1                   ! Desired color will be after this index.
     idx2  = idx1+1                           ! ... and before this index (inclusive).
     fract = ratio - real(idx1)+1            ! Distance between the two indexes (0-1).
    endif
   endif

   if (idx1 == 0 .OR.  idx2 == 0) then
    write(*,*) 'x,min,max,ratio: ',x,minimum,maximum,ratio
   endif
  
   rgbv(1) = (color(1,idx2) - color(1,idx1))*fract + color(1,idx1)
   rgbv(2) = (color(2,idx2) - color(2,idx1))*fract + color(2,idx1)
   rgbv(3) = (color(3,idx2) - color(3,idx1))*fract + color(3,idx1)
  else
   rgbv=(/255,255,255/)  ! out of range = white
  endif
   

end function rgb5

! Converts full RGB (256x256x256) to SolidView 15 bit color attr
function rgb2attr(rgbv) result(attr)
  integer(kind=2), INTENT(IN) :: rgbv(3)  !=INT16
  integer(INT16) :: attr
  integer(INT8) :: red,green,blue 
!    bits 0 to 4 are the intensity level for blue (0 to 31),
!    bits 5 to 9 are the intensity level for green (0 to 31),
!    bits 10 to 14 are the intensity level for red (0 to 31),
!    bit 15 is 1 if the color is valid, or 0 if the color is not valid (as with normal STL files).
!    attr= b'0000001100000001'   ! 00000 01100 00000 1 == blue 0, green 12, red 0, valid
 ! attr=0 
  red =  rgbv(1)/8 
  green = rgbv(2)/8 
  blue =  rgbv(3)/8 
  attr=blue + 32*green + 1024*red ! packs the bits according to the scheme above
  attr=ibset(attr,15) ! sets position 15 to 1
!  write(*,*) rgbv
!  write(*,*) red,green,blue
!  write(*,'(b8.8)') red
!  write(*,'(b8.8)') green
!  write(*,'(b8.8)') blue
!  write(*,'(b16.16)') attr
end function rgb2attr

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


!!https://stackoverflow.com/questions/58938347/how-do-i-replace-a-character-in-the-string-with-another-charater-in-fortran

pure recursive function replaceStr(string,search,substitute) result(modifiedString)
        implicit none
        character(len=*), intent(in)  :: string, search, substitute
        character(len=:), allocatable :: modifiedString
        integer                       :: i, stringLen, searchLen
        stringLen = len(string)
        searchLen = len(search)
        if (stringLen==0 .or. searchLen==0) then
            modifiedString = ""
            return
        elseif (stringLen<searchLen) then
            modifiedString = string
            return
        end if
        i = 1
        do
            if (string(i:i+searchLen-1)==search) then
                modifiedString = string(1:i-1) // substitute // replaceStr(string(i+searchLen:stringLen),search,substitute)
                exit
            end if
            if (i+searchLen>stringLen) then
                modifiedString = string
                exit
            end if
            i = i + 1
            cycle
        end do
    end function replaceStr

end module special_fct

