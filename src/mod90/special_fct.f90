MODULE special_fct
! vector and matrix functions
! coordinate transform
! color functions
! zernike functions
! string replacement function
! binary search interface
USE parameters, ONLY : EPS, PI
USE set_precision, ONLY : wp, sk, int2d, int3d
USE ISO_FORTRAN_ENV, ONLY : INT8,INT16,INT32,REAL32
use, INTRINSIC :: iso_c_binding, ONLY  : c_int64_t
USE, INTRINSIC ::  ieee_arithmetic
USE M_color, ONLY : jucolor
IMPLICIT NONE

INTERFACE OPERATOR (.p.) ! binary operator summation convention/tensors
!   a .p. b returns scalar sum matrices; rank 0 of a(i,j)*b(i,j) a,b rank 2
!   a .p. b returns scalar sum of vectors; rank 0 of a(i)*b(i) a,b rank 1
!   a .p. b returns vector sum(j) rank 1 of a(i)*b(i,j) a,b rank 1,2
!   a .p. b returns vector sum(i) rank 1 of a(i,j)*b(j) a,b rank 2,1
 MODULE PROCEDURE sum_of_sum_matrix_by_matrix, sum_of_vector_by_matrix, &
                  sum_of_matrix_by_vector, sum_of_vector_by_vector
END INTERFACE

INTERFACE

  SUBROUTINE bsearch(r,rv,n,high,low)
   USE set_precision, ONLY  : wp
   INTEGER, INTENT(IN) :: n
   REAL(wp), INTENT(IN) :: r, rv(n)
   INTEGER, INTENT(OUT) :: high, low
  END SUBROUTINE

END INTERFACE

CONTAINS
 
! vector & matrix operations

! scalar matrix (inner) product
FUNCTION sum_of_sum_matrix_by_matrix(array1,array2) result(dL2)
 REAL (wp), INTENT (IN) :: array1(:,:),array2(:,:)
 REAL(wp) ::  v3(size(array1,1)),dL2
 INTEGER :: i
 v3=[(1,i=1,size(array1,1))]
 dL2=dot_product(v3,matmul(v3,array1*array2))
END FUNCTION sum_of_sum_matrix_by_matrix

! scalar vector (inner) product
FUNCTION sum_of_vector_by_vector(v1,v2) result(dL2)
 REAL (wp), INTENT (IN) :: v1(:),v2(:)
 REAL (wp) :: dL2
 dl2=dot_product(v1,v2)
END FUNCTION sum_of_vector_by_vector

! vector x matrix (inner) product
FUNCTION sum_of_vector_by_matrix(v1,array2) result(v2)
 REAL (wp), INTENT (IN) :: v1(:),array2(:,:)
 REAL (wp) :: v2(SIZE(array2,1))
 v2=matmul(v1,array2)
END FUNCTION sum_of_vector_by_matrix

! matrix x vector (inner) product
FUNCTION sum_of_matrix_by_vector(array1,v2) result(v1)
 REAL (wp), INTENT (IN) :: v2(:),array1(:,:)
 REAL (wp) :: v1(SIZE(array1,2))
 v1=matmul(array1,v2)
END FUNCTION sum_of_matrix_by_vector

!! REAL32 functions for STL facet calcs

! vector vector (cross) product (dimension 3)
FUNCTION cross_product(v1, v2) result(v3)
  REAL(wp), INTENT(IN) :: v1(3), v2(3)
  REAL(wp) :: v3(3)
  v3(1) = v1(2) * v2(3) - v1(3) * v2(2)
  v3(2) = v1(3) * v2(1) - v1(1) * v2(3)
  v3(3) = v1(1) * v2(2) - v1(2) * v2(1)
END FUNCTION cross_product

! surface normal vector (REAL32 and dimension 3)
FUNCTION surface_normal(v1, v2, v3) result(v4)
  REAL(REAL32), INTENT(IN) :: v1(3), v2(3), v3(3) ! vertices of triangle
  REAL(REAL32) :: v4(3)
  v4(1) = (v2(2)-v1(2)) * (v3(3)-v1(3)) - (v2(3)-v1(3)) * (v3(2)-v1(2))
  v4(2) = (v2(3)-v1(3)) * (v3(1)-v1(1)) - (v2(1)-v1(1)) * (v3(3)-v1(3))
  v4(3) = (v2(1)-v1(1)) * (v3(2)-v1(2)) - (v2(2)-v1(2)) * (v3(1)-v1(1))
END FUNCTION surface_normal

!! color functions

FUNCTION colormap(x,minimum, maximum,map) result(rgbv)
 REAL (wp), INTENT (IN) :: minimum,maximum,x
 INTEGER(c_int64_t), INTENT (IN) :: map
 INTEGER(int16) :: rgbv(3) ! rgbv={r,g,b}
 SELECT CASE (map)
   CASE (1)
      rgbv=rgb2(x,minimum,maximum)
   CASE (2)
      rgbv=rgb5(x,minimum,maximum)
   CASE (3)
      rgbv=hsbrgb(x,minimum,maximum)
   CASE (4)
      rgbv=gplotpalette(x,minimum,maximum)
   CASE (5)
      rgbv=USSpalette(.true.,.false.,x,minimum,maximum)
   CASE (6)
      rgbv=PerceptuallyUniformPalette(.true.,x,minimum,maximum)
   CASE (7)
      rgbv=USSpalette(.false.,.false.,x,minimum,maximum)
   CASE (8)
      rgbv=PerceptuallyUniformPalette(.false.,x,minimum,maximum)
   CASE (9)
      rgbv=USSpalette(.true.,.true.,x,minimum,maximum)
   CASE DEFAULT
      rgbv=USSpalette(.true.,.false.,x,minimum,maximum)
END SELECT
END FUNCTION colormap

! perceptually uniform but single hue sequential maps
FUNCTION PerceptuallyUniformPalette(fixedrange,x,powmin, powmax) result(rgbv)
REAL (wp), INTENT (IN) :: powmin,powmax,x
LOGICAL, INTENT(IN) :: fixedrange
INTEGER :: i,high,low
REAL (wp) :: col(9), minimum, maximum
INTEGER(int16), dimension(3,9) :: palette
INTEGER(int16) :: rgbv(3) ! rgbv={r,g,b}
!ANSI 38.5 to 49.5 every 0.5; Brewer 2.0 https://colorbrewer2.org no more than 9 classes max recommended
! but here we interpolate between them to avoid pixellation
if (fixedrange) then
 minimum =38.5
 maximum =49.5
else
 minimum=powmin
 maximum=powmax
endif
do i=1,9
 col(i)=maximum-((i-1)/8.0)*(maximum-minimum)
end do
palette=int(reshape((/&
255,255,217,&
237,248,177,&
199,233,180,&
127,205,187,&
65,182,196,&
29,145,192,&
34,94,168,&
37,52,148,&
8,29,88/),shape(palette)),kind=int3d)
call bsearch(x,col,9,high,low)
!Uncomment these and comment out the linear interpolation if you want to be confined to 9 classes with pixellation
!!if ( abs(col(high)-x) .lt. abs(x-col(low)) ) then
!! rgbv(:)=palette(:,high)
!!else
!! rgbv(:)=palette(:,low)
!!endif
! linear interpolation in rgb space
 if (high .ne. low) then
  rgbv(:)=int(abs(8.0*((x-col(low))*palette(:,high)+(col(high)-x)*palette(:,low))/(maximum-minimum)),kind=int3d)
 else
  rgbv(:)=int(palette(:,low),kind=int3d)
 endif
 ! need these if fixed range
  if (fixedrange) then
   if (x .lt. minimum) then
    rgbv(:)=int((/255,255,255/),kind=int3d)
   endif
   if (x .gt. maximum) then
    rgbv(:)=int((/0,0,0/),kind=int3d)
   endif
  endif
END FUNCTION PerceptuallyUniformPalette

! fixed discrete diopteric palette: The Uniform Standard Scale
FUNCTION USSpalette(fixedrange,extendedNIDEK,x,powmin, powmax) result(rgbv)
REAL (wp), INTENT (IN) :: powmin,powmax,x
LOGICAL, INTENT(IN) :: fixedrange,extendedNIDEK
INTEGER :: i,high,low
REAL (wp) :: col(26),minimum,maximum
INTEGER, dimension(3,26) :: palette
INTEGER(int16) :: rgbv(3) ! rgbv={r,g,b}
!Smolek et al Ophthalmology Feb 2002 Table 4. USS scale from 67.5 to 30 every 1.5 D
!simulated NIDEK extension 9 to 101.5, evenly spaced intervals, not exactly NIDEK scheme, not documented
if (fixedrange) then
 if (extendedNIDEK) then
  minimum =9
  maximum =101.5
 else
  minimum =30
  maximum =67.5
 endif
else
 minimum=powmin
 maximum=powmax
endif
 do i=1,26
  col(i)=maximum-((i-1)/25.0)*(maximum-minimum)
 end do
palette=reshape((/&
255, 238, 248, &
255, 217, 227, &
255, 197, 207, &
255, 176, 187, &
255, 158, 168, &
255, 138, 148, &
255, 115, 125, &
255, 95, 105,  &
255, 71, 80, &
255, 40, 50, &
255, 0, 0, &
255, 102, 0, &
252, 153, 0, &
252, 188, 0, &
255, 255, 0, &
162, 250, 59, &
80, 230, 51, &
51, 204, 51, &
32, 176, 72, &
0, 153, 102, &
0, 106, 157, &
0, 51, 204, &
0, 0, 204, &
0, 0, 153, &
0, 0, 112, &
0, 0, 80/),shape(palette))
call bsearch(x,col,26,high,low)
! linear interpolation in rgb space
 if (high .ne. low) then
  rgbv(:)=int(abs(25*((x-col(low))*palette(:,high)+(col(high)-x)*palette(:,low))/(maximum-minimum)),kind=int3d)
 else
  rgbv(:)=int(palette(:,low),kind=int3d)
 endif
! need these if fixed range
 if (fixedrange) then
  if (x .lt. minimum) then
   rgbv(:)=int((/255,255,255/),kind=int3d)
  endif
  if (x .gt. maximum) then
   rgbv(:)=int((/0,0,0/),kind=int3d)
  endif
 endif

END FUNCTION USSpalette

! makes a noncontinuous/discrete interval 12 color palette similar to the one in printgraph using gnuplot/splot
FUNCTION gplotpalette(x,minimum, maximum) result(rgbv)
REAL (wp), INTENT (IN) :: minimum,maximum,x
INTEGER :: high,low
INTEGER(int16) :: rgbv(3) ! rgbv={r,g,b}
CHARACTER(len=6) :: tempH
CHARACTER(6), dimension(12) :: palette
REAL (wp) :: col(12)
! https://stackoverflow.com/questions/54658674/gnuplot-apply-colornames-from-datafile/54659829#54659829
! these are gnuplot hexidecimal rgb representations of some of gnuplot's colors used in printgraph.f90:
!'purple','dark-blue','blue','light-blue','light-green','green','web-green','yellow','goldenrod','light-red','red'
palette=(/'c080ff','00008b','0000ff','add8e6','90ee90','00ff00',&
         &'00c000','ffff00','ffc020','f03232','ff0000','8b0000'/)
         col(1)=FLOOR(minimum+0.5)
         col(12)=FLOOR(maximum+0.5)
         col(2)=0.09*(col(12)-col(1))+col(1)
         col(3)=0.18*(col(12)-col(1))+col(1)
         col(4)=0.27*(col(12)-col(1))+col(1)
         col(5)=0.36*(col(12)-col(1))+col(1)
         col(6)=0.45*(col(12)-col(1))+col(1)
         col(7)=0.54*(col(12)-col(1))+col(1)
         col(8)=0.63*(col(12)-col(1))+col(1)
         col(9)=0.72*(col(12)-col(1))+col(1)
         col(10)=0.81*(col(12)-col(1))+col(1)
         col(11)=0.90*(col(12)-col(1))+col(1)
call bsearch(x,col,12,high,low)
! this is why it looks pixellated, no interpolation
if ( abs(col(high)-x) .lt. abs(x-col(low)) ) then
 tempH=palette(high)
else
 tempH=palette(low)
endif
! assumes hex colors are formatted 'rrggbb'
! converts character string hex to integer triplet values 0-255, 2 hex chars at a time
read(tempH(1:2),'(Z2)') rgbv(1)
read(tempH(3:4),'(Z2)') rgbv(2)
read(tempH(5:6),'(Z2)') rgbv(3)
END FUNCTION gplotpalette

! convert values to heatmap using Hue from Hue/Saturation/Value and color.f90
! input 3 scalars, output INTEGER(kind=2) vector
! using https://fortranwiki.org/fortran/show/M_color Color Library Version 5.0   
FUNCTION hsbrgb(x,minimum, maximum) result(rgbv)
 REAL (wp), INTENT (IN) :: minimum,maximum,x
 REAL :: hue,sat,bright,rr,gg,bb
 INTEGER :: stat
 INTEGER(int16) :: rgbv(3) ! rgbv={r,g,b}
    stat = 0
    hue = REAL(360*(maximum - x) / (maximum - minimum),kind=sk)
    sat=100.0 ; bright=100.0
    call jucolor('hsv',hue,sat,bright,'rgb',rr,gg,bb,stat)
    if (stat.ne.0) then
     rgbv=int((/255,255,255/),kind=int3d)  ! out of range or error = white
     write (*,*) 'Error in hsbrgb', hue,x,minimum,maximum
    endif
    rgbv(1) = int(min(255,int(2.55*rr)),kind=int3d)
    rgbv(2) = int(min(255,int(2.55*gg)),kind=int3d)
    rgbv(3) = int(min(255,int(2.55*bb)),kind=int3d)
END FUNCTION hsbrgb

! convert values to rgb 2 color (red to blue) heatmap
! input 3 scalars, output INTEGER(kind=2) vector
! https://stackoverflow.com/questions/20792445/calculate-rgb-value-for-a-range-of-values-to-create-heat-map    
FUNCTION rgb2(x,minimum, maximum) result(rgbv)
 REAL (wp), INTENT (IN) :: minimum,maximum,x
 REAL (wp) :: ratio
 INTEGER(int16) :: rgbv(3) ! rgbv={r,g,b}
 INTEGER(int16), PARAMETER :: zero=0
    ratio = 2 * (x-minimum) / (maximum - minimum)
    rgbv(3) = max(zero, int(255*(1 - ratio),2))
    rgbv(1) = max(zero, int(255*(ratio - 1),2))
    rgbv(2) = int(255,2) - rgbv(3) - rgbv(1)
END FUNCTION rgb2

! convert values to rgb 5 color (red to blue) heatmap
! input 3 scalars, output INTEGER(kind=2) vector
! http://www.andrewnoske.com/wiki/Code_-_heatmaps_and_color_gradients  
! https://stackoverflow.com/questions/3708307/how-to-initialize-two-dimensional-arrays-in-fortran 
FUNCTION rgb5(x,minimum, maximum) result(rgbv)
 REAL (wp), INTENT (IN) :: minimum,maximum,x
 REAL (wp) :: ratio,fract
 INTEGER(int16) :: nc,rgbv(3),idx1,idx2 ! rgbv={r,g,b}
! color={{0,0,255},{0,255,255},{0,255,0},{255,255,0},{255,0,0}}
 INTEGER(int16) :: color(3,5)=reshape( (/ 0_int16, 0_int16, 255_int16, &      !blue
                                          0_int16, 255_int16, 255_int16, &    !cyan
                                          0_int16, 255_int16, 0_int16, &      !green
                                          255_int16, 255_int16, 0_int16, &    !yellow
                                          255_int16, 0_int16, 0_int16 /), &   !red
                                          (/3,5/)  )
! color={{255,0,0},{255,255,0},{0,255,0},{0,255,255},{0,0,255}}
! INTEGER(int16) :: color(3,5)=reshape( (/              255, 0, 0, &      !red
!                                                       255, 255, 0, &    !yellow
!                                                       0, 255, 0, &      !green
!                                                       0, 255, 255, &    !cyan 
!                                                       0, 0, 255/), &    !blue                                                         
!                                           (/3,5/)  )
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
     idx1  = int(floor(ratio)+1,kind=int3d)                   ! Desired color will be after this index.
     idx2  = int(idx1+1,kind=int3d)                           ! ... and before this index (inclusive).
     fract = ratio - REAL(idx1)+1            ! Distance between the two indexes (0-1).
    endif
   endif

   if (idx1 == 0 .OR.  idx2 == 0) then
    write(*,*) 'Error: x,min,max,ratio: ',x,minimum,maximum,ratio
   endif
  
   rgbv(1) = int((color(1,idx2) - color(1,idx1))*fract + color(1,idx1),kind=int3d)
   rgbv(2) = int((color(2,idx2) - color(2,idx1))*fract + color(2,idx1),kind=int3d)
   rgbv(3) = int((color(3,idx2) - color(3,idx1))*fract + color(3,idx1),kind=int3d)
  else
   rgbv=(/255_int16,255_int16,255_int16/)  ! out of range = white
  endif
   
END FUNCTION rgb5

! Converts full RGB (256x256x256) to VisCAM/SolidView or Materials Magic 15 bit color attr or no color
FUNCTION rgb2attr(deftype,rgbv) result(attr)
  USE, INTRINSIC :: iso_c_binding, ONLY : c_int
  INTEGER(kind=2), INTENT(IN) :: rgbv(3)  !=INT16
  INTEGER(c_int), INTENT(IN) :: deftype
  INTEGER(INT16) :: attr
  INTEGER(INT8) :: red,green,blue
  red = int(rgbv(1)/8,kind=int2d)
  green = int(rgbv(2)/8,kind=int2d)
  blue =  int(rgbv(3)/8,kind=int2d)
!    color compatible with VisCAM and SolidView definition
!    bits 0 to 4 are the intensity level for blue (0 to 31),
!    bits 5 to 9 are the intensity level for green (0 to 31),
!    bits 10 to 14 are the intensity level for red (0 to 31),
!    bit 15 is 1 if the color is valid, or 0 if the color is not valid (as with normal STL files).
!    attr= b'0000001100000001'   ! 00000 01100 00000 1 == blue 0, green 12, red 0, valid
 if (deftype == 0) then
  attr=int(blue + 32*green + 1024*red, kind=int3d) ! packs the bits according to the scheme above
  attr=ibset(attr,15) ! sets position 15 to 1
 endif
 if (deftype == 1) then
!    color compatible with Materials Magic definition
!    bits 0 to 4 are the intensity level for red (0 to 31),
!    bits 5 to 9 are the intensity level for green (0 to 31),
!    bits 10 to 14 are the intensity level for blue (0 to 31),
!    bit 15 is 0 if the color is valid, or 1 if per object color is to be used
  attr=int(red + 32*green + 1024*blue, kind=int3d) ! packs the bits according to the scheme above
  attr=ibclr(attr,15) ! sets position 15 to 0
  endif
 if (deftype /= 1 .and. deftype /=0) then
  attr=0
 endif
!  write(*,*) rgbv
!  write(*,*) red,green,blue
!  write(*,'(b8.8)') red
!  write(*,'(b8.8)') green
!  write(*,'(b8.8)') blue
!  write(*,'(b16.16)') attr
END FUNCTION rgb2attr

!! coordinate transform
SUBROUTINE PolarTranslate(ctr_circle_x,ctr_circle_y,rlocal,tht_local,R_global,Theta_global)
    IMPLICIT NONE
    REAL (wp), INTENT (IN) ::  ctr_circle_x,ctr_circle_y,rlocal,tht_local
    REAL (wp), INTENT(OUT) :: R_global,Theta_global
    REAL (wp) :: X_global,Y_global
    X_global=(rlocal*cos(tht_local)-ctr_circle_x)
    Y_global=(rlocal*sin(tht_local)-ctr_circle_y)
    R_global=sqrt(X_global*X_global+Y_global*Y_global)
    if (ABS(X_global) > EPS .AND. ABS(Y_global) > EPS) then
     if (X_global > 0 .AND. Y_global > 0 ) then
      Theta_global=ATan(Y_global/X_global)
     endif
     if (X_global < 0 .AND. Y_global > 0 ) then
      Theta_global=ATan(Y_global/X_global)+PI
     endif
     if (X_global < 0 .AND. Y_global < 0 ) then
      Theta_global=ATan(Y_global/X_global)+PI
     endif
     if (X_global > 0 .AND. Y_global < 0 ) then
      Theta_global=ATan(Y_global/X_global)+2*PI
     endif
    else
     if (ABS(X_global) <= EPS .AND. ABS(Y_global) > EPS) then
      if (Y_global > 0) then
       Theta_global=PI/2
      else
       Theta_global=3*PI/2
      endif
     endif
     if (ABS(X_global) > EPS .AND. ABS(Y_global) <= EPS) then
      if (X_global > 0) then
       Theta_global=0
      else
       Theta_global=PI
      endif
     endif
    endif
END SUBROUTINE PolarTranslate


!! string/character functions
!!https://stackoverflow.com/questions/58938347/how-do-i-replace-a-character-in-the-string-with-another-charater-in-fortran

PURE RECURSIVE FUNCTION replacestr(string,search,substitute) result(modifiedString)
        IMPLICIT NONE
        CHARACTER(len=*), INTENT(IN)  :: string, search, substitute
        CHARACTER(len=:), allocatable :: modifiedString
        INTEGER                       :: i, stringLen, searchLen
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
                modifiedString = string(1:i-1) // substitute // replacestr(string(i+searchLen:stringLen),search,substitute)
                exit
            end if
            if (i+searchLen>stringLen) then
                modifiedString = string
                exit
            end if
            i = i + 1
            cycle
        end do
    END FUNCTION replacestr

!! epsilon & factorial functions
! needed for zernike functions
FUNCTION eps2(m) result(e) !eps2(0)=2, eps2(m)=1 m /=0
 INTEGER :: e
 INTEGER, INTENT(IN) :: m
 if (m == 0) then
  e=2
 else
  e=1
 endif    
END FUNCTION eps2

 RECURSIVE FUNCTION fact(n)  result(f) ! classic recursive factorial
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
END FUNCTION fact

 FUNCTION binomial(n,k)  result (m)
 INTEGER :: m
 INTEGER, INTENT(IN) :: n,k
! m = fact(n)/(fact(k)*fact(n-k)) ! inefficient
 if (k > (n-k)) then
   m = pfact(n,k)/pfact(n-k,0)
  else
   m = pfact(n,n-k)/pfact(k,0)
 endif
END FUNCTION binomial

 FUNCTION pfact(n,k)  result(f) ! partial factorial k+1 to n: pfact(n,1)=pfact(n,0)=fact(n)
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
END FUNCTION pfact

!! zernike radial functions

 FUNCTION R00(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=1
END FUNCTION R00

 FUNCTION R11(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=rho
END FUNCTION R11

 FUNCTION R20(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=2*rho*rho-1
END FUNCTION R20

 FUNCTION R22(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=rho*rho
END FUNCTION R22

 FUNCTION R31(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=3**rho*rho*rho-2*rho
END FUNCTION R31

 FUNCTION R33(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=rho*rho*rho
END FUNCTION R33

 FUNCTION R40(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=6*rho*rho*rho*rho-6*rho*rho+1
END FUNCTION R40

 FUNCTION R42(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=4**rho*rho*rho*rho-3*rho*rho
END FUNCTION R42

 FUNCTION R44(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=rho*rho*rho*rho
END FUNCTION R44

 FUNCTION R51(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=10*rho*rho*rho*rho*rho-12*rho*rho*rho+3*rho
END FUNCTION R51

 FUNCTION R53(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=5*rho*rho*rho*rho*rho-4*rho*rho*rho
END FUNCTION R53

 FUNCTION R55(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=rho*rho*rho*rho*rho
END FUNCTION R55

 FUNCTION R60(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=20*rho*rho*rho*rho*rho*rho-30*rho*rho*rho*rho+12*rho*rho-1
END FUNCTION R60

 FUNCTION R62(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=15*rho*rho*rho*rho*rho*rho-20*rho*rho*rho*rho+6*rho*rho
END FUNCTION R62

 FUNCTION R64(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=6*rho*rho*rho*rho*rho*rho-5*rho*rho*rho*rho
END FUNCTION R64

 FUNCTION R66(rho) result (m)
 REAL(wp) :: m
 REAL(wp),INTENT(IN) :: rho
 m=rho*rho*rho*rho*rho*rho
END FUNCTION R66

! if n >= m >= 0 n-m even ie mod(n-m)=0
 RECURSIVE FUNCTION Rzern(n,m,p)  result(f) ! radial zernike polynomial
 REAL(wp) :: f
 INTEGER, INTENT(IN) :: n,m
 REAL(wp),INTENT(IN) :: p
 if (n < m .OR. m < 0 .OR. mod(n-m,2) /= 0) then
  write(*,*) 'Illegal indices in zernike',n,m
  stop
 endif
 if ( n == m) then
  f=p**n
 else
 if (n > 4) then
   f = ( 2*(n-1)*(2*n*(n-2)*p*p-m*m-n*(n-2))*Rzern(n-2,m,p)-n*(n+m-2)*(n-m-2)*Rzern(n-4,m,p))/((n+m)*(n-m)*(n-2))
 else
  if (n == 2 .AND. m == 0) then
   f = R20(p)
  endif
  if (n == 3 .AND. m == 1) then
   f = R31(p)
  endif  
  if (n == 4 .AND. m == 0) then
   f = R40(p)
  endif
  if (n == 4 .AND. m == 2) then
   f = R42(p)
  endif
 endif
 endif
END FUNCTION Rzern

! if n >= 0 ABS(m) <= n
 FUNCTION zernfct(n,m,p,phi) result(f)
 INTEGER, INTENT(IN) :: n,m
 REAL(wp) :: f
 REAL(wp),INTENT(IN) :: p,phi
 if (m >= 0) then
  f = Rzern(n,m,p)*cos(m*phi)
 else
  f = Rzern(n,-m,p)*sin(m*phi)
 endif
END FUNCTION zernfct

END MODULE special_fct

