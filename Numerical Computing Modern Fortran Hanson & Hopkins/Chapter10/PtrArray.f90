      SUBROUTINE PtrArray(cptr, ncptr, sizes)  &
                 BIND(C, NAME='PtrArray')
! Code to illustrate the interoperability of a C array
! of pointers to one-dimensional arrays. cptr is the array
! of ncptr pointers. The length of the array pointed at by
! cptr(i) is sizes(i)
      USE, INTRINSIC :: iso_c_binding, ONLY : c_int, c_ptr,  &
         c_double, c_f_pointer

      INTEGER(c_int), INTENT(IN) :: ncptr
      INTEGER(c_int), INTENT(IN) :: sizes(ncptr)
      TYPE(c_ptr) :: cptr(ncptr)
      TYPE rows
        REAL(c_double), DIMENSION(:), POINTER :: r
      END TYPE rows
      TYPE(rows),DIMENSION(ncptr) :: carray
      
        DO i = 1, ncptr
! carray(i) % r will contain the data from the array pointed at
! by cptr(i).
          ALLOCATE(carray(i) % r(1:sizes(i)))
          CALL c_f_pointer(cptr(i), carray(i) % r, [sizes(i)])
        END DO

! Just print out the contents
        DO i = 1, ncptr
           WRITE(*, '(''row '',i3,'' length: '',i3)')i, sizes(i)
           WRITE(*, '(''          '',5e14.6)') carray(i)%r(1:sizes(i))
        END DO

      END SUBROUTINE PtrArray

