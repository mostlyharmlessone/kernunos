    SUBROUTINE c_saxpy(n, sa, sx, incx, sy, incy) &
               BIND (C, NAME='C_saxpy')
      USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_float
      INTEGER (c_int), INTENT (IN) :: n, incx, incy
      REAL (c_float), INTENT (IN) :: sa, sx(*)
      REAL (c_float), INTENT (INOUT) :: sy(*)

      CALL saxpy(n, sa, sx, incx, sy, incy)
    END SUBROUTINE c_saxpy

    SUBROUTINE c_val_saxpy(n, sa, sx, incx, sy, incy) &
               BIND (C, NAME='C_val_saxpy')
      USE, INTRINSIC :: iso_c_binding, ONLY: c_int, c_float
      INTEGER (c_int), INTENT (IN), VALUE :: n, incx, incy
      REAL (c_float), INTENT (IN), VALUE :: sa
      REAL (c_float), INTENT (IN) :: sx(*)
      REAL (c_float), INTENT (INOUT) :: sy(*)

      CALL saxpy(n, sa, sx, incx, sy, incy)
    END SUBROUTINE c_val_saxpy
