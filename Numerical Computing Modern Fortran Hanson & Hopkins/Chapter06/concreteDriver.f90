      PROGRAM driver
      USE set_precision, ONLY: wp
      USE concrete, ONLY : dpdata
      USE sortElements, ONLY : qsort
      TYPE(dpdata) :: datavals
      INTEGER :: n
      n = 10
      datavals%left = 1
      datavals%right = n
      ALLOCATE (datavals%items(n))
      datavals%items(1:n) = [5.0e0_wp, 4.0e0_wp, 3.0e0_wp, 2.0e0_wp, &
                             1.0e0_wp, 10.0e0_wp, 9.0e0_wp, 8.0e0_wp, &
                             7.0e0_wp, 6.0e0_wp]
      WRITE(*,'(''Before sorting: '')')
      WRITE(*,'(5e16.8)')datavals%items(1:n)
      WRITE(*,'(''-----------------------------------'')')
      CALL qsort(datavals)
      WRITE(*,'(''After sorting: '')')
      WRITE(*,'(5e16.8)')datavals%items(1:n)
      END
