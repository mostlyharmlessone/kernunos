MODULE sparsekit_test01_fcts

CONTAINS

FUNCTION afun ( x, y, z )

!*****************************************************************************80
!
!! AFUN
!
  IMPLICIT NONE

  REAL ( kind = 8 ) afun
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  afun = -1.0D+00

  return
end
FUNCTION bfun ( x, y, z )

!*****************************************************************************80
!
!! BFUN
!
  IMPLICIT NONE

  REAL ( kind = 8 ) bfun
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  bfun = -1.0D+00

  return
end
FUNCTION cfun ( x, y, z )

!*****************************************************************************80
!
!! CFUN
!
  IMPLICIT NONE

  REAL ( kind = 8 ) cfun
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  cfun = -1.0D+00

  return
end
FUNCTION dfun ( x, y, z )

!*****************************************************************************80
!
!! DFUN
!
  IMPLICIT NONE

  REAL ( kind = 8 ) dfun
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  dfun = 0.0D+00

  return
end
FUNCTION efun ( x, y, z )

!*****************************************************************************80
!
!! EFUN
!
  IMPLICIT NONE

  REAL ( kind = 8 ) efun
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  efun = 0.0D+00

  return
end
FUNCTION ffun ( x, y, z )

!*****************************************************************************80
!
!! FFUN
!
  IMPLICIT NONE

  REAL ( kind = 8 ) ffun
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  ffun = 0.0D+00

  return
end
FUNCTION gfun ( x, y, z )

!*****************************************************************************80
!
!! GFUN
!
  IMPLICIT NONE

  REAL ( kind = 8 ) gfun
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  gfun = 0.0D+00

  return
end
SUBROUTINE ope ( n, x, y, a, ja, ia )
 
!*****************************************************************************80
!
!! OPE computes A * x for a sparse matrix A.
!
  IMPLICIT NONE

  integer n

  REAL ( kind = 8 ) a(*)
  integer i
  integer ia(n+1)
  integer ja(*)
  integer k1
  integer k2
  REAL ( kind = 8 ) x(*)
  REAL ( kind = 8 ) y(*)
!
! sparse matrix * vector multiplication
!
  do i=1,n
    k1 = ia(i)
    k2 = ia(i+1) -1
    y(i) = dot_product ( a(k1:k2), x(ja(k1:k2)) )
  end do

  return
end
SUBROUTINE opet ( n, x, y, a, ja, ia )

!*****************************************************************************80
!
!! OPET computes A' * x for a sparse matrix A.
!
  IMPLICIT NONE

  REAL ( kind = 8 ) a(*)
  integer i
  integer ia(*)
  integer ja(*)
  integer k
  integer n
  REAL ( kind = 8 ) x(*)
  REAL ( kind = 8 ) y(*)
!
! sparse matrix * vector multiplication
!
  y(1:n) = 0.0D+00

  do  i=1,n
    do k=ia(i), ia(i+1)-1
      y(ja(k)) = y(ja(k)) + x(i)*a(k)
    end do
  end do

  return
end
SUBROUTINE ydfnorm ( n, y1, y )

!*****************************************************************************80
!
!! YDFNORM prints the L2 norm of the difference of two vectors.
!
  IMPLICIT NONE

  integer n

  REAL ( kind = 8 ) t
  REAL ( kind = 8 ) y(n)
  REAL ( kind = 8 ) y1(n)

  t = sqrt ( sum ( ( y(1:n) - y1(1:n) )**2 ) )
  write(*,*) '2-norm of error (exact answer-tested answer)=',t

  return
end
SUBROUTINE dump0 ( n, a, ja, ia )

!*****************************************************************************80
!
!! DUMP0
!
  IMPLICIT NONE

  REAL ( kind = 8 ) a(*)
  integer i
  integer ia(*)
  integer ja(*)
  integer n
  integer k
  integer k1
  integer k2

  do i = 1, n
    write(*,100) i
    k1=ia(i)
    k2 = ia(i+1)-1
    write(*,101) (ja(k),k=k1,k2)
    write(*,102) (a(k),k=k1,k2)
  end do

 100  format ('row :',i2,20(2h -))
 101  format('     column indices:',10i5)
 102  format('             values:',10f5.1)

  return
end
SUBROUTINE afunbl ( nfree, x, y, z, coeff )

!*****************************************************************************80
!
!! AFUNBL
!
  IMPLICIT NONE

  REAL ( kind = 8 ) coeff(100)
  integer i
  integer j
  integer nfree
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  do j=1, nfree
    do i=1, nfree
      coeff((j-1)*nfree+i) = 0.0D+00
    end do
    coeff((j-1)*nfree+j) = -1.0D+00
  end do

  return
end
SUBROUTINE bfunbl ( nfree, x, y, z, coeff )

!*****************************************************************************80
!
!! BFUNBL
!
  IMPLICIT NONE

  REAL ( kind = 8 ) coeff(100)
  integer i
  integer j
  integer nfree
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  do j=1, nfree
    do i=1, nfree
      coeff((j-1)*nfree+i) = 0.0D+00
    end do
    coeff((j-1)*nfree+j) = -1.0D+00
  end do

  return
end
SUBROUTINE cfunbl ( nfree, x, y, z, coeff )

!*****************************************************************************80
!
!! CFUNBL
!
  IMPLICIT NONE

  REAL ( kind = 8 ) coeff(100)
  integer i
  integer j
  integer nfree
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  do j=1, nfree
    do i=1, nfree
      coeff((j-1)*nfree+i) = 0.0D+00
    end do
    coeff((j-1)*nfree+j) = -1.0D+00
  end do

  return
end
SUBROUTINE dfunbl ( nfree, x, y, z, coeff )

!*****************************************************************************80
!
!! DFUNBL 
!
  IMPLICIT NONE

  REAL ( kind = 8 ) coeff(100)
  integer i
  integer j
  integer nfree
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  do j=1, nfree
    do i=1, nfree
      coeff((j-1)*nfree+i) = 0.0D+00
    end do
  end do

  return
end
SUBROUTINE efunbl ( nfree, x, y, z, coeff )

!*****************************************************************************80
!
!! EFUNBL
!
  IMPLICIT NONE

  REAL ( kind = 8 ) coeff(100)
  integer i
  integer j
  integer nfree
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  do j=1, nfree
    do i=1, nfree
      coeff((j-1)*nfree+i) = 0.0D+00
    end do
  end do

  return
end
SUBROUTINE ffunbl ( nfree, x, y, z, coeff )

!*****************************************************************************80
!
!! FFUNBL
!
  IMPLICIT NONE

  REAL ( kind = 8 ) coeff(100)
  integer i
  integer j
  integer nfree
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  do j=1, nfree
    do i=1, nfree
      coeff((j-1)*nfree+i) = 0.0D+00
    end do
  end do

  return
end
SUBROUTINE gfunbl ( nfree, x, y, z, coeff )

!*****************************************************************************80
!
!! GFUNBL
!
  IMPLICIT NONE

  REAL ( kind = 8 ) coeff(100)
  integer i
  integer j
  integer nfree
  REAL ( kind = 8 ) x
  REAL ( kind = 8 ) y
  REAL ( kind = 8 ) z

  do j=1, nfree
    do i=1, nfree
      coeff((j-1)*nfree+i) = 0.0D+00
    end do
  end do

  return
end
SUBROUTINE xyk ( nel, xyke, x, y, ijk, node )

!*****************************************************************************80
!
!!  XYK evaluates the material property function xyk
!
!  Discussion:
!
!    In this version of the routine, the matrix returned is the identity matrix.
!
  IMPLICIT NONE

  integer node

  integer ijk(node,*)
  integer nel
  REAL ( kind = 8 ) x(*)
  REAL ( kind = 8 ) xyke(2,2)
  REAL ( kind = 8 ) y(*)

  xyke(1,1) = 1.0D+00
  xyke(2,2) = 1.0D+00
  xyke(1,2) = 0.0D+00
  xyke(2,1) = 0.0D+00

  return
end

END MODULE
