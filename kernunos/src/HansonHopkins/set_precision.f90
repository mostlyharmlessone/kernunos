! modified from http://www.siam.org/books/ot134 Numerical Computing with Modern Fortran Richard J.Hanson and Tim Hopkins SIAM

! Module set_precision provides the kind type parameter needed
! to define the precision of a complete package along
! with values for all commonly used precisions
    MODULE set_precision
    USE ISO_FORTRAN_ENV, ONLY : int8, int16, int32, int64
    use iso_c_binding, ONLY : c_bool, c_int
    IMPLICIT NONE
    private
    public :: wp, sk, sp, dp, xdp, qp, int8, int16, int32, int64, lk, c_bool, skind, dkind, int2d, int3d, c_int
! ..

! .. Intrinsic Functions ..
      INTRINSIC KIND
! .. Parameters ..
! Define the standard precisions
! For IEEE standard arithmetic we could also use
!     INTEGER, PARAMETER :: skind = SELECTED_REAL_KIND(p=6, r=37)
!     INTEGER, PARAMETER :: dkind = SELECTED_REAL_KIND(p=15, r=307)
      INTEGER, PARAMETER :: skind = KIND(0.0E0)
      INTEGER, PARAMETER :: dkind = KIND(0.0D0)
      INTEGER, PARAMETER :: int2d = SELECTED_INT_KIND(2)
      INTEGER, PARAMETER :: int3d = SELECTED_INT_KIND(3)
! The next statement is required to run the codes
! associated with Chapter 10 on IEEE arithmetic.
! This is non-standard and may not be available.
! Different compilers set this value in different ways;
! see comments below for advice.
!     INTEGER, PARAMETER:: qkind = ...
! Set the precision for the whole package
      INTEGER, PARAMETER :: wp = dkind
      INTEGER, PARAMETER :: sk = skind

      INTEGER, parameter :: sp = selected_real_kind(6)
      INTEGER, parameter :: dp = selected_real_kind(15)
      INTEGER, parameter :: xdp = selected_real_kind(18)
      INTEGER, parameter :: qp = selected_real_kind(33)
      INTEGER, parameter :: lk = kind(.true.)


!-----------------------------------------------------------
! For the non-standard quadruple precision:
! IBM and Intel compilers recognize KIND(0.0Q0) 
!     INTEGER, PARAMETER:: qkind = KIND(0.0Q0)
!-----------------------------------------------------------
! If you are using the NAG compiler then you can
! make use of the NAG-supplied f90_kind module
!     USE, INTRINSIC :: f90_kind
!     INTEGER, PARAMETER:: skind = single ! single precision
!     INTEGER, PARAMETER:: dkind = double ! double precision
!     INTEGER, PARAMETER:: qkind = quad   ! quad   precision
! but note that this module is not part of the standard
! and is thus likely to be non-portable.
!-----------------------------------------------------------

    END MODULE set_precision
