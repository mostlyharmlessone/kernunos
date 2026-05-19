 MODULE parameters
! defines arrays and functions ued for corneal topography
 USE set_precision, ONLY : wp
 REAL(wp), PARAMETER :: PI=3.1415926535897932384626433832795_wp
 REAL(wp), PARAMETER :: RFCT=33750.0_wp
 REAL(wp), PARAMETER :: EPS=0.0001_wp  ! used in pspli,SplineCenter,corneal calc fcts
! INTEGER, PARAMETER :: NP=141         ! PentaCam
! INTEGER, PARAMETER :: MM=180, N=22   ! Atlas
! INTEGER, PARAMETER :: MM=360, N=16  ! EyeSys
 integer, PARAMETER :: M2=10 ! lsq fourier series terms; if even then there's an equal number of sine and cosine terms; don't make higher than 10 or get Gibb's phenomenon
! integer :: LWORK1
! real(wp), allocatable :: WORK1(:)
! natural spline; csr and LAPACK not superlu is fastest for these matrix sizes
 LOGICAL, PARAMETER :: periodic =.false. , csr = .true. , sparse = .false.
END MODULE