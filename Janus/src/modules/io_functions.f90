module io_functions
! module for opening files sanely

   INTERFACE

    subroutine ConvertOFFtoSTL(OFFNAME,STLNAME,STLBINNAME) 
     use special_fct, only : surface_normal,rgb2attr
      use ISO_FORTRAN_ENV, only: INT8,INT16,INT32,REAL32
     character(len=*), intent(in) :: OFFNAME,STLNAME,STLBINNAME
    end subroutine

    SUBROUTINE fillarray(IuseG,KX1,POWMIN,POWMAX)
!     COMPUTES ATLAS DATA 
!     IuseG to select what to place in RadSlope%Zp AND/OR compute LIOC
      USE cornea_arrays, ONLY : RadSlope,AxialP,sagc2,instantp,meanp,mongea,lioc
      USE set_precision, ONLY : wp
      USE spline_interfaces, ONLY : SplineEval1Dx1D
      use,intrinsic :: ieee_arithmetic
      integer, intent(in) :: IuseG 
      character(len=*), intent(in) :: KX1     
      real(wp), intent(out) :: POWMIN, POWMAX
    END SUBROUTINE

    SUBROUTINE Geom(b, powmin, powmax, elements, vertices, nV, nE)
       use cornea_arrays
       use set_precision, ONLY : wp
       use c_interfaces, ONLY : OpenGL_Show
       use special_fct, only : rgb2, rgb5
       use, intrinsic :: iso_c_binding, ONLY : c_float,c_int
       use ISO_FORTRAN_ENV, only: stdin=>input_unit     
       TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
       real(wp), intent(IN) :: powmin,powmax 
       integer(c_int), INTENT(INOUT) :: elements(*)                          ! faces x 3   index 0
       real(c_float), INTENT(INOUT) :: vertices(*)                           ! vertices x 6
       integer(c_int), INTENT(INOUT) :: nE, nV
    END SUBROUTINE

    subroutine rcnvrta(KXNAME)
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : Atlas, PI
     character(len=*), intent(in) :: KXNAME
    end subroutine

    subroutine rcnvrte(RANAME,XXNAME)
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : EyeSys,Atlas
     character(len=*), intent(in) :: RANAME,XXNAME 
    end subroutine

    subroutine rcnvrtp(filenameE,filenameC)
     USE cornea_arrays, ONLY : Penta
     character(len=*), intent(in) :: filenameE,filenameC
    end subroutine

    subroutine RCNVRTT(MM,N,NP)
     USE set_precision, ONLY : wp
     USE cornea_arrays
     INTEGER, INTENT(IN) :: MM,N,NP
    end subroutine

    SUBROUTINE WriteGeom(b,powmin,powmax,OFFNAME,PLYNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
      character(len=*), intent(in) :: OFFNAME,PLYNAME 
      real(wp), intent(IN) :: powmin,powmax 
    END SUBROUTINE  
    
    subroutine WriteCenter(b,KXNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      TYPE(wpRadSlopeMatrix),INTENT(IN) :: b 
      character(len=*), intent(in) :: KXNAME   
    end subroutine      

    SUBROUTINE WRITEARRAY(b,KXNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
      character(len=*), intent(in) :: KXNAME 
    END SUBROUTINE

    SUBROUTINE PRINTGRAPH(POWMIN,POWMAX,FILENAME)
     use set_precision, only : wp
     REAL(wp), INTENT(IN) :: POWMIN, POWMAX
     character(len=*), intent(in) :: FILENAME
    END SUBROUTINE
    
  END INTERFACE
 
 contains
  
 function get_new_fileunit() result (f)
 implicit none
 logical :: op
 integer :: f
 f = 1
 do
  inquire(f,opened=op)
  if (op .eqv. .false.) exit
  f = f + 1
 end do
 end function
  
end module io_functions
