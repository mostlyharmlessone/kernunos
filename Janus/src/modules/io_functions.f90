module io_functions
! module for opening files sanely

   INTERFACE
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

    SUBROUTINE Geom(b, powmin, powmax)
       use cornea_arrays
       use set_precision, ONLY : wp
       use c_interfaces, ONLY : OpenGL_Show
       use special_fct, only : rgb2, rgb5
       use, intrinsic :: iso_c_binding, ONLY : c_float,c_int
       use ISO_FORTRAN_ENV, only: stdin=>input_unit     
       TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
       real(wp), intent(IN) :: powmin,powmax 
    END SUBROUTINE

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
