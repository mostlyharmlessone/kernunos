       subroutine WriteCenter(b,KXNAME)
       USE cornea_arrays
       USE set_precision, ONLY : wp
       TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
       character(len=*), intent(in) :: KXNAME
       integer :: MM,i
       MM=size(b%r,2)             
       open (UNIT = 12, FILE = KXNAME)
       do i=1,MM
        write(12,*) RadSlope%thta(i),RadSplineCenter(i)
       end do
       close (12)       
    
       end subroutine WriteCenter
       
