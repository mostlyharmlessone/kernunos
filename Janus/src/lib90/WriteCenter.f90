       subroutine WriteCenter(b,KXNAME)
       USE cornea_arrays
       USE set_precision, ONLY : wp
       use io_functions, only : get_new_fileunit
       TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
       character(len=*), intent(in) :: KXNAME
       integer :: MM,i,unitno1
       MM=size(b%r,2)
       unitno1 = get_new_fileunit()
       open(unitno1, file=trim(KXNAME), action="write", iostat=ierr)            
       do i=1,MM
        write(12,*) RadSlope%thta(i),RadSplineCenter(i)
       end do
       close (unitno1)       
    
       end subroutine WriteCenter
       
