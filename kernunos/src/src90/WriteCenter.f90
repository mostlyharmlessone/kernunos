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
         write(unitno1,*) RadSlope%thta(i),RadSplineCenter(1,i)
        end do
!       close the circle
        write(unitno1,*) RadSlope%thta(1),RadSplineCenter(1,1)
       close (unitno1)       
    
       end subroutine WriteCenter
       
