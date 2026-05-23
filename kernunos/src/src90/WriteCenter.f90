       SUBROUTINE WriteCenter(b,KXNAME)
       USE cornea_arrays
       USE set_precision, ONLY : wp
       USE io_functions, ONLY  : get_new_fileunit
       TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
       CHARACTER(len=*), INTENT(IN) :: KXNAME
       INTEGER :: MM,i,unitno1
       MM=size(b%r,2)
       unitno1 = get_new_fileunit()
       open(unitno1, file=trim(KXNAME), action="write", iostat=ierr)
        do i=1,MM
         write(unitno1,*) RadSlope%thta(i),RadSplineCenter(1,i)
        end do
!       close the circle
        write(unitno1,*) RadSlope%thta(1),RadSplineCenter(1,1)
       close (unitno1)       
    
       END SUBROUTINE WriteCenter
       
