       subroutine WriteCenterJ(a,b,KXNAME)
       USE parameters, ONLY : PI
       USE set_precision, ONLY : wp
       use io_functions, only : get_new_fileunit
       real(wp),INTENT(IN) :: a, b(:,:)
       character(len=*), intent(in) :: KXNAME
       integer :: N,MM,i,unitno1
       N=size(b,1)-1
       MM=size(b,2)
       unitno1 = get_new_fileunit()
       open(unitno1, file=trim(KXNAME), action="write", iostat=ierr)
       if (ierr .ne. 0) write(*,*) 'WriteCenterJ cannot open',KXNAME
       do i=1,MM
        write(unitno1,*) PI*(i-1)/90.0_wp,(b(N+1,i)-a)
       end do
!      close the loop
       i=1
       write(unitno1,*) PI*(i-1)/90.0_wp,(b(N+1,i)-a)
       close (unitno1)       
    
       end subroutine WriteCenterJ
