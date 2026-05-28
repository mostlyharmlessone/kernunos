      SUBROUTINE WriteCenterJ(a,b,KXNAME)
       USE parameters, ONLY : PI
       USE set_precision, ONLY : wp
       USE io_functions, ONLY  : get_new_fileunit
       REAL(wp),INTENT(IN) :: a, b(:,:)
       CHARACTER(len=*), INTENT(IN) :: KXNAME
       INTEGER :: N,MM,i,unitno1
       N=size(b,1)-1
       MM=size(b,2)
       unitno1 = get_new_fileunit()
       open(unitno1, file=trim(KXNAME), action="write", iostat=ierr)
       if (ierr .ne. 0) then
        write(*,*) 'WriteCenterJ cannot open',KXNAME
        return
       else
        do i=1,MM
         write(unitno1,*) PI*(i-1)/90.0_wp,(b(N+1,i)-a)
        end do
!       close the loop
        i=1
        write(unitno1,*) PI*(i-1)/90.0_wp,(b(N+1,i)-a)
        close (unitno1)
       endif
      END SUBROUTINE WriteCenterJ
