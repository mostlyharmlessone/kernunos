!      Generates legend for a colormap

       subroutine makelegend(flag, powmin, powmax, legend, nL)
       use set_precision, ONLY : wp
       use special_fct, only : colormap
       use, intrinsic :: iso_c_binding, ONLY : c_float,c_int, c_int64_t
       use, intrinsic ::  ieee_arithmetic
       use ISO_FORTRAN_ENV, only: stdin=>input_unit     ! for the pause read(stdin,*)    
       real(wp), intent(INOUT) :: powmin,powmax
       real(c_float) :: c_pow,c_rgbv(3)
       integer :: k
       real(wp) :: pow,maximum,minimum
       real(c_float), INTENT(INOUT) :: legend(*)
       integer(c_int64_t), INTENT(INOUT) :: flag
       integer(c_int64_t) :: map
       integer(c_int), INTENT(INOUT) :: nL

       map=mod((flag-mod(flag,100))/100,100)

       SELECT CASE (map)
       CASE (1)
        minimum=powmin
        maximum=powmax
       CASE (2)
        minimum=powmin
        maximum=powmax
       CASE (3)
        minimum=powmin
        maximum=powmax
       CASE (4)
        minimum=powmin
        maximum=powmax
       CASE (5)
        minimum =30
        maximum =67.5
       CASE (6)
        minimum =38.5
        maximum =49.5
       CASE (7)
        minimum=powmin
        maximum=powmax
       CASE (8)
        minimum=powmin
        maximum=powmax
        CASE (9)
         minimum =9
         maximum =101.5
       CASE DEFAULT
        minimum =30
        maximum =67.5
       END SELECT

!       Evenly placed steps; if there are uneven steps with peripheral extensions eg, this needs modifications
        do k=1,nL/4
         pow=maximum-(maximum-minimum)*(k-1)/(nL/4-1)
         if (ieee_is_finite(pow)) then         
          c_rgbv=colormap(pow,minimum,maximum,map)  !0-255 scale
         else
          c_rgbv = (/255,255,255/)
         endif                           
         c_pow=real(pow,kind=4)  ! explicitly cast to kind=4 for consistent with c_float
         legend(4*k-3:4*k)=(/c_pow,c_rgbv/)
         end do

       end subroutine makelegend

