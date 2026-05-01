subroutine LSQEval(M2,c,v,f,ft,ftt,fttt)
 USE set_precision, ONLY : wp
 INTEGER, intent(in) :: M2
 REAL(wp), intent(in) :: v,c(M2)
 REAL(wp),INTENT(OUT),OPTIONAL :: f,ft,ftt,fttt
 integer :: j
! initialize
 if (Present(f)) f=0
 if (Present(ft)) ft=0
 if (Present(ftt)) ftt=0
 if (Present(fttt)) fttt=0
! series and derivatives
 do j=1,M2/2 ! cosine series
  if (Present(f)) f=f+c(j)*cos((j-1)*v)
  if (Present(ft)) ft=ft-c(j)*sin((j-1)*v)*(j-1)
  if (Present(ftt)) ftt=ftt-c(j)*cos((j-1)*v)*(j-1)*(j-1)
  if (Present(fttt)) fttt=fttt+c(j)*sin((j-1)*v)*(j-1)*(j-1)*(j-1)
 end do
 do j=M2/2+1,M2  ! sine series
  if (Present(f)) f=f+c(j)*sin((j-M2/2)*v)
  if (Present(ft)) ft=ft+c(j)*cos((j-M2/2)*v)*(j-M2/2)
  if (Present(ftt)) ftt=ftt-c(j)*sin((j-M2/2)*v)*(j-M2/2)*(j-M2/2)
  if (Present(fttt)) fttt=fttt-c(j)*cos((j-M2/2)*v)*(j-M2/2)*(j-M2/2)*(j-M2/2)
 end do
end subroutine
