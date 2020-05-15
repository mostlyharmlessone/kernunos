program f_call_c

  integer :: i=1, ierr
  double precision :: x=5
  double precision y
  print *, "Fortran calling C, passing"
  print *, "i=",i,"x=",x

  y = gsl_sf_bessel_J0 (x)
  print *, x,y
end program f_call_c
