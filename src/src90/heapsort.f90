! https://stdlib.fortran-lang.org/page/specs/stdlib_sorting.html#sort_index-creates-an-array-of-sorting-indices-for-an-input-array-while-also-sorting-the-array

 SUBROUTINE heapsort(RealVector,IndexVector,n)
  USE set_precision, ONLY : wp
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  REAL(wp), INTENT(INOUT) :: RealVector(n)
  INTEGER, INTENT(INOUT) :: IndexVector(n)
  INTEGER :: i,j,m,counter,newIndex
  REAL(wp) :: newReal
  m=n/2+1
  counter=n
  do
    if (m > 1) then
     m=m-1
     newReal=RealVector(m)
     newIndex=IndexVector(m)
    else
     newReal=RealVector(counter)
     newIndex=IndexVector(counter)
     RealVector(counter)=RealVector(1)
     IndexVector(counter)=IndexVector(1)
     counter=counter-1
     if (counter == 1) then
      RealVector(1)=newReal
      IndexVector(1)=newIndex
      return
     endif
    endif
    i=m
    j=2*m

    do while (j <= counter)
     if (j < counter ) then
      if (RealVector(j) < RealVector(j+1)) j=j+1
     endif
     if (newReal < RealVector(j)) then
      RealVector(i)=RealVector(j)
      IndexVector(i)=IndexVector(j)
      i=j
      j=2*j
     else
      j=counter+1
     endif
    end do
    RealVector(i)=newReal
    IndexVector(i)=newIndex
  end do

 END SUBROUTINE heapsort
