       SUBROUTINE PRINTGRAPH(POWMIN,POWMAX,FILENAME)
       use set_precision, only : wp
       REAL(wp), INTENT(IN) :: POWMIN, POWMAX
       character(len=*), intent(in) :: FILENAME

800    FORMAT(A,F6.1,A,F6.1,A)
900    FORMAT(A,F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,
     &F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,
     &F6.1,A,A,A,F6.1,A,A,A) 

       col1=POWMIN-2
       col11=POWMAX+3
       col2=0.1*(col11-col1)+col1
       col3=0.2*(col11-col1)+col1
       col4=0.3*(col11-col1)+col1
       col5=0.4*(col11-col1)+col1
       col6=0.5*(col11-col1)+col1
       col7=0.6*(col11-col1)+col1
       col8=0.7*(col11-col1)+col1
       col9=0.8*(col11-col1)+col1
       col10=0.9*(col11-col1)+col1
       
       WRITE(17,*) 'set pm3d map impl'
       WRITE(17,800) 'set zrange[',col1,':',col11,']'
       WRITE(17,900) 'set palette defined (',col1,"'",'purple',
     &"',",col2,"'",'dark-blue',"',",col3,"'",'blue',
     &"',",col4,"'",'light-blue',"',",col5,"'",'light-green',
     &"',",col6,"'",'green',"',",col7,"'",'web-green',
     &"',",col8,"'",'yellow',"',",col9,"'",'goldenrod',
     &"',",col10,"'",'red',"',",col11,"'",'dark-red',"')"
       WRITE(17,*) '@NOXTICS ; @NOYTICS'
       WRITE(17,*) 'splot ',"'",FILENAME,"'"

       RETURN
       END
 

       
