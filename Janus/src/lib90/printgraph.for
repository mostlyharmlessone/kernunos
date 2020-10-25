       SUBROUTINE PRINTGRAPH(POWMIN,POWMAX,FILENAME)
       use set_precision, only : wp
       REAL(wp), INTENT(IN) :: POWMIN, POWMAX
       character(len=*), intent(in) :: FILENAME

800    FORMAT(A,F6.1,A,F6.1,A)
900    FORMAT(A,F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,
     &F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,
     &F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A) 

       col1=FLOOR(POWMIN)
       col12=FLOOR(POWMAX+4)
       col2=0.09*(col12-col1)+col1
       col3=0.18*(col12-col1)+col1
       col4=0.27*(col12-col1)+col1
       col5=0.36*(col12-col1)+col1
       col6=0.45*(col12-col1)+col1
       col7=0.54*(col12-col1)+col1
       col8=0.63*(col12-col1)+col1
       col9=0.72*(col12-col1)+col1
       col10=0.81*(col12-col1)+col1
       col11=0.90*(col12-col1)+col1      
       
       WRITE(17,*) 'set pm3d map impl'
       WRITE(17,800) 'set zrange[',col1,':',col11,']'
       WRITE(17,900) 'set palette defined (',col1,"'",'purple',
     &"',",col2,"'",'dark-blue',"',",col3,"'",'blue',
     &"',",col4,"'",'light-blue',"',",col5,"'",'light-green',
     &"',",col6,"'",'green',"',",col7,"'",'web-green',
     &"',",col8,"'",'yellow',"',",col9,"'",'goldenrod',
     &"',",col10,"'",'light-red',     
     &"',",col11,"'",'red',"',",col12,"'",'dark-red',"')"
       WRITE(17,*) '@NOXTICS ; @NOYTICS'
       WRITE(17,*) 'splot ',"'",FILENAME,"'"

       RETURN
       END
 

       
