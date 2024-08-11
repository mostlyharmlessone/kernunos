       SUBROUTINE PRINTGRAPH(unitno1,POWMIN,POWMAX,FILENAME)
       use set_precision, only : wp
       REAL(wp), INTENT(IN) :: POWMIN, POWMAX
       integer, intent(in) :: unitno1
       character(len=*), intent(in) :: FILENAME
       real :: col(12)

800    FORMAT(A,F6.1,A,F6.1,A)
900    FORMAT(A,F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,&
     F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A,&
     F6.1,A,A,A,F6.1,A,A,A,F6.1,A,A,A) 

       col(1)=FLOOR(POWMIN)
       col(12)=FLOOR(POWMAX+4)
       col(2)=0.09*(col(12)-col(1))+col(1)
       col(3)=0.18*(col(12)-col(1))+col(1)
       col(4)=0.27*(col(12)-col(1))+col(1)
       col(5)=0.36*(col(12)-col(1))+col(1)
       col(6)=0.45*(col(12)-col(1))+col(1)
       col(7)=0.54*(col(12)-col(1))+col(1)
       col(8)=0.63*(col(12)-col(1))+col(1)
       col(9)=0.72*(col(12)-col(1))+col(1)
       col(10)=0.81*(col(12)-col(1))+col(1)
       col(11)=0.90*(col(12)-col(1))+col(1)
       
       WRITE(unitno1,*) 'set pm3d map impl'
       WRITE(unitno1,800) 'set zrange[',col(1),':',col(11),']'
       WRITE(unitno1,900) 'set palette defined (',col(1),"'",'purple',&
     "',",col(2),"'",'dark-blue',"',",col(3),"'",'blue',&
     "',",col(4),"'",'light-blue',"',",col(5),"'",'light-green',&
     "',",col(6),"'",'green',"',",col(7),"'",'web-green',&
     "',",col(8),"'",'yellow',"',",col(9),"'",'goldenrod',&
     "',",col(10),"'",'light-red',&
     "',",col(11),"'",'red',"',",col(12),"'",'dark-red',"')"
       WRITE(unitno1,*) '@NOXTICS ; @NOYTICS'
       WRITE(unitno1,*) 'splot ',"'",FILENAME,"'",'notitle'


write(*,*) 'powmin,powmax',powmin,powmax,col(1),col(12)

WRITE(*,900) 'set palette defined (',col(1),"'",'purple',&
"',",col(2),"'",'dark-blue',"',",col(3),"'",'blue',&
"',",col(4),"'",'light-blue',"',",col(5),"'",'light-green',&
"',",col(6),"'",'green',"',",col(7),"'",'web-green',&
"',",col(8),"'",'yellow',"',",col(9),"'",'goldenrod',&
"',",col(10),"'",'light-red',&
"',",col(11),"'",'red',"',",col(12),"'",'dark-red',"')"


       RETURN
       END
 

       
