module io_functions
! module for opening files sanely

   INTERFACE

    subroutine ConvertOFFtoSTL(OFFNAME,STLNAME,STLBINNAME) 
     use special_fct, only : surface_normal,rgb2attr
      use ISO_FORTRAN_ENV, only: INT8,INT16,INT32,REAL32
     character(len=*), intent(in) :: OFFNAME,STLNAME,STLBINNAME
    end subroutine

    SUBROUTINE fillarray(IuseG,KX1,POWMIN,POWMAX)
!     COMPUTES ATLAS DATA 
!     IuseG to select what to place in RadSlope%Zp AND/OR compute LIOC
      USE cornea_arrays, ONLY : RadSlope,AxialP,sagc2,instantp,meanp,mongea,lioc
      USE set_precision, ONLY : wp
      USE spline_interfaces, ONLY : SplineEval1Dx1D
      use,intrinsic :: ieee_arithmetic
      integer, intent(in) :: IuseG 
      character(len=*), intent(in) :: KX1     
      real(wp), intent(out) :: POWMIN, POWMAX
    END SUBROUTINE

    SUBROUTINE Geom(flag, b, donut, powmin, powmax, elements, vertices, nV, nE)
       use cornea_arrays
       use set_precision, ONLY : wp
       use c_interfaces, ONLY : OpenGL_Show
       use special_fct, only : rgb2, rgb5
       use, intrinsic :: iso_c_binding, ONLY : c_float,c_int
       use ISO_FORTRAN_ENV, only: stdin=>input_unit     
       TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
       logical, intent(IN) :: donut       
       real(wp), intent(INOUT) :: powmin,powmax 
       integer(c_int), INTENT(INOUT) :: elements(*)                          ! faces x 3   index 0
       real(c_float), INTENT(INOUT) :: vertices(*)                           ! vertices x 6
       integer(c_int), INTENT(INOUT) :: flag, nE, nV                         ! passed from janus to call OpenGL
    END SUBROUTINE

    subroutine rcnvrta(KXNAME)
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : Atlas, PI
     character(len=*), intent(in) :: KXNAME
    end subroutine

    subroutine rcnvrte(RANAME,XXNAME)
     USE set_precision, ONLY : wp
     USE cornea_arrays, ONLY : EyeSys
     character(len=*), intent(in) :: RANAME,XXNAME 
    end subroutine

    subroutine rcnvrtp(filenameE,filenameC)
     USE cornea_arrays, ONLY : Penta
     character(len=*), intent(in) :: filenameE,filenameC
    end subroutine

    subroutine RCNVRTT(MM,N,NP)
     USE set_precision, ONLY : wp
     USE cornea_arrays
     INTEGER, INTENT(IN) :: MM,N,NP
    end subroutine

    SUBROUTINE WriteGeom(b,donut,powmin,powmax,OFFNAME,PLYNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      TYPE(wpJMatrix),INTENT(IN) :: b
      character(len=*), intent(in) :: OFFNAME,PLYNAME
      real(wp), intent(IN) :: powmin,powmax
      logical, intent(IN) :: donut   
    END SUBROUTINE  
    
    subroutine WriteCenter(b,KXNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      TYPE(wpRadSlopeMatrix),INTENT(IN) :: b 
      character(len=*), intent(in) :: KXNAME   
    end subroutine      

    SUBROUTINE WRITEARRAY(b,KXNAME)
      USE cornea_arrays
      USE set_precision, ONLY : wp
      TYPE(wpRadSlopeMatrix),INTENT(IN) :: b
      character(len=*), intent(in) :: KXNAME 
    END SUBROUTINE

    SUBROUTINE PRINTGRAPH(POWMIN,POWMAX,FILENAME)
     use set_precision, only : wp
     REAL(wp), INTENT(IN) :: POWMIN, POWMAX
     character(len=*), intent(in) :: FILENAME
    END SUBROUTINE
    
  END INTERFACE
 
 contains
  
 function get_new_fileunit() result (f)
 implicit none
 logical :: op
 integer :: f
 f = 1
 do
  inquire(f,opened=op)
  if (op .eqv. .false.) exit
  f = f + 1
 end do
 end function
  
end module io_functions

subroutine rcnvrtp(filenameE,filenameC)
! PENTACAM VERSION
 use io_functions, only : get_new_fileunit
 USE cornea_arrays, ONLY : Penta
 implicit none
 character(len=*), intent(in) :: filenameE,filenameC
 integer :: unitno1, unitno2, ierr, readerr,i,k,NP, read_front
 logical :: exists
 character(len=7) :: matrixchar
 character(len=1) :: iter1,equal
 character(len=2) :: iter2
 character(len=3) :: iter3
 character(len=1000) :: somecharacter
 NP=141
 inquire(file=trim(filenameE), exist=exists)
 if (exists) then
   unitno1 = get_new_fileunit()
   open(unitno1, file=trim(filenameE), action="read", iostat=ierr)
     if (ierr .eq. 0) then 
    inquire(file=trim(filenameC), exist=exists)
    if (exists) then
     unitno2 = get_new_fileunit()
     open(unitno2, file=trim(filenameC), action="read", iostat=ierr)
     if (ierr .eq. 0) then
     read_front=0
     do
      read(unitno2, '(A)', iostat=readerr) somecharacter
         if (readerr .eq. 0) then
           if (somecharacter .eq. "Matrixsize Y=141" .and. read_front .eq. 0) then
            k=0 ; read_front=1  ! only read the front curvatures
           do
            k=k+1
            if (k <= 10 ) then
             read(unitno2,'(A,A,A,A)',iostat=readerr) matrixchar,iter1,equal,somecharacter
            endif
            if (k <= 100 .AND. k > 10 ) then           
               read(unitno2,'(A,A,A,A)',iostat=readerr) matrixchar,iter2,equal,somecharacter  
            endif
            if ( k > 100 .AND. k <= NP ) then
               read(unitno2,'(A,A,A,A)',iostat=readerr) matrixchar,iter3,equal,somecharacter
            endif
            if (k < NP ) then
               if (readerr .eq. 0) then  ! reads till end of data matches
                 read (somecharacter,*) (Penta%CUR(k,i),i=1,NP) 
               endif  
             else
                  write(*,*) 'Read ',k,' rows from ',trim(filenameC)
!                  do k=1,NP
!                   write (*,*) 'Matrix ',k-1,'= ',Penta%CUR(:,k)
!                  end do                                
!                 stop 
               exit  ! End of data         
             endif        
           end do 
           endif
         else                       
           exit  !EOF
         endif        
      end do  
      close(unitno2) 
      else
         print*, "Error ", ierr ," attempting to open file ", trim(filenameC)
        stop
    endif
    else
     print*, "Error -- cannot find file: ", trim(filenameC)
     stop
   endif
   read_front=0
   do
    read(unitno1, '(A)', iostat=readerr) somecharacter
       if (readerr .eq. 0) then 
         if (somecharacter .eq. "Matrixsize Y=141" .and. read_front .eq. 0) then
!           print*, "Char in file ", trim(filenameE), " is ", somecharacter
            k=0 ; read_front=1   ! only read the front elevations
           do
            k=k+1
            if (k <= 10 ) then
             read(unitno1,'(A,A,A,A)',iostat=readerr) matrixchar,iter1,equal,somecharacter
            endif
            if (k <= 100 .AND. k > 10 ) then           
               read(unitno1,'(A,A,A,A)',iostat=readerr) matrixchar,iter2,equal,somecharacter  
            endif
            if ( k > 100 .AND. k <= NP ) then
               read(unitno1,'(A,A,A,A)',iostat=readerr) matrixchar,iter3,equal,somecharacter
            endif
            if (k < NP ) then
               if (readerr .eq. 0) then  ! reads till end of data matches
                 read (somecharacter,*) (Penta%ELE(k,i),i=1,NP) 
               endif  
             else
                 write(*,*) 'Read ',k,' rows from ', trim(filenameE)
!                  do k=1,NP
!                   write (*,*) 'Matrix ',k-1,'= ',Penta%ELE(:,k)
!                  end do                                
!                 stop
               exit             
             endif        
           end do
           endif
         else            
           exit   !EOF
         endif        
      end do  
      close(unitno1) 
      else
         print*, "Error ", ierr ," attempting to open file ", trim(filenameE)
        stop
    endif
    else
     print*, "Error -- cannot find file: ", trim(filenameE)
     stop
   endif
end subroutine rcnvrtp

subroutine rcnvrte(RANAME,XXNAME)
! EYESYS VERSION
  use io_functions, only : get_new_fileunit
  USE set_precision, ONLY : wp
  USE cornea_arrays, ONLY : EyeSys
  implicit none
  logical :: exists
  character(len=*), intent(in) :: RANAME,XXNAME  
  REAL(wp) :: ZX(16),YX(16)
  INTEGER :: I,J,ITH,unitno1,unitno2,MM,N,ierr
  MM=360
  N=16
  inquire(file=trim(RANAME), exist=exists)
  if (exists) then
   unitno1 = get_new_fileunit()
   open(unitno1, file=trim(RANAME), action="read", iostat=ierr)
   if (ierr .eq. 0) then            
    inquire(file=trim(XXNAME), exist=exists)    
    if (exists) then
     unitno2 = get_new_fileunit()
     open(unitno2, file=trim(XXNAME), action="read", iostat=ierr)      
     if (ierr .eq. 0) then       
      do I=1,MM
       READ(unitno1,*) ITH,ZX(:)
       READ(unitno2,*) ITH,YX(:)
       do J=1,N
        EyeSys%RA(i,j)=ZX(j)
        EyeSys%XX(i,j)=YX(j)
       end do
       EyeSys%DEG(i)=ITH
      end do 
      CLOSE (unitno1)
      CLOSE (unitno2)
      else
         print*, "Error ", ierr ," attempting to open file ", trim(XXNAME)
        stop
      endif         
     else
      print*, "Error -- cannot find file: ", trim(XXNAME)
      stop
     endif 
    else
     print*, "Error ", ierr ," attempting to open file ", trim(RANAME)
     stop
    endif       
   else
    print*, "Error -- cannot find file: ", trim(RANAME)
    stop
   endif              
end subroutine rcnvrte

subroutine rcnvrta(KXNAME)
! ATLAS VERSION
 use io_functions, only : get_new_fileunit
 USE set_precision, ONLY : wp
 USE cornea_arrays, ONLY : Atlas
 implicit none
 logical :: exists
 CHARACTER(80) KH1,KH2,KH3
 character(len=*), intent(in) :: KXNAME
 INTEGER :: K,I,J,io,ITH,JTH,unitno,MM,N,ierr
 REAL(wp) :: R,DIST,Y,POW,AVGN,AVGR
 MM=180
 N=22

 inquire(file=trim(KXNAME), exist=exists)
 if (exists) then
  unitno = get_new_fileunit()
  open(unitno, file=trim(KXNAME), action="read", iostat=ierr)
   if (ierr .eq. 0) then                         
!   READ HEADERS
    K=0 
    DO 
       K=K+1 
       
       READ(unitno,*,END=100,IOSTAT=io) KH1
	
        IF(io.GT.0) THEN
         WRITE(*,*) 'ERROR ON INPUT'
         GOTO 100
        ENDIF
	
        IF (KH1.EQ.'#Begin_Table') THEN
         READ(unitno,*,END=100,IOSTAT=io) KH1
         READ(unitno,*,END=100,IOSTAT=io) KH1
         READ(unitno,*,END=100,IOSTAT=io) KH1,KH2,KH3
        ENDIF
	
        IF (KH1.EQ.'Ring') THEN
         IF (KH2.EQ.'Point'.AND.KH3.EQ.'Radius') THEN
!         DATA READ RADIUS? RING POSITION
          DO J=1,N
           DO I=1,MM
!          RING, POINT(0-180), RADIUS  
            READ(unitno,*) ITH,JTH,R
!          IN DEGREES
!	   THETA=2*JTH 
!          RING NUMBERS
!          SHOULD ALWAYS BE TRUE: ITH.EQ.(J-1) & JTH.NE.(I-1)
           IF (ITH.NE.(J-1)) WRITE(*,*) 'ATLAS RADIUS READ ERROR'
           IF (JTH.NE.(I-1)) WRITE(*,*) 'ATLAS RADIUS POINT=THETA/2 READ ERROR'
            Atlas%AR(JTH+1,ITH+1)=R
           end do 
          end do           
        ENDIF 
        ENDIF  
	
        IF (KH1.EQ.'Ring') THEN
         IF (KH2.EQ.'Point'.AND.KH3.EQ.'Distance(MM)') THEN       
!         DATA READ POWER
          DO J=1,N
           DO I=1,MM
!          RING,POINT(0-180),DISTANCE,ELEVATION,POWER,CHARACTER,CHARACTER 
!          DISTANCE OR RADIUS? ABOVE FOR EACH RING
           READ(unitno,*,END=100,IOSTAT=io) ITH,JTH,DIST,Y,POW,KH1,KH2
!          SHOULD ALWAYS BE TRUE: ITH.EQ.(J-1) & JTH.NE.(I-1)
           IF (ITH.NE.(J-1)) WRITE(*,*) 'ATLAS POWER READ ERROR'
           IF (JTH.NE.(I-1)) WRITE(*,*) 'ATLAS POWER POINT=THETA/2 READ ERROR'
!          UNLIKELY TO NEED ELEVATION
            Atlas%AY(JTH+1,ITH+1)=Y
            Atlas%AD(JTH+1,ITH+1)=DIST
            Atlas%AP(JTH+1,ITH+1)=POW
           end do 
          end do           
         ENDIF
        ENDIF
       
      END DO
!      FINISHED READING ATLAS FILE
      write(*,*) 'Read ',K,' lines in',trim(KXNAME)     
100   CLOSE (unitno)
      else
       print*, "Error ", ierr ," attempting to open file ", trim(KXNAME)
       stop
      endif       
    else
     print*, "Error -- cannot find file: ", trim(KXNAME)
     stop
    endif

!      POPULATE Atlas DEG
       do i=1,MM
        ITH=2*(i-1)
        Atlas%DEG(i)=ITH
       end do

!      Calculate average magnification
!      ARRAYD is never 0
       AVGR=0
       AVGN=0
       DO I=1,MM 
        DO J=1,N
        IF (Atlas%AR(I,J).GT.0)THEN
         AVGN=AVGN+1                 
         AVGR=AVGR+Atlas%AR(I,J)/Atlas%AD(I,J)
        ENDIF
        end do
       end do 

!      Multiply ArrayR by magnification
       do I=1,MM 
        do J=1,N
        Atlas%AR(I,J)=Atlas%AR(I,J)*AVGN/AVGR
        end do
       end do 

!       write(*,*) 'Magnification from ',trim(KXNAME),AVGN/AVGR 

!      CODE for debugging
!       unitno = get_new_fileunit()
!       open(unitno, file='READR.ORIG.CAR', iostat=ierr)
!       if (ierr .eq. 0) then
!       DO I=1,MM 
!       DO J=1,N
!       IF (J.EQ.19)THEN 
!       IF (Atlas%AP(I,J).GT.0)THEN
!        WRITE(unitno,*) I,Atlas%AP(I,J)  
!       ENDIF 
!       IF (Atlas%AR(I,J).GT.0)THEN
!        WRITE(unitno,*) I,Atlas%AP(I,J)/10,Atlas%AR(I,J),Atlas%AD(I,J)  
!       ENDIF 
!       ENDIF
!       end do	 
!       end do 
!       CLOSE(unitno)
!      else
!       print*, "Error ", ierr ," attempting to open file ", trim('READR.ORIG.CAR')
!       stop
!      endif
             
       RETURN
       
end subroutine rcnvrta


