program main
    implicit none
    integer  i
    character(len=64) ::  binary_data="b28d18bdbd610395c85e63df&
                                      &d85c7d90caeb1e548c96c73c&
                                      &193c056bd41ec9fc"
    character(len=32) ::  refbinhead, readbinhead

    read( binary_data, '(32Z2)' ) (refbinhead(i:i), i = 1,32)
    open(unit=11, file="random-file.bin", access='stream')
    read(11) readbinhead
    close(11)

    write(*,'(A,(100Z2.2))')"ref  : ",( refbinhead(i:i), i = 1,32)
    write(*,'(A,(100Z2.2))')"read : ",(readbinhead(i:i), i = 1,32)
    write(*,'(A, L2)') "Are they equal?: ", refbinhead==readbinhead
endprogram
