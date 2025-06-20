module metadata_decoders
    implicit none
contains

    function decode_four_byte_unsigned(data, length, value, size) result(success)
        implicit none
        integer(kind=1), intent(in) :: data(:)
        integer, intent(in) :: length
        integer, intent(out) :: value
        integer, intent(out) :: size
        logical :: success
        integer :: b1, b2, b3, b4

        ! Convert signed bytes to full integers
        b1 = transfer(data(1), 0)

        ! 1-byte format
        if (iand(b1, int(128)) == 0) then
            value = b1
            size = 1
            success = .true.
            return
        end if

        ! 2-byte format
        if (iand(b1, int(192)) == int(128)) then
            if (length < 2) then
                success = .false.
                return
            end if
            b2 = transfer(data(2), 0)
            value = ishft(iand(b1, int(63)), 8) + b2
            size = 2
            success = .true.
            return
        end if

        ! 4-byte format
        if (length < 4) then
            success = .false.
            return
        end if

        b2 = transfer(data(2), 0)
        b3 = transfer(data(3), 0)
        b4 = transfer(data(4), 0)
        value = ishft(iand(b1, int(63)), 24) + ishft(b2, 16) + ishft(b3, 8) + b4
        size = 4
        success = .true.
    end function decode_four_byte_unsigned

    function decode_two_byte_signed(data, length, value, size) result(success)
        implicit none
        integer(kind=1), intent(in) :: data(:)
        integer, intent(in) :: length
        integer, intent(out) :: value
        integer, intent(out) :: size
        logical :: success
        integer :: b1, b2, combined

        if (length < 1) then
            success = .false.
            return
        end if

        b1 = transfer(data(1), 0)

        ! 1-byte format (7-bit signed)
        if (iand(b1, int(128)) == 0) then
            value = iand(b1, int(127))
            if (iand(value, int(64)) /= 0) value = value - 128
            size = 1
            success = .true.
            return
        end if

        ! 2-byte format (14-bit signed)
        if (length < 2) then
            success = .false.
            return
        end if

        b2 = transfer(data(2), 0)
        combined = ishft(iand(b1, int(127)), 8) + b2
        if (iand(combined, int(8192)) /= 0) then
            value = combined - 16384
        else
            value = combined
        end if

        size = 2
        success = .true.
    end function decode_two_byte_signed

end module metadata_decoders

program test_decoders
    use metadata_decoders
    implicit none

    integer(kind=1) :: bytes(4)
    integer :: val, size
    logical :: ok
    integer :: ubytes(4),i

    print *, "=== Test: FOUR_BYTE_UNSIGNED_ENCODING (0xC0000001 = 65536) ==="
    bytes = [int(-64, kind=1), int(0, kind=1), int(0, kind=1), int(1, kind=1)]

    print *, "=== Test: FOUR_BYTE_UNSIGNED_ENCODING (C0000001 = 65536) ==="
    ubytes = [192, 0, 0, 1]  ! equivalent to hex: C0 00 00 01

    do i = 1, 4
        bytes(i) = transfer(ubytes(i), bytes(i))
    end do

    ok = decode_four_byte_unsigned(bytes, 4, val, size)
    if (ok) print *, "Decoded unsigned: ", val, "Size: ", size

    print *, "=== Test: TWO_BYTE_SIGNED_ENCODING (-1 encoded as FF FF) ==="
    bytes = [int(-1,1), int(-1,1), int(0,1), int(0,1)]
    ok = decode_two_byte_signed(bytes, 2, val, size)
    if (ok) print *, "Decoded signed: ", val, "Size: ", size

end program test_decoders








