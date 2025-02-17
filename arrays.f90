program arrays
    implicit none
    integer :: i
    integer, dimension(5) :: arr

    do i = 1, 5
        arr(i) = i
    end do

    do i = 1, 5
        print *, arr(i)
    end do
end program arrays
