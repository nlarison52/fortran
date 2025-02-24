program test
    use util

    implicit none
    integer, allocatable, dimension(:, :) :: x
    integer :: i, j

    allocate(x(3, 3))

    do i = 1, 3
        do j = 1, 3
            x(i, j) = i * j
        end do
    end do    

    x(1, 2) = 69

    print *, "matrix before"
    do i = 1, 3
        print *, x(i, :)
    end do

    call transpose(x)

    print *, "matrix after"

    do i = 1, 3
        print *, x(i, :)
    end do

end program test
