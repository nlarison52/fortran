program array
    implicit none
    integer :: i, j
    integer, dimension(3, 3) :: x, y, z
    integer, allocatable, dimension(:, :) :: n
    
    do i = 1, 3
        do j = 1, 3
            x(i, j) = i * j
            y(i, j) = i + j
        end do
    end do
    
    print *, "array 1:"
    do i = 1, 3
        print *, x(i, :)
    end do
    
    print *, "array 2:"
    do i = 1, 3
        print *, y(i, :)
    end do

    
    ! here is the case with dynamic memory
    allocate(n(3, 3))

    n = MATMUL(x, y)

    print *, "array 3:"
    do i = 1, 3
        print *, n(i, :) 
    end do

    if (allocated(n)) deallocate(n)

end program
