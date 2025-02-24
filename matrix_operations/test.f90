program test
    use math

    integer, allocatable, dimension(:, :) :: b, A, x
    integer :: i

    allocate(A(2, 2))
    allocate(x(2, 1))

    A = 1
    x = 1

    b = mm(A, x)

    do i = 1, 2
        print *, b(i, :)
    end do

    deallocate(A)
    deallocate(x)
    deallocate(b)
end program test
