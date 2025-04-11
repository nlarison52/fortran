program test
    integer, parameter :: n = 10
    real :: x(n), y(n), z(n)
    real :: a

    call random_seed()
    call random_number(a)
    call random_number(y)
    call random_number(x)

    a = a * 100 + 1
    y = y * 100 + 1
    x = x * 100 + 1

    call axpy(a, x, y, n, z)

    print *, z
    


    




contains

    subroutine axpy(a, x, y, n, z)
        ! n is dim x
        implicit none
        integer :: i
        integer, intent(in) :: n
        real, intent(in) :: a, x(:), y(:)
        real, intent(inout) :: z(:)

        do i = 1, n
            z(i) = x(i) * a + y(i)
        end do 

    end subroutine axpy
end program test
