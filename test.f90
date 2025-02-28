program hello
    implicit none
    integer :: n, m, p

    interface
        function square(x) result(y)
            integer, intent(in) :: x
            integer :: y
        end function square
    end interface
    n = 5
    m = 6

    do p = 1, 5, 1
        n = square(n)
        call sqr(m)
        print *, n
        print *, m
    end do

        contains
            subroutine sqr(x)
                integer, intent(inout) :: x
                x = x ** 2
            end subroutine sqr
end program hello

