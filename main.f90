program main
    implicit none
    integer :: x, y
    x = 6
    print *, factorial(x)

contains
    recursive function factorial(x) result(z)
        integer, intent(in):: x
        integer :: z

        if (x == 1) then 
            z = 1
        else
            z = x * factorial(x - 1)
        end if
    end function factorial


end program main
