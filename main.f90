program main
    implicit none
    real :: x, y

    print *, "Enter a temperature in F: "
    read *, x

    y = convert(x)

    print *, "Temperature in C: ", y

contains

     function convert(f) result(c)
        real, intent(in) :: f
        real :: c

        c = 5.0 / 9.0 * (f - 32.0)
    end function convert


end program main
