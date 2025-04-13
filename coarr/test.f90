program test
    implicit none
    integer, parameter :: n = 10
    real :: x(n)[*], y(n)[*], z(n)[*], z_single(n)
    real :: a[*]
    real, allocatable :: x_loc(:)[:], y_loc(:)[:], z_loc(:)[:]
    integer :: leftover, av_size, offset, i

    if (this_image() == 1) then
        call random_seed()
        call random_number(a)
        call random_number(y)
        call random_number(x)
    end if

    sync all 
    if (this_image() /= 1) then
        a = a[1]
        x = x(:)[1]
        y = y(:)[1]
    end if

    sync all

    a = a * 100 + 1
    y = y * 100 + 1
    x = x * 100 + 1

    leftover = mod(n, num_images())
    av_size = n / num_images()

    if (this_image() == 1) then
        allocate(x_loc(av_size+leftover)[*])
        allocate(y_loc(av_size+leftover)[*])
        allocate(z_loc(av_size+leftover)[*])

        x_loc = x(1 : av_size+leftover)
        y_loc = y(1 : av_size+leftover)
        z_loc = z(1 : av_size+leftover)

        call axpy(a, x_loc, y_loc, av_size+leftover, z_loc)

    else
        allocate(x_loc(av_size)[*])
        allocate(y_loc(av_size)[*])
        allocate(z_loc(av_size)[*])

        offset = (this_image() - 1) * av_size + leftover + 1

        x_loc = x(offset : offset+av_size-1)
        y_loc = y(offset : offset+av_size-1)
        z_loc = z(offset : offset+av_size-1)
        call axpy(a, x_loc, y_loc, av_size, z_loc)
    end if





    sync all ! then collect vals to the first image

    if (this_image() == 1) then
        z(1 : av_size+leftover) = z_loc

        do i = 2, num_images()
            offset = (i - 1) * av_size + leftover + 1
            z(offset : offset+av_size-1) = z_loc(:)[i]
        end do

        print *, "Parallel result: ", z

        call axpy(a, x, y, n, z_single)

        print *, "Single core result: ", z_single
    end if




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
