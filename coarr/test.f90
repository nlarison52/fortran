program test
    implicit none
    integer :: i, me
    integer :: max = 0
    integer :: x[*]
    real :: r


    me = this_image()
    call random_seed()
    call random_number(r)
    x = int(r * 100) + 1

    sync all

    if (me == 1) then
        do i = 1, num_images()
            print *, "Image", i, "value: ", x[i]
            if (x[i] > max) then
                max = x[i]
            end if
        end do

        print *, "Max val: ", max

    end if


end program test
