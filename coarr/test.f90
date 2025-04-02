program test
    implicit none
    integer :: i, me
    integer :: max = 0
    integer :: x[*]


    me = this_image()
    call random_seed()
    x = random_number()

    sync all

    if (me == 1) then
        do i = 1, num_images()
            print *, "Image", i, "value: ", x[i]
            if (x[i] > max) then
                x = x[i]
            end if
        end do

    end if


end program test
