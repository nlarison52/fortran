program transpose
    integer :: n, m
    real, allocatable :: X(:,:)[:], Y(:,:)[:], Z(:,:)[:]
    integer :: images, me


    images = num_images()
    me = this_image()

    allocate(X(m, n)[*])

    sync all
    if (me == 1) then
        ! assign my first array that will then be sent to other images


    end if



end program transpose
