program coarray_test
    implicit none
    integer :: me, total
    integer, codimension[*] :: x  ! Declare a coarray variable

    ! Get image number (me) and total number of images
    me = this_image()
    total = num_images()

    ! Assign unique values per image
    x = me * 10

    ! Sync images before reading
    sync all

    ! Print data from another image (modulo trick for circular access)
    print *, "Image", me, "sees x from Image", mod(me, total) + 1, ":", x[mod(me, total) + 1]

end program coarray_test
