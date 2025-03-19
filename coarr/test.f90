program test
    implicit none
    integer :: my_rank
    integer, allocatable, dimension(:) :: x[:]

    my_rank = this_image()

    allocate(x(my_rank)[*])

    x = my_rank

    sync all

    print *, "Im num: ", my_rank
    print *, "Value: ", x



end program test
