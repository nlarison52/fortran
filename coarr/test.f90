program test
    implicit none
    integer, allocatable :: glob_arr(:), ptrs(:), final_arr(:)
    integer :: loc_arr(5)[*], i, cur_min, j, cur_j
    real :: r(5)

    call random_seed()
    call random_number(r)

    loc_arr = int(r * 100 + 1)

    call sort_in_place(loc_arr)


    if (this_image() == 1) then
        allocate(glob_arr(5 * num_images()))
        allocate(final_arr(5 * num_images()))
        do i = 1, num_images()
            glob_arr((i - 1) * 5 + 1 : (i - 1) * 5 + 5) = loc_arr(:)[i]
        end do

        ! print *, glob_arr

        allocate(ptrs(num_images()))
        ptrs = 1


        do i = 1, 5 * num_images()
            cur_min = huge(0)
            cur_j = -1
            do j = 1, num_images()
                if (ptrs(j) > 5) cycle

                if (glob_arr((j-1)*5+ptrs(j)) < cur_min) then
                    cur_min = glob_arr((j-1)*5+ptrs(j))
                    cur_j = j

                end if
            end do
            final_arr(i) = cur_min
            ptrs(cur_j) = ptrs(cur_j) + 1
        end do

        print *, final_arr
    end if 




contains
    subroutine sort_in_place(arr)
        implicit none
        integer, intent(inout) :: arr(:)
        integer :: i, j, key, n

        n = size(arr)

        do i = 2, n
            key = arr(i)
            j = i - 1
            do while (j > 0 .and. arr(j) > key)
                arr(j+1) = arr(j)
                j = j - 1
            end do
            arr(j+1) = key
        end do
    end subroutine sort_in_place

end program test
