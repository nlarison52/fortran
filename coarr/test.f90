program test
    implicit none
    integer, allocatable :: glob_arr(:)
    integer :: loc_arr(5)[*], i, cur_min, j
    integer :: loc_ptr[*]
    real :: r(5)

    call random_seed()
    call random_number(r)

    loc_arr = int(r * 100 + 1)

    call sort_in_place(loc_arr)

    loc_ptr = 1

    if (this_image() == 1) then
        allocate(glob_arr(5 * num_images()))

        do i = 1, num_images()
            glob_arr((i - 1) * 5 + 1 : (i - 1) * 5 + 5) = loc_arr(:)[i]
        end do

        print *, glob_arr


        do i = 1, num_images() * 5
            do j = 1, num_images()
                if (loc_arr(loc_ptr[i])[i] < cur_min) then
                    cur_min = loc_arr(loc_ptr[i])[i] 
                    loc_ptr[i] = loc_ptr[i] + 1
                    ! this logic is incorrect, 
                    ! needs to increment local ptr only if that is selected from all remote arrays
                    ! (already sorted so just select from lowest index)
                end if
            end do

            glob_arr(i) = cur_min 
        end do 

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
