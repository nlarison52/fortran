program coarray_sum
    implicit none
    integer :: me, total, local_value, i
    integer, codimension[*] :: all_values
    integer :: sum_result

    me = this_image()
    total = num_images()

    local_value = me

    all_values = local_value

    sync all

    if (me == 1) then
        sum_result = 0

        do concurrent(i = 1:total)
            sum_result = sum_result + all_values[i]
        end do

        print *, "Total sum from all images: ", sum_result

    end if

end program coarray_sum
