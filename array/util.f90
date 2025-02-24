module util
    implicit none
contains
    subroutine transpose(x)
        integer, allocatable, dimension(:, :), intent(inout) :: x
        integer, allocatable, dimension(:, :) :: y
        integer, dimension(2) :: dims
        integer :: i, j

        dims = shape(x)

        allocate(y(dims(2), dims(1)))

        do i = 1, dims(1)
            do j = 1, dims(2)
                y(i, j) = x(j, i)
            end do
        end do
        
        deallocate(x) 
        allocate(x(dims(2), dims(1)))

        x = y
        deallocate(y)
    end subroutine transpose

end module util
