module math
    implicit none
contains
    function mm(x, y) result(c)
        integer, allocatable, dimension(:, :) :: x, y, c
        integer, dimension(2) :: dimx, dimy
        integer :: i, j, k

        dimx = shape(x)
        dimy = shape(y)

        if (dimx(2) /= dimy(1)) then
           error stop "Matix dimensions are incompatible" 
        end if

        allocate(c(dimx(1), dimy(2)))
        c = 0
    
        do i = 1, dimx(1)
            do j = 1, dimy(2)
                do k = 1, dimx(2)
                    c(i, j) = c(i, j) + x(i, k) * y(k, j)     
                end do
            end do
        end do
    end function mm
end module math
