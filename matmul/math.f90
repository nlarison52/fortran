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


    function rref(X, b) result(Z)
        real, dimension(:, :) :: X, b
        real, allocatable, dimension(:, :) :: Z
        integer, dimension(2) :: dimx, dimb, dimz
        integer :: i, j

        dimx = shape(X)
        dimb = shape(b)
        
        if (dimx(1) /= dimx(2)) then
            error stop "X must be a square matrix"
        end if

        if ((dimb(1) /= dimx(1)) .or. (dimb(2) /= 1)) then
            error stop "b must be a vecotor with Nx1 where N is dim(X)"
        end if

        allocate(Z(dimx(1), dimx(2)+1))

        dimz = shape(Z)
        Z(:, 1:dimx(2)) = X
        Z(:, dimx(2)+1) = b

        do i = 1, dimz(1) !this works because base matrix is square
            call normalize(Z, i)
            call clear_col(Z, i)
        
            
        end do


    end function rref


    function is_reduced(Z) result(res)
        real, dimension(:, :) :: Z
        integer, dimension(2) :: zdim
        integer :: i, j
        logical :: res

        zdim = shape(Z)

        res = .true.

        do i = 1, zdim(1)
            do j = 1, zdim(2)-1
                if (i == j) then
                    if (Z(i, j) /= 1) then
                        res = .false.
                        end
                    end if
                else
                    if (Z(i, j) /= 0) then
                        res = .false.
                        end
                    end if
                end if
            end do
        end do
    end function is_reduced

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

end module math
