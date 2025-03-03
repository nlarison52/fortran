module math
    implicit none

contains
    function mm(x, y) result(c) ! basic matmul implementation
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

    function rref(X) result(Z)
        implicit none
        real, dimension(:, :) :: X
        real, allocatable, dimension(:, :) :: Z
        integer, dimension(2) :: dimx
        integer :: i, j, cur_pivot, pivot_row
        real, parameter :: eps = 1.0E-7

        dimx = shape(X)
        allocate(Z(dimx(1), dimx(2)))
        Z = X  ! copy X into Z so original matrix remains unchanged

        cur_pivot = 0
        do i = 1, dimx(1)
            do while (abs(Z(i, cur_pivot)) < eps)
                cur_pivot = cur_pivot + 1
                do j = i, dimx(2)
                    if (abs(Z(j, cur_pivot)) > eps) then
                        call swap_rows(X, i, j)
                        exit
                    end if
                end do
            end do

            call normalize_row(Z, i, cur_pivot)
            call clear_col(Z, i, cur_pivot)
        end do

    end function rref

    subroutine normalize_row(X, row, col)
        implicit none
        real, intent(inout), dimension(:,:) :: X
        integer, intent(in) :: row, col
        real, parameter :: eps = 1.0E-7  ! Small threshold for floating-point safety

        ! Check if X(row, col) is "effectively zero" before dividing
        if (abs(X(row, col)) > eps) then
            X(row, :) = X(row, :) / X(row, col)  ! ✅ Safe division
        else
            error stop "Pivot element is too close to zero for normalization!"  
        end if
    end subroutine normalize_row


    subroutine clear_col(X, row, col)
        implicit none
        real, intent(inout), dimension(:, :) :: X
        integer, intent(in) :: row, col
        integer :: i

        do i = 1, size(X, 1)
            if (i == row) cycle
            X(i, :) = X(i, :) - X(row, :) * X(i, col)
        end do

    end subroutine clear_col


    subroutine swap_rows(X, i, j)
        real, intent(inout), dimension(:, :) :: X
        integer :: i, j 
        real, allocatable, dimension(:) :: temp
        integer, dimension(2) :: dimx

        dimx = shape(X)

        allocate(temp(dimx(2)))

        temp = X(i, :)
        X(i, :) = X(j, :)
        X(j, :) = temp

    end subroutine swap_rows


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
                        exit
                    end if
                else
                    if (Z(i, j) /= 0) then
                        res = .false.
                        exit
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

    subroutine real_transpose(x)
        real(8), allocatable, dimension(:, :), intent(inout) :: x
        real(8), allocatable, dimension(:, :) :: y
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
    end subroutine real_transpose

end module math
