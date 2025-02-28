program test_rref
    use math
    implicit none

    real, allocatable, dimension(:,:) :: A, R, LU
    integer, allocatable, dimension(:) :: ipiv
    integer :: i, j, n, m, info

    ! define matrix size
    n = 3
    m = 4  ! augmented system

    ! allocate matrices
    allocate(A(n, m), R(n, m), LU(n, m))
    allocate(ipiv(n))

    ! defining test matrix
    A = reshape([1.0, 2.0, 3.0, 8.0, &
                 4.0, 5.0, 6.0, 3.0, &
                 0.0, 0.0, 0.0, 0.0], [n,m])

    ! print original matrix
    print *, "original matrix A:"
    do i = 1, n
        print "(4F8.3)", A(i, :)
    end do

    ! compute rref using our function
    R = rref(A)

    ! print our rref result
    print *, "rref(A) result:"
    do i = 1, n
        print "(4F8.3)", R(i, :)
    end do

    ! copy A into LU before using LAPACK
    LU = A

    ! compute LU decomposition (row echelon form)
    call dgetrf(n, m, LU, n, ipiv, info)

    ! print LU decomposition (row echelon form)
    print *, "LU decomposition (row echelon form):"
    do i = 1, n
        print "(4F8.3)", LU(i, :)
    end do

    ! compare LU with RREF
    print *, "comparison of rref(A) vs LU:"
    do i = 1, n
        do j = 1, m
            if (abs(R(i, j) - LU(i, j)) > 1.0E-6) then
                print *, "mismatch at (", i, ",", j, "): ", R(i, j), " vs ", LU(i, j)
            end if
        end do
    end do

    ! cleanup
    deallocate(A, R, LU, ipiv)

end program test_rref
