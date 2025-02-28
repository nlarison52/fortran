program test_rref
    use math
    implicit none

    real, allocatable, dimension(:,:) :: A, R
    integer :: i, j

    ! allocate and define a test matrix
    allocate(A(3,4))  ! example 3x4 augmented matrix

    ! defining the test matrix
    A = reshape([1.0, 2.0, 3.0, &
             4.0, 5.0, 6.0, &
             0.0, 0.0, 0.0], [3,3]) 

!     print original matrix
    print *, "original matrix A:"
    do i = 1, size(A,1)
        print "(4F8.3)", A(i, :)
    end do

    ! compute rref
    R = rref(A)

    ! print reduced matrix
    print *, "reduced row echelon form (rref) of A:"
    do i = 1, size(R,1)
        print "(4F8.3)", R(i, :)
    end do

    ! cleanup
    deallocate(A, R)

end program test_rref
