program test
    use math
    implicit none
    integer :: n, nrhs, lda, ldb, info, i
    integer, allocatable :: ipiv(:)
    real(8), allocatable :: A(:,:), b(:,:)

    n = 3
    nrhs = 1
    lda = n
    ldb = n

    allocate(A(n, n), b(n, nrhs), ipiv(n))

    A = reshape([2.0d0, -1.0d0, 3.0d0, &
                1.0d0,  3.0d0, -2.0d0, &
                0.0d0,  2.0d0, 1.0d0], [n,n])

    call real_transpose(A)

    b = reshape([5.0d0, -4.0d0, 3.0d0], [n, nrhs])

    do i = 1, n
        print *, A(i, :)
    end do 

    print *, b

    call dgesv(n, nrhs, A, lda, ipiv, b, ldb, info)

    if (info == 0) then
        print *, "Solution x:"
        print *, b
    else
        print *, "error: Singular matrix"
    end if

    deallocate(A, b, ipiv)
end program test
