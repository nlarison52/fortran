program solve_system
    implicit none
    integer, parameter :: n = 2, nrhs = 1, lda = n, ldb = n
    integer :: ipiv(n), info
    real :: A(lda, n), B(ldb, nrhs)

    ! Matrix A in column-major order (LAPACK format)
    A = reshape([2.0, 1.0, &
                 1.0, 3.0], shape(A))

    ! Right-hand side B (2D array)
    B = reshape([8.0, 13.0], shape(B))

    ! Solve Ax = B using SGESV
    call SGESV(n, nrhs, A, lda, ipiv, B, ldb, info)

    if (info == 0) then
        print *, "Solution vector x:"
        print *, B  ! B now contains x
    else
        print *, "SGESV failed with info =", info
    end if
end program solve_system
