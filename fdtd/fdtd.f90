program fdtd
    implicit none
    integer, parameter :: nx = 200, ny = 200, nt = 2500
    real, parameter :: dx = 1.0, dy = 1.0, dt = 0.1
    real :: Hx(nx, ny), Hy(nx, ny), Ez(nx, ny)
    real :: mu(nx, ny), epsilon(nx, ny)
    integer :: i, j, t, i_center, j_center, radius, percent, filled, bar_length
    integer :: tx_x, tx_y, rx_x, rx_y
    real, parameter :: c_mur = (1.0 - dt / dx) / (1.0 + dt / dx)
    real :: f0, f1, chirp_duration, t_norm
    real :: a, b

    Hx = 0.0
    Hy = 0.0
    Ez = 0.0
    mu = 1.0
    epsilon = 1.0

    f0 = 0.05
    f1 = 0.2
    chirp_duration = 100
    tx_x = 2
    tx_y = 2
    rx_x = tx_x
    rx_y = tx_y

    open(10, file="output.dat", status="replace")
    open(20, file="radar.dat", status="replace")

    i_center = nx / 2
    j_center = ny / 2
    radius = min(nx, ny) / 6

    ! this is for a circle
    do i = 1, nx
        do j = 1, ny
            if ((i - i_center)**2 + (j - j_center)**2 <= radius**2) then
                mu(i, j) = 2.0
                epsilon(i, j) = 4.0
            end if
        end do
    end do

    ! placement of ellipse
    i_center = nx / 2
    j_center = ny / 2
    a = nx / 6   ! Semi-major axis (adjustable)
    b = ny / 10   ! Semi-minor axis (adjustable)

    ! this is for a ellipse
!    do i = 1, nx
!        do j = 1, ny
!            if (((i - i_center)**2 / a**2) + ((j - j_center)**2 / b**2) <= 1.0) then
!                mu(i, j) = 2.0         ! Change permeability inside ellipse
!                epsilon(i, j) = 4.0    ! Change permittivity inside ellipse
!            end if
!        end do
!    end do

    print *, "Starting FDTD Simulation..."
    bar_length = 40  

    do t = 1, nt

        if (t <= chirp_duration) then
            t_norm = real(t) / chirp_duration
            Ez(tx_x, tx_y) = sin(2 * 3.14159 * (f0 + (f1 - f0) * t_norm) * t)
        end if
        
        do i = 1, nx
            do j = 1, ny-1
                Hx(i, j) = Hx(i, j) - (dt / mu(i, j)) * (Ez(i, j+1) - Ez(i, j)) / dy
            end do
        end do

        do i = 1, nx-1
            do j = 1, ny
                Hy(i, j) = Hy(i, j) + (dt / mu(i, j)) * (Ez(i+1, j) - Ez(i, j)) / dx
            end do
        end do

        do i = 2, nx-1
            do j = 2, ny-1
                Ez(i, j) = Ez(i, j) + (dt / epsilon(i, j)) * &
                          ((Hy(i, j) - Hy(i-1, j)) / dx - (Hx(i, j) - Hx(i, j-1)) / dy)
            end do
        end do



        ! Apply Mur's Absorbing Boundary Conditions
        do i = 2, nx-1
            Ez(i, 1) = Ez(i, 2) + c_mur * (Ez(i, 2) - Ez(i, 3))
            Ez(i, ny) = Ez(i, ny-1) + c_mur * (Ez(i, ny-1) - Ez(i, ny-2))
        end do

        do j = 2, ny-1
            Ez(1, j) = Ez(2, j) + c_mur * (Ez(2, j) - Ez(3, j))
            Ez(nx, j) = Ez(nx-1, j) + c_mur * (Ez(nx-1, j) - Ez(nx-2, j))
        end do

        ! Write to file every 50 steps
        if (mod(t, 50) == 0) then
            write(10, *) "Timestep:", t
            do i = 1, nx
                write(10, '(200F8.4)') (Ez(i, j), j = 1, ny)
            end do
        end if

        ! radar rx write
        if (mod(t, 5) == 0) then
            write(20, '(I5, F10.5)') t, Ez(rx_x, rx_y)
        end if

        if (mod(t, 100) == 0) then
            percent = int(100.0 * t / nt)
            filled = int(bar_length * percent / 100)

            write(*, '(A, I3, A, A, A, I5, A I5, A)', advance='no') CHAR(13), &
                percent, "% [", repeat("=", filled) // repeat(" ", bar_length - filled), "] ", t, "/", nt
        end if
    end do

    print *, CHAR(13), "Simulation Complete! [====================] 100% Done."

end program fdtd
