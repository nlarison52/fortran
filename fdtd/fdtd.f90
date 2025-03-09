program fdtd
    implicit none
    integer, parameter :: nx = 200, ny = 200, nt = 2500, arrlen = 100
    integer, parameter :: pml_thickness = 10  ! Add PML
    real, parameter :: dx = 1.0, dy = 1.0, dt = 0.1
    real :: Hx(nx, ny), Hy(nx, ny), Ez(nx, ny)
    real :: mu(nx, ny), epsilon(nx, ny)
    real :: Ezx(nx, ny), Ezy(nx, ny)  
    integer :: i, j, t, i_center, j_center, radius, percent, filled, bar_length
    integer :: rx_x, rx_y, tx_x
    real, parameter :: c_mur = (1.0 - dt / dx) / (1.0 + dt / dx)
    real :: f0, f1, chirp_duration, t_norm, steer_angle_deg
    real :: a, b
    integer, dimension(arrlen) :: tx_y
    real :: weights(arrlen)
    real :: sigma_x(nx), sigma_y(ny)  

    Hx = 0.0
    Hy = 0.0
    Ez = 0.0
    Ezx = 0.0  
    Ezy = 0.0
    mu = 1.0
    epsilon = 1.0

    f0 = 0.05
    f1 = 0.2
    chirp_duration = 10
    steer_angle_deg = -45
    rx_x = 25
    rx_y = 100

    tx_x = 25
    tx_y = [(nint(real(ny)/2.0 - real(arrlen)/2.0 + real(i)), i = 1, arrlen)]

    ! Initialize steering weights
    do i = 1, arrlen
        weights(i) = exp(-((tx_y(i) - 100.0)**2) / (2.0 * (arrlen / 4.0)**2))
    end do

    ! PML setup
    do i = 1, nx
        if (i <= pml_thickness) then
            sigma_x(i) = 1.5 * ((pml_thickness - i + 1) / real(pml_thickness))**3
        else if (i > nx - pml_thickness) then
            sigma_x(i) = 1.5 * ((i - (nx - pml_thickness)) / real(pml_thickness))**3
        else
            sigma_x(i) = 0.0
        end if
    end do
    do j = 1, ny
        if (j <= pml_thickness) then
            sigma_y(j) = 1.5 * ((pml_thickness - j + 1) / real(pml_thickness))**3
        else if (j > ny - pml_thickness) then
            sigma_y(j) = 1.5 * ((j - (ny - pml_thickness)) / real(pml_thickness))**3
        else
            sigma_y(j) = 0.0
        end if
    end do

    open(10, file="output.dat", status="replace")
    open(20, file="radar.dat", status="replace")

    i_center = nx / 2
    j_center = ny / 2
    radius = min(nx, ny) / 6

!    do i = 1, nx
!        do j = 1, ny
!            if ((i - i_center)**2 + (j - j_center)**2 <= radius**2) then
!                mu(i, j) = 2.0
!                epsilon(i, j) = 4.0
!            end if
!        end do
!    end do

    i_center = nx / 2
    j_center = ny / 2
    a = real(nx) / 6.0
    b = real(ny) / 10.0

!    do i = 1, nx
!        do j = 1, ny
!            if (((i - i_center)**2 / a**2) + ((j - j_center)**2 / b**2) <= 1.0) then
!                mu(i, j) = 2.0
!                epsilon(i, j) = 4.0
!            end if
!        end do
!    end do

    print *, "Starting FDTD Simulation..."
    bar_length = 40

    do t = 1, nt
        if (t <= chirp_duration) then
            t_norm = real(t) / chirp_duration
            do i = 1, arrlen
                Ez(tx_x, tx_y(i)) = 10.0 * weights(i) * sin(2.0 * 3.14159 * (f0 + (f1 - f0) * t_norm) * t + &
                    2.0 * 3.14159 * (f0 + (f1 - f0) * t_norm) * (tx_y(i) - 100) * &
                    sin(steer_angle_deg * 3.14159 / 180.0))
                Ezx(tx_x, tx_y(i)) = Ez(tx_x, tx_y(i))  ! Sync PML fields with source
                Ezy(tx_x, tx_y(i)) = Ez(tx_x, tx_y(i))
            end do
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
                Ezx(i, j) = Ezx(i, j) * (1.0 - sigma_x(i) * dt / epsilon(i, j)) + &
                            (dt / epsilon(i, j)) * (Hy(i, j) - Hy(i-1, j)) / dx
                Ezy(i, j) = Ezy(i, j) * (1.0 - sigma_y(j) * dt / epsilon(i, j)) - &
                            (dt / epsilon(i, j)) * (Hx(i, j) - Hx(i, j-1)) / dy
                Ez(i, j) = Ezx(i, j) + Ezy(i, j)
            end do
        end do

        do i = 1, nx
            Ez(24, i) = 0.0
            Ezx(24, i) = 0.0  ! Ensure PML fields are consistent
            Ezy(24, i) = 0.0
        end do
        ! Removed Mur's ABC - PML handles boundaries now

        if (mod(t, 5) == 0) then
            write(10, *) "Timestep:", t
            do i = 1, nx
                write(10, '(200F8.4)') (Ez(i, j), j = 1, ny)
            end do
        end if

        if (mod(t, 5) == 0) then
            if (t > 100) then
                write(20, '(I5, F10.5)') t, Ez(rx_x, rx_y)
            else
                write(20, '(I5, F10.5)') t, 0.0
            end if
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
