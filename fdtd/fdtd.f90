program fdtd
    implicit none
    integer, parameter :: nx = 100, ny = 100, nt = 2500
    real, parameter :: dx = 1.0, dy = 1.0, dt = 0.1
    real :: Hx(nx, ny), Hy(nx, ny), Ez(nx, ny)
    real :: mu(nx, ny), epsilon(nx, ny)
    integer :: i, j, t, i_center, j_center, radius

    Hx = 0.0
    Hy = 0.0
    Ez = 0.0
    mu = 1.0
    epsilon = 1.0

    open(10, file="output.dat", status="replace")


    i_center = nx / 2
    j_center = ny / 2
    radius = min(nx, ny) / 6

    do i = 1, nx
        do j = 1, ny
            if ((i - i_center)**2 + (j - j_center)**2 <= radius**2) then
                mu(i, j) = 2.0         ! Change permeability inside circle
                epsilon(i, j) = 4.0    ! Change permittivity inside circle
            end if
        end do
    end do
 
    do t = 1, nt
        
        if (t < 50) then
            Ez(2, 2) = sin(2.0 * 3.14159 * 0.1 * t)  ! Source at (2,2)
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

            if (mod(t, 50) == 0) then
                write(10, *) "Timestep:", t
                do i = 1, nx
                    write(10, '(100F8.4)') (Ez(i, j), j = 1, ny)
                end do
            end if

    end do

    print *, "FDTD simulation complete."

end program fdtd
