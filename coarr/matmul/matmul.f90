program matmul
    implicit none
    real, allocatable :: A(:,:)[:], B(:,:)[:], Z(:,:)[:]
    integer :: n, i, j, chunk, remainder, images, me, offset

    n = 10



    images = num_images()
    me = this_image()


    allocate(A(n, n)[*])
    allocate(B(n, n)[*])
    allocate(Z(n, n)[*])

    if (me == 1) then ! loop assigns A and B to be all 1s and 2 * I
        A = 0
        B = 1

        do i = 1, n
            do j = 1, n
                if (i == j) then
                    A(i, j) = 2
                end if
            end do
        end do
    end if

    sync all

    if (me == 1) then
        do i = 2, images
            A(:,:)[i] = A(:,:)
            B(:,:)[i] = B(:,:)
        end do
    end if

   sync all

   chunk = n / images
   remainder = mod(n, images)

   if (me <= remainder) then
       offset = (me - 1) * (chunk + 1)

   else
       offset = remainder * (chunk + 1) + (me - remainder - 1) * chunk

   end if 

   if (me <= remainder) then
        do i = 1, chunk + 1
            do j = 1, n
                Z(offset + i, j) = dot_product(A(i, :), B(:, j))
            end do
        end do

    else
        do i = 1, chunk
            do j = 1, n
                Z(offset + i, j) = dot_product(A(i, :), B(:, j))
            end do
        end do

    end if


    sync all

    ! if (me == 4) then
    !     do i = 1, n
    !         print *, Z(i, :)
    !     end do
    ! end if

    if (me == 1) then
        do i = 2, images
            if (i <= remainder) then
                offset = (i - 1) * (chunk + 1)
                do j = 1, chunk + 1
                    Z(offset+j, :) = Z(offset+j, :)[i]
                end do

            else
                offset = remainder * (chunk + 1) + (i - remainder - 1) * chunk
                do j = 1, chunk
                    Z(offset+j, :) = Z(offset+j, :)[i]
                end do
            end if
        end do

        do i = 1, n
            print *, Z(i, :)
        end do
    end if


    

end program matmul
