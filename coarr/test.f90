program test
    implicit none
    integer :: i, me
    integer :: A(4, 4)[*]
    integer :: b(4), z[*]

    b = [(i, i = 1,4)]

    me = this_image()
    A(me, :) = me

    z = dot_product(A(me, :), b)


    sync all
    if (this_image() == 1) then

    print *, "Image 1 sees: "

        do i = 1, num_images()
            print *, z[i]

        end do
    end if

end program test
