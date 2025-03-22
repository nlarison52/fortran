program test
  implicit none
  integer :: my_rank, row, col, i, j, source_image
  real(8) :: x(2, 2), z(2, 2)[*]
  integer :: num_procs

  ! Explicit declaration
  real(8), external :: ddot

  x = 1.0d0
  my_rank = this_image()
  num_procs = num_images()

  if (num_procs /= 4) then
      if (my_rank == 1) print *, "Run with exactly 4 images!"
      stop
  end if

  ! Correct mapping of image rank to matrix indices
  row = (my_rank - 1) / 2 + 1
  col = mod(my_rank - 1, 2) + 1

  ! Compute the matrix element (row, col)
  z(row, col) = ddot(2, x(row, :), 2, x(:, col), 1)

  sync all  ! Wait for all computations

  if (my_rank == 1) then
      print *, "Final Matrix is:"
      do i = 1, 2
          do j = 1, 2
              source_image = (i - 1) * 2 + j
              z(i,j) = z(i,j)[source_image]
          end do
          print '(2F8.2)', z(i,:)
      end do
  end if

end program test
