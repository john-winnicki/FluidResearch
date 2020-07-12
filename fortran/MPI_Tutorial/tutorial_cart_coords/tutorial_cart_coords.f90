program tutorial_cart_shift 

!LESSON LEARNED: YOU MUST USE DIMS_CREATE in order to create the dimensions of the cart
!Lesson learned: You have to zero the dims array before passing it, since dims is both an input and output. If it isn't zero, it
!constrains certain values
use mpi
IMPLICIT NONE

integer old_comm, new_comm, ndims, ierr 
logical periods(0:1), reorder

integer size

integer old_comm_SIZE 

integer dims(0:1)

integer rank, coords(0:1)

CALL MPI_INIT(ierr)

old_comm = MPI_COMM_WORLD

call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)


ndims = 2 
periods(0) = .true. 
periods(1) = .false. 
reorder = .true.

dims = 0
CALL MPI_Dims_create(size, 2, dims, ierr)

call MPI_Cart_create(old_comm, ndims, dims, periods, reorder, new_comm, ierr)

call MPI_COMM_RANK(new_comm, rank, ierr)
! write(*,*) rank, dims
call MPI_Cart_coords(new_comm, rank, 2, coords, ierr)
write(*,100) rank, coords
100 FORMAT('Rank: ', I0, '      **************      Coords: (', I0,",", I0, ")")
CALL MPI_FINALIZE(ierr)

end program tutorial_cart_shift

