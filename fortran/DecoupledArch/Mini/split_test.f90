program split_test

use mpi

INTEGER world_rank, world_size, ierr, color
INTEGER row_Comm, row_rank, row_size

CALL MPI_INIT(ierr)

CALL MPI_Comm_rank(MPI_COMM_WORLD, world_rank, ierr)
CALL MPI_Comm_size(MPI_COMM_WORLD, world_size, ierr)

color = modulo(world_rank, 4)

CALL MPI_Comm_split(MPI_COMM_WORLD, color, world_rank, row_comm, ierr)

CALL MPI_Comm_rank(row_comm, row_rank, ierr)
CALL MPI_Comm_size(row_comm, row_size, ierr)

WRITE(*,*) world_rank, world_size, row_rank, row_size

CALL MPI_Comm_free(row_comm, ierr);

call MPI_FINALIZE(ierr)

END PROGRAM split_test
