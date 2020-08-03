program vec_gen

use mpi
IMPLICIT NONE

INTEGER :: fin_step = 2500
INTEGER :: curr_step = 0
REAL, DIMENSION(2, 2500) :: vectors = 0.
integer rank, ierr, request
integer i
integer x,y

INTEGER DIMENSION(1,1) :: tracker_group = 0.
INTEGER DIMENSION(1,1) :: solver_group = 0.
INTEGER tracker_handle, solver_handle, st_intercom

CALL MPI_INIT(ierr)
CALL MPI_Comm_rank(MPI_COMM_WORLD, rank,ierr)

tracker_group = {0};
solver_group = {1};
call MPI_Group_incl(MPI_COMM_WORLD, 1, tracker_group, tracker_handle, ierr)
call MPI_Group_incl(MPI_COMM_WORLD, 1, solver_group, solver_handle, ierr)

IF(rank .eq. 0) THEN

     MPI_Intercomm_create(tracker_group, 0, MPI_COMM_WORLD, 1, 1, st_intercomm, ierr);
     !Tracker group is the group we want to link, 0 is the rank of the leader (arbitrarily chosen), MPI_COMM_WORLD is the communicator in which the two "leader"
     ! Processes share (that is, the tracker and solver processes).
!     IN MPI_COMM_WORLD, the rank is 0, and the rank of the tracker is 1. So in the above, we use 1 as the leader rank of the peer communicator.

    CALL MPI_IBCAST(x, 1, MPI_REAL, 0, 0, st_intercomm, request, ierr)

    write(*,*) "Writing up text file"
    open(1, file = 'particle_locations.txt', status = 'REPLACE')
    DO i = 1, fin_step
        WRITE(1,100) vectors(1,i), vectors(2,i), rank
        100 FORMAT (F10.4,",",F10.4,",",I0)
    END DO
END IF

call MPI_FINALIZE(ierr)

END PROGRAM vec_gen
