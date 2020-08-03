program solver

use mpi

IMPLICIT NONE

INTEGER :: fin_step = 500
INTEGER :: curr_step = 0
REAL, DIMENSION(2, 2500) :: vectors = 0.
integer rank, ierr, request
integer i

!INTEGER, DIMENSION(1) :: tracker_group_list
!INTEGER, DIMENSION(1) :: solver_group_list
!INTEGER tracker_group, solver_group, st_intercomm1, st_intercomm2, world_handle
INTEGER st_intercomm1, st_intercomm2
!INTEGER tracker_comm, solver_comm

!INTEGER tracker_comm_rank, solver_comm_rank, tgsize, sgsize

REAL :: x1 = 0
REAL :: y1 = 0
REAL :: x2 = 0
REAL :: y2 = 0
REAL :: x3 = 0
REAL :: y3 = 0
REAL :: x4 = 0
REAL :: y4 = 0

INTEGER intercomm_rank, root_intercomm_rank

INTEGER color, solve_track_comm, status(MPI_STATUS_SIZE)

INTEGER :: tag = 22

REAL :: particle_pos_x = 1.
REAL :: particle_pos_y = 1.

INTEGER :: tracker_num = 2
INTEGER :: solver_num = 4




!LESSON LEARNED: TAGS ARE REALLY IMPORTANT. THEY MUST BE THE SAME THROUGHOUT THE COMMUNICATION CHANNELS.

! In essence, we create an intercommunicator. However, the intercommunicator actually produces 2 different communicators. Each of these communicators will be sending something from a certain rank within that communicator. In this case, we have 2 processes. One is rank 0, the other is rank 1. We use SPLIT to create 2 new communicators, each housing one process. Then, we call CREATE INTERCOMMUNICATOR for each, differently depending on the rank. If it is of rank 0, then we create intercomm1. If it is of rank 1, then we create intercomm2. Note that in every communication (intercomm create, send, recv), we always use the same tag. We then transmit the value of x from using the local intercommunicator to the rank of the value in the other intercommunicator. So we call send using the local intercommunicator produced (because NOTE that this is the only one that even exists on this machine). The rank we specify though, is the rank of the process in the OTHER communicator. 

CALL MPI_INIT(ierr)
CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

color = 1

CALL MPI_Comm_split(MPI_COMM_WORLD, color, rank, solve_track_comm, ierr);

CALL MPI_Intercomm_create(solve_track_comm, 0, MPI_COMM_WORLD, 0, tag, st_intercomm2, ierr);

CALL MPI_Comm_rank(st_intercomm2, intercomm_rank, ierr)
write(*,*) "intercomm rank: ", rank, intercomm_rank
!    CALL MPI_Bcast(root_intercomm_rank, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)


DO WHILE (curr_step<fin_step)
    IF(intercomm_rank .eq. 0) THEN
!            vectors(1, curr_step) = COS(curr_step * 0.1)
!            vectors(2, curr_step) = SIN(curr_step * 0.1)

        x1 = COS(curr_step * 0.1)
        y1 = SIN(curr_step * 0.1)

        x1 = -1.
        y1 = 1.


        CALL MPI_SEND(x1, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y1, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)

        CALL MPI_SEND(x1, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y1, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)

!        CALL MPI_BCAST(y, 1, MPI_REAL, root_intercomm_rank, st_intercomm, ierr)

!         write(*,200) vectors(1,i), vectors(2,i), rank
!         200 FORMAT (F10.4,",",F10.4,",",I0)
    ELSE IF (intercomm_rank .eq. 1) THEN
        x2 = COS( curr_step * 0.1 + 2.1)
        y2 = SIN( curr_step * 0.1 + 2.1)

        x2 = -1.
        y2 = -1.

        CALL MPI_SEND(x2, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y2, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)

        CALL MPI_SEND(x2, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y2, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)

    ELSE IF (intercomm_rank .eq. 2) THEN
        x3 = COS( curr_step * 0.1 + 1)
        y3 = SIN( curr_step * 0.1 + 1)

        x3 = 1.
        y3 = -1.

        CALL MPI_SEND(x3, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y3, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)

        CALL MPI_SEND(x3, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y3, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)

    ELSE
        x4 = COS( curr_step * 0.1)
        y4 = SIN( curr_step * 0.1)

        x4 = 1.
        y4 = 1.

        CALL MPI_SEND(x4, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y4, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)

        CALL MPI_SEND(x4, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y4, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)

    END IF
    curr_step = curr_step + 1
END DO

call MPI_FINALIZE(ierr)

END PROGRAM solver
