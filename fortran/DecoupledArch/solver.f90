program solver

use mpi

IMPLICIT NONE

INTEGER :: fin_step = 2500
INTEGER :: curr_step = 0
REAL, DIMENSION(2, 2500) :: vectors = 0.
integer rank, ierr, request
integer i

!INTEGER, DIMENSION(1) :: tracker_group_list
!INTEGER, DIMENSION(1) :: solver_group_list
INTEGER tracker_group, solver_group, st_intercomm1, st_intercomm2, world_handle

INTEGER tracker_comm, solver_comm

INTEGER tracker_comm_rank, solver_comm_rank, tgsize, sgsize

REAL :: x = 0
REAL :: y = 0

INTEGER intercomm_rank, root_intercomm_rank

INTEGER color, solve_track_comm, status(MPI_STATUS_SIZE)

INTEGER :: tag = 22

REAL :: particle_pos_x = 3.
REAL :: particle_pos_y = 2.



!LESSON LEARNED: TAGS ARE REALLY IMPORTANT. THEY MUST BE THE SAME THROUGHOUT THE COMMUNICATION CHANNELS.

! In essence, we create an intercommunicator. However, the intercommunicator actually produces 2 different communicators. Each of these communicators will be sending something from a certain rank within that communicator. In this case, we have 2 processes. One is rank 0, the other is rank 1. We use SPLIT to create 2 new communicators, each housing one process. Then, we call CREATE INTERCOMMUNICATOR for each, differently depending on the rank. If it is of rank 0, then we create intercomm1. If it is of rank 1, then we create intercomm2. Note that in every communication (intercomm create, send, recv), we always use the same tag. We then transmit the value of x from using the local intercommunicator to the rank of the value in the other intercommunicator. So we call send using the local intercommunicator produced (because NOTE that this is the only one that even exists on this machine). The rank we specify though, is the rank of the process in the OTHER communicator. 

CALL MPI_INIT(ierr)
CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

!CALL MPI_COMM_GROUP(MPI_COMM_WORLD, world_handle, ierr)

!tracker_group_list(1) = 0
!solver_group_list(1) = 1
!
!CALL MPI_Group_incl(world_handle, 1, tracker_group_list, tracker_group, ierr)
!CALL MPI_Group_incl(world_handle, 1, solver_group_list, solver_group, ierr)
!
!CALL MPI_COMM_CREATE_GROUP(MPI_COMM_WORLD, tracker_group, 15, tracker_comm, ierr)
!CALL MPI_COMM_CREATE_GROUP(MPI_COMM_WORLD, solver_group, 161, solver_comm, ierr)

IF(rank .eq. 0) THEN
    color = 0
ELSE
    color = 1
END IF
write(*,*) "color: ", color
CALL MPI_Comm_split(MPI_COMM_WORLD, color, rank, solve_track_comm, ierr);

write(*,*) "here"

IF(color .eq. 0) THEN

    !write(*,*) "Tracker comm: ", solve_track_comm

    CALL MPI_COMM_SIZE(solve_track_comm, tgsize, ierr)
    !write(*,*) tgsize

    CALL MPI_COMM_RANK(solve_track_comm, tracker_comm_rank, ierr)
    !write(*,*) "tracker comm rank: ", tracker_comm_rank

    !write(*,*) "here0"

    open(1, file = 'particle_locations.txt', status = 'REPLACE')

    CALL MPI_INTERCOMM_CREATE(solve_track_comm, 0, MPI_COMM_WORLD, 1, tag, st_intercomm1, ierr)

     !Tracker group is the group we want to link, 0 is the rank of the leader (arbitrarily chosen), MPI_COMM_WORLD is the communicator in which the two "leader"
     ! Processes share (that is, the tracker and solver processes).
!     IN MPI_COMM_WORLD, the rank is 0, and the rank of the tracker is 1. So in the above, we use 1 as the leader rank of the peer communicator.

    CALL MPI_Comm_rank(st_intercomm1, intercomm_rank, ierr)

    write(*,*) "intercomm rank: ", rank, intercomm_rank

!    root_intercomm_rank = intercomm_rank
!    CALL MPI_Bcast(root_intercomm_rank, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

    IF(intercomm_rank .eq. 0) THEN

    DO WHILE (curr_step<fin_step)

        CALL MPI_RECV(x, 1, MPI_REAL, 0, tag, st_intercomm1, status, IERR)
        CALL MPI_RECV(y, 1, MPI_REAL, 0, tag, st_intercomm1, status, IERR)

        particle_pos_x = particle_pos_x + 0.1*x
        particle_pos_y = particle_pos_y + 0.1*y

!        CALL MPI_BCAST(y, 1, MPI_REAL, root_intercomm_rank, st_intercomm, ierr)
        WRITE(1,100) particle_pos_x, particle_pos_y, rank
            100 FORMAT (F10.4,",",F10.4,",",I0)
        curr_step = curr_step + 1
    END DO

    END IF
    
    CLOSE(1)

END IF



IF(color .eq. 1) THEN

!    write(*,*) "Solver comm: ", solve_track_comm

    CALL MPI_COMM_SIZE(solve_track_comm, sgsize, ierr)
!    write(*,*) sgsize

    CALL MPI_COMM_RANK(solve_track_comm, solver_comm_rank, ierr)
!    write(*,*) "solver comm rank: ", solver_comm_rank

!    write(*,*) "here1"

    CALL MPI_Intercomm_create(solve_track_comm, 0, MPI_COMM_WORLD, 0, tag, st_intercomm2, ierr);

    CALL MPI_Comm_rank(st_intercomm2, intercomm_rank, ierr)
    write(*,*) "intercomm rank: ", rank, intercomm_rank
!    CALL MPI_Bcast(root_intercomm_rank, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)


    IF(intercomm_rank .eq. 0) THEN
    DO WHILE (curr_step<fin_step)
        vectors(1, curr_step) = COS(curr_step * 0.1)
        vectors(2, curr_step) = SIN(curr_step * 0.1)

        x = COS(curr_step * 0.1)
        y = SIN(curr_step * 0.1)


        CALL MPI_SEND(x, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)
!        CALL MPI_BCAST(y, 1, MPI_REAL, root_intercomm_rank, st_intercomm, ierr)

!         write(*,200) vectors(1,i), vectors(2,i), rank
!         200 FORMAT (F10.4,",",F10.4,",",I0)
        curr_step = curr_step + 1
    END DO
    END IF

END IF

call MPI_FINALIZE(ierr)

END PROGRAM solver
