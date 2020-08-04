! Authors: John Winnicki [jw858@cornell.edu]
! Purpose: Vector field solver program implemented using the Decoupled Particle Tracking Architecture shared by Theo

! To run this program, compile the tracker.f90 and solver.f90 scripts as solver and tracker, and then run: "mpiexec -n 6 ./programscript"
! Program script contains a very simple bash script which uses the ${PMI_RANK} value. We use the fact that the MPI rank is known in the
! environment as PMI_RANK. As a result, we can simply use the following logic: If the rank is 0 or 1, run tracker. Otherwise, run solver.
! As seen above, we run the script 6 times, and so there will be 2 processes assigned to run tracker and 4 processes to solver.

program solver

use mpi

IMPLICIT NONE

!******************SEE TRACKER FOR EXPLANATIONS OF VARIABLES**************
INTEGER :: fin_step = 500
INTEGER :: curr_step = 0
REAL, DIMENSION(2, 2500) :: vectors = 0.
integer rank, ierr

INTEGER st_intercomm1, st_intercomm2

REAL :: x1 = 0
REAL :: y1 = 0
REAL :: x2 = 0
REAL :: y2 = 0
REAL :: x3 = 0
REAL :: y3 = 0
REAL :: x4 = 0
REAL :: y4 = 0

INTEGER intercomm_rank

INTEGER color, solve_track_comm, status(MPI_STATUS_SIZE)

INTEGER :: tag = 22

REAL :: particle_pos_x = 1.
REAL :: particle_pos_y = 1.

INTEGER :: tracker_num = 2
INTEGER :: solver_num = 4




!LESSON LEARNED: TAGS ARE REALLY IMPORTANT. THEY MUST BE THE SAME THROUGHOUT THE COMMUNICATION CHANNELS.

! In essence, we create an intercommunicator. However, the intercommunicator actually produces 2 different communicators.
!Each of these communicators will be sending something from a certain rank within that communicator. In this case, we have 2 processes.
!One is rank 0, the other is rank 1. We use SPLIT to create 2 new communicators, each housing one process.
!Then, we call CREATE INTERCOMMUNICATOR for each, differently depending on the rank. If it is of rank 0, then we create intercomm1.
!If it is of rank 1, then we create intercomm2. Note that in every communication (intercomm create, send, recv), we always use the same tag.
!We then transmit the value of x from using the local intercommunicator to the rank of the value in the other intercommunicator.
!So we call send using the local intercommunicator produced (because NOTE that this is the only one that even exists on this machine).
!The rank we specify though, is the rank of the process in the OTHER communicator.

CALL MPI_INIT(ierr)
CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)

color = 1

CALL MPI_Comm_split(MPI_COMM_WORLD, color, rank, solve_track_comm, ierr);

! The explanation in tracker should suffice. I will add here that 0 is hard coded to be the rank of the lead tracker process, since
! this value is guaranteed by the bash script we set up.
CALL MPI_Intercomm_create(solve_track_comm, 0, MPI_COMM_WORLD, 0, tag, st_intercomm2, ierr);

CALL MPI_Comm_rank(st_intercomm2, intercomm_rank, ierr)

!The above is the same as tracker, hence see Tracker for a more thorough explanation

DO WHILE (curr_step<fin_step)
    IF(intercomm_rank .eq. 0) THEN

        ! The following four logical statements are all essentially the same, but for the
        ! four different quadrants that the coordinate system. I have arbitrarily assigned
        ! Each of the four quadrants of the coordinate system to a rank in the solver group.
        ! Now, assuming we know each rank, we simply calculate a vector and send that vector.
        ! Here, we use a simple constant vector, (-1, 1) for the first quadrant. The goal is
        ! Just to draw a rotated square and have the particle follow that path.

!        x1 = COS(curr_step * 0.1)
!        y1 = SIN(curr_step * 0.1)

        x1 = -1.
        y1 = 1.

        ! MPI_SEND is quite standard: x1, 1, MPI_REAL are all the same as in tracker.f90. 0 and 1 are used
        ! for the fourth argument, and these represent the rank of the tracker in the TRACKER group.
        ! tag, st_intercomm2, and ierr are all the same as in tracker.f90.

        CALL MPI_SEND(x1, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y1, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)

        CALL MPI_SEND(x1, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y1, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)

    ELSE IF (intercomm_rank .eq. 1) THEN
!        x2 = COS( curr_step * 0.1 + 2.1)
!        y2 = SIN( curr_step * 0.1 + 2.1)

        x2 = -1.
        y2 = -1.

        CALL MPI_SEND(x2, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y2, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)

        CALL MPI_SEND(x2, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y2, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)

    ELSE IF (intercomm_rank .eq. 2) THEN
!        x3 = COS( curr_step * 0.1 + 1)
!        y3 = SIN( curr_step * 0.1 + 1)

        x3 = 1.
        y3 = -1.

        CALL MPI_SEND(x3, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y3, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)

        CALL MPI_SEND(x3, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y3, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)

    ELSE
!        x4 = COS( curr_step * 0.1)
!        y4 = SIN( curr_step * 0.1)

        x4 = 1.
        y4 = 1.

        CALL MPI_SEND(x4, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y4, 1, MPI_REAL, 0, tag, st_intercomm2, IERR)

        CALL MPI_SEND(x4, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)
        CALL MPI_SEND(y4, 1, MPI_REAL, 1, tag, st_intercomm2, IERR)

    END IF
    curr_step = curr_step + 1
END DO

!Standard ending functions
call MPI_FINALIZE(ierr)

END PROGRAM solver
