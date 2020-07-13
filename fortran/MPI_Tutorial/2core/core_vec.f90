program core_vec

!Another lesson learned: Always use different tags when working with different sets of data

use mpi

IMPLICIT NONE

!IMPORTANT: TO CHANGE NUMBER OF PERIODS, YOU MUST CHANGE T_NUM, DIMENSION OF PARTICLES ARRAY,
!AND DIMENSIONS OF PARTICLES ARRAY IN CHECK_ACTIVE SUBROUTINE AND RANK_DET SIZE

!IF YOU CHANGE THE STARTING LOCATION, MAKE SURE THE INTIALIZED RANK_DET(0) ELEMENT IS CORRECT

!t_num is number of times particle's position is calculated, i and j are for loop variable,
!mult is the multiplication factor
integer :: t_num = 2500 !Number of periods
integer i !for loop
integer j !for loop
REAL mult !multiplication factor
!particles is an array of tuples represented the particle at various positions
REAL, DIMENSION(2, 2500) :: particles
REAL temp1 !temporary x coordinate storage location
REAL temp2 !temporary y coordinate storage location

integer old_comm, new_comm, ndims, ierr !world communicator, new cartesian communicator,
!number of dimensions of cartesian communicator, error integers (respectively)
logical periods(0:1), reorder !periods and reorder fields for new communicator construction
integer size !size of world communicator
integer dims(0:1) !dimensions integer array to be used to create the
!dimensions field for new cartesian communicator
integer rank, status(MPI_STATUS_SIZE) !processor rank, coordinates, status integers

LOGICAL active !boolean signifying if processor should be calculating the particle's position
integer :: pos = 1 !position on particles array to calculate (more simply, the time step). Initialized to 1.
LOGICAL :: end = .FALSE. !Boolean signifying whether the calculations should end
integer end_request_0 !requests variable to check status of irecv and isend for processor 0
INTEGER end_request_1 !requests variable to check status of irecv and isend for processor 1

INTEGER, DIMENSION(2500) :: rank_det


!Set up the cartesian communicator. This is a cartesian coordinator which
!has 2 nodes, at least that's how I'm assuming it is since my computer uses that"
CALL MPI_INIT(ierr)
old_comm = MPI_COMM_WORLD
call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
ndims = 2
periods(0) = .true.
periods(1) = .false.
reorder = .true.
dims = 0
CALL MPI_Dims_create(size, 2, dims, ierr)
CALL MPI_Cart_create(old_comm, ndims, dims, periods, reorder, new_comm, ierr)

CALL MPI_Comm_rank(new_comm, rank,ierr)

!Initalize Particle position. Notice we start particle 1,1 at -3. See check_active for when to use which processor"
particles = 0.
particles(1,1) = 10.
particles(2,1) = 15.

rank_det=0
rank_det(1) = 1

!Initialize the processor to be active or not active. By active, we
!mean that the processor is handling calculations concerning the particle
active = .FALSE.
CALL check_active(rank, active, particles, pos)
write(*,*)"First check done"

!A while loop is initialized: while the timestep is less than t_num (the set number of time steps from the beginning), do the following
DO WHILE (pos < t_num)
    mult = 0.05

!    write(*,100) rank, active
!    100 FORMAT("Inside while loop and rank is: ", I0, " ******** Active status is: ", L)

!If the processor is deemed active, then it should calculate the next position of the
!particle and record it in the particles array
    IF (active) THEN
        pos = pos+1
!Here, we call the vec_field subroutine to find the direction of motion based on location and place that in temp1, temp2
        CALL vec_field(particles(1,pos-1), particles(2, pos-1),temp1,temp2)
!Set the particles new position
        particles(1,pos) = particles(1,pos-1) + mult * temp1
        particles(2,pos) = particles(2,pos-1) + mult * temp2
        rank_det(pos) = rank
        write(*,*) "Pos: ", pos, " ********* Rank is: ", rank, " ******** Particle(1,pos) is: ", particles(1,pos)

!Here, we check if the processor's active status has changed. This would be if the
!x-axis position is greater than or equal to 0 in the current settings.
        CALL check_active(rank, active, particles, pos)

!If the processor is not active anymore, send the current position to the other processor (which is presumably active now)
!to take over the job. In addition to this, broadcast the particles array to the other processor so you can use it.
!This broadcast isn't really necessary here, but it is included for future use. If we wanted to use more processors,
!This step would be necessary, rather than a simple send function.
!Note that both the processors are accounted for in this.
        IF( .NOT. active) THEN
            write(*,*) rank, " not active anymore"
            IF (rank.eq.0) THEN
                CALL MPI_SEND( pos, 1, MPI_INTEGER, 1, 17, new_comm, ierr)
                write(*,*) "SENT FROM 0"
                CALL MPI_BCAST(particles,t_num*2,MPI_INTEGER,0,new_comm,ierr) !10 since 5 rows of 2
                CALL MPI_BCAST(rank_det,t_num,MPI_INTEGER,0,new_comm,ierr)
            ELSE
                 CALL MPI_SEND( pos, 1, MPI_INTEGER, 0, 17, new_comm, ierr)
                 write(*,*) "SENT FROM 1"
                 CALL MPI_BCAST(particles,t_num*2,MPI_INTEGER,1,new_comm,ierr) !10 since 5 rows of 2
                 CALL MPI_BCAST(rank_det,t_num,MPI_INTEGER,1,new_comm,ierr)
            END IF
        END IF
!If the processor is not active, it should be awaiting the signal from the other processor. We use a blocking receive in order
!To keep the processor from moving on.
!Once the position and particles are received, we refresh the active status, and it should become active.
    ELSE
        write(*,*) "Waiting for receive now, rank: ", rank
        IF (rank.eq.0) THEN
            CALL MPI_RECV( pos, 1, MPI_INTEGER, 1, 17, new_comm, status, ierr)
            write(*,*) "RECEIVED FROM 1"
            write(*,*) "POS is: ", pos
            CALL MPI_BCAST(particles,t_num*2,MPI_INTEGER,1,new_comm,ierr) !10 since 5 rows of 2
            CALL MPI_BCAST(rank_det,t_num,MPI_INTEGER,1,new_comm,ierr)
            CALL check_active(rank, active, particles, pos)
        ELSE
            CALL MPI_RECV( pos, 1, MPI_INTEGER, 0, 17, new_comm, status, ierr)
            write(*,*) "RECEIVED FROM 0"
            write(*,*) "POS is: ", pos
            CALL MPI_BCAST(particles,t_num*2,MPI_INTEGER,0,new_comm,ierr) !10 since 5 rows of 2
            CALL MPI_BCAST(rank_det,t_num,MPI_INTEGER,0,new_comm,ierr)
            write(*,*) "Broadcast finished ", rank
            CALL check_active(rank, active, particles, pos)
        END IF
    END IF
END DO

!If a processor has left the while loop, it has finished calculations. Here, there are 2 scenarios:
!Either it finished first or it finished second. If it finished, first, then the other one presumably
!Is still waiting to receive a signal to become active again. This is because the one that finished first
!Still satisfies the criteria to be active.
!Now, we need to send a signal to the receiving processor that has a blocking receive.
!Skip the following if statement, and come back to it.
write(*,*)"**************SUCCESSFUL ", rank," EXIT**************"

!So here, we use MPI_IRECV to receive the end buffer. It's important to note
!That this is not a blocking receive. We cannot use a blocking receive because
!That would result in a deadlock, because we don't send this for the first processor
!(This is because a processor has to flag that it is time to end to begin with)
!The same is true with using isend rather than a regular blocking send
IF (rank.eq.0) THEN
    CALL MPI_IRECV(end, 1, MPI_LOGICAL, 1, 20, new_comm, end_request_0, ierr)
ELSE
    CALL MPI_IRECV(end, 1, MPI_LOGICAL, 0, 20, new_comm, end_request_1, ierr)
END IF


!Here, we check if the end flag has been raised. If it has, then the processor arriving here
!Is second and does not need to send the position or broadcast the particles array, since no
!One is stuck in the blocking receive in the while loop anymore.
!If end is not true, then no one has been here yet, and that means a processor is stuck in the blocking array
IF(.not. end) THEN
!Here, we send the pos and particle array so the other processor can get out of the blocking array.
    IF (rank.eq.0) THEN

        WRITE(*,*) "Starting SEND AND BROADCAST FROM 0"
!We then say that it is time to end the calculations. We send the end variable to the other processor.
!See (122-126) for more information on this.

        end = .TRUE.
        call MPI_ISEND(end, 1, MPI_LOGICAL, 1, 20, new_comm, end_request_0, ierr)
        write(*,*) "ENDING ISENT FROM 0"

        call MPI_SEND( pos, 1, MPI_INTEGER, 1, 17, new_comm, ierr)
        CALL MPI_BCAST(particles,t_num*2,MPI_INTEGER,0,new_comm,ierr) !10 since 5 rows of 2
        CALL MPI_BCAST(rank_det,t_num,MPI_INTEGER,0,new_comm,ierr)
        write(*,*) "Finished End SENT FROM 0"
    ELSE
        WRITE(*,*) "Starting SEND AND BROADCAST FROM 1"

        end = .TRUE.
        call MPI_ISEND(end, 1, MPI_LOGICAL, 0, 20, new_comm, end_request_1, ierr)
        write(*,*) "ENDING ISENT FROM 1"

        call MPI_SEND( pos, 1, MPI_INTEGER, 0, 17, new_comm, ierr)
        CALL MPI_BCAST(particles,t_num*2,MPI_INTEGER,1,new_comm,ierr) !10 since 5 rows of 2
        CALL MPI_BCAST(rank_det,t_num,MPI_INTEGER,1,new_comm,ierr)
        write(*,*) "Finished End SENT FROM 1"
    END IF
END IF

IF (rank.eq.0) THEN
    write(*,*) "Writing up text file"
    open(1, file = 'particle_locations.txt', status = 'REPLACE')
    do i = 1, t_num
        write(1,100) particles(1,i), particles(2,i), rank_det(i)
        100 FORMAT (F10.4,",",F10.4,",",I0)
    END DO
END IF

CLOSE(1)

call MPI_FINALIZE(ierr)

end program core_vec



SUBROUTINE check_active(rank, active, particles, pos)
!
! Purpose: To check if the processor is supposed to be calculating the position at the time.
! The logic is relatively simple. Essentially it checks the particle's x coordinate at the pos
! If the x coordinate is less than 0, then rank 0 should be active. Otherwise, rank 1 should be active.
! Again, this assumes only 2 processors.
!
IMPLICIT NONE
!Data Dictionary: All of the definitions are the same as the main program
INTEGER, INTENT(IN) :: rank
REAL, DIMENSION(2, 2500), INTENT(IN) :: particles
INTEGER, INTENT(IN) :: pos
LOGICAL, INTENT(INOUT) :: active

!write(*,*) "Rank: ", rank, " and pos : ", pos, "and particles(1,pos) : ", particles(1,pos)
IF (particles(1,pos) < 0.) THEN
    IF (rank.eq.0) THEN
        active = .TRUE.
    ELSE
        active = .FALSE.
    END IF
ELSE
    IF (rank.eq.0) THEN
        active = .FALSE.
    ELSE
        active = .TRUE.
    END IF
END IF

END SUBROUTINE



SUBROUTINE vec_field(x,y,t1,t2)
!
! Purpose: To calculate the vector field at position (x,y)
!
IMPLICIT NONE
!Data Dictionary
REAL, INTENT(IN) :: x
REAL, INTENT(IN) :: y
REAL, INTENT(OUT) :: t1
REAL, INTENT(OUT) :: t2

t1 = -y
t2 = x

END SUBROUTINE
