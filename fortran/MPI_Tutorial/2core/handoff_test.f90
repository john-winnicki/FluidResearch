program handoff_test

use mpi

IMPLICIT NONE

!t_num is number of times particle's position is calculated, i and j are for loop variable, p_num is the nubmer of particles
!mult is the multiplication factor
integer :: t_num = 10
integer i
integer j
REAL mult
integer :: p_num = 1
!particles is an array of tuples represented the particle at various positions
REAL, DIMENSION(2, 10) :: particles
REAL temp1
REAL temp2

integer old_comm, new_comm, ndims, ierr
logical periods(0:1), reorder
integer size
integer old_comm_SIZE
integer dims(0:1)
integer rank, coords(0:1), status(MPI_STATUS_SIZE)

LOGICAL active
integer :: pos = 1
LOGICAL :: end = .FALSE.
integer end_request_0
integer end_request_1



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

!Initalize Particle position
particles = 0.
particles(1,1) = - 3.
particles(2,1) = 0.

active = .FALSE.
CALL check_active(rank, active, particles, pos)
write(*,*)"First check done"

DO WHILE (pos < t_num)

    write(*,100) rank, active
    100 FORMAT("Inside while loop and rank is: ", I0, " ******** Active status is: ", L)

    IF (active) THEN
        pos = pos+1
        particles(1,pos) = particles(1,pos-1) + 1
        write(*,*) "Pos: ", pos, " ********* Rank is: ", rank, " ******** Particle(1,pos) is: ", particles(1,pos)
        CALL check_active(rank, active, particles, pos)
        IF( .NOT. active) THEN
            write(*,*) rank, " not active anymore"
            IF (rank.eq.0) THEN
                CALL MPI_SEND( pos, 1, MPI_INTEGER, 1, 17, new_comm, ierr)
                write(*,*) "SENT FROM 0"
                CALL MPI_BCAST(particles,10,MPI_INTEGER,0,new_comm,ierr) !10 since 5 rows of 2
            ELSE
                 CALL MPI_SEND( pos, 1, MPI_INTEGER, 0, 17, new_comm, ierr)
                 write(*,*) "SENT FROM 1"
                 CALL MPI_BCAST(particles,10,MPI_INTEGER,1,new_comm,ierr) !10 since 5 rows of 2
            END IF
        END IF
    ELSE
        write(*,*) "Waiting for receive now, rank: ", rank
        IF (rank.eq.0) THEN
            CALL MPI_RECV( pos, 1, MPI_INTEGER, 1, 17, new_comm, status, ierr)
            write(*,*) "RECEIVED FROM 1"
            write(*,*) "POS is: ", pos
            CALL MPI_BCAST(particles,10,MPI_INTEGER,1,new_comm,ierr) !10 since 5 rows of 2
            CALL check_active(rank, active, particles, pos)
        ELSE
            CALL MPI_RECV( pos, 1, MPI_INTEGER, 0, 17, new_comm, status, ierr)
            write(*,*) "RECEIVED FROM 0"
            write(*,*) "POS is: ", pos
            CALL MPI_BCAST(particles,10,MPI_INTEGER,0,new_comm,ierr) !10 since 5 rows of 2
            write(*,*) "Broadcast finished ", rank
            CALL check_active(rank, active, particles, pos)
        END IF
    END IF
END DO

write(*,*)"**************SUCCESSFUL ", rank," EXIT**************"

IF (rank.eq.0) THEN
    CALL MPI_IRECV(end, 1, MPI_LOGICAL, 1, 17, new_comm, end_request_0, ierr)
ELSE
    CALL MPI_IRECV(end, 1, MPI_LOGICAL, 0, 17, new_comm, end_request_1, ierr)
END IF


IF(.not. end) THEN
    IF (rank.eq.0) THEN
        call MPI_SEND( pos, 1, MPI_INTEGER, 1, 17, new_comm, ierr)
        CALL MPI_BCAST(particles,10,MPI_INTEGER,0,new_comm,ierr) !10 since 5 rows of 2
        end = .TRUE.
        call MPI_ISEND(end, 1, MPI_LOGICAL, 1, 17, new_comm, end_request_0, ierr)
        write(*,*) "ENDING SENT FROM 0"
    ELSE
        call MPI_SEND( pos, 1, MPI_INTEGER, 0, 17, new_comm, ierr)
        CALL MPI_BCAST(particles,10,MPI_INTEGER,1,new_comm,ierr) !10 since 5 rows of 2
        write(*,*) "ENDING SENT FROM 1, not isend though"
        end = .TRUE.
        call MPI_ISEND(end, 1, MPI_LOGICAL, 0, 17, new_comm, end_request_1, ierr)
        write(*,*) "ENDING SENT FROM 1"
    END IF
END IF

!Write the position of the particle to a text file named data1.txt
!open(1, file = 'data1.txt', status = 'REPLACE')
!do i = 0, t_num
!    mult = 0.01
!    do j = 1, p_num
!        CALL vec_field(particles(1,j), particles(2, j),temp1,temp2)
!        particles(1,j) = particles(1,j) + mult * temp1
!        particles(2,j) = particles(2,j) + mult * temp2
!        write(1,100) particles(1,j), particles(2,j)
!        100 FORMAT (F10.5,",",F10.5)
!    end do
!end do
!close(1)

call MPI_FINALIZE(ierr)

end program handoff_test




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




SUBROUTINE check_active(rank, active, particles, pos)
!
!Purpose: To check if the processor is supposed to be calculating the position at the time
!
IMPLICIT NONE
!Data Dictionary
INTEGER, INTENT(IN) :: rank
REAL, DIMENSION(2, 10), INTENT(IN) :: particles
INTEGER, INTENT(IN) :: pos
LOGICAL, INTENT(INOUT) :: active

write(*,*) "Rank: ", rank, " and pos : ", pos, "and particles(1,pos) : ", particles(1,pos)
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
