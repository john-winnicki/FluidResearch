program core_mini

!LESSON LEARNED: YOU MUST USE DIMS_CREATE in order to create the dimensions of the cart
!Lesson learned: You have to zero the dims array before passing it, since dims is both an input and output. If it isn't zero, it
!constrains certain values
IMPLICIT NONE

!t_num is number of times particle's position is calculated, i and j are for loop variable, p_num is the nubmer of particles
!mult is the multiplication factor
integer :: t_num = 3000
integer i
integer j
REAL mult
integer :: p_num = 1
!particles is an array of tuples represented the particles starting positions (second dimension is p_num)
REAL, DIMENSION(2, 1) :: particles
REAL temp1
REAL temp2

particles(1,1) = 10.
particles(2,1) = 15.


open(1, file = 'data1.txt', status = 'REPLACE')

do i = 0, t_num
    mult = 0.01
    do j = 1, p_num
        CALL vec_field(particles(1,j), particles(2, j),temp1,temp2)
        particles(1,j) = particles(1,j) + mult * temp1
        particles(2,j) = particles(2,j) + mult * temp2
        write(1,100) particles(1,j), particles(2,j)
        100 FORMAT (F10.5,",",F10.5)
    end do
end do

close(1)

end program core_mini



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
