program outputdata
implicit none

  integer :: i
  REAL, PARAMETER :: PI = 3.14159265359

  open(1, file = 'data.dat', status = 'new')
  do i=0,5000
    write(1,*) real(2*PI*real(i)/5000), SIN(2*PI*real(i)/5000)
  end do

  close(1)

end program outputdata
