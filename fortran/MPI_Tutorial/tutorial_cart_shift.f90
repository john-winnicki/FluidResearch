program tutorial_cart_shift 

include "mpif.h"
integer old_comm, new_comm, ndims, ierr 
integer dim_size(0:1)
logical periods(0:1), reorder

integer ERR
CALL MPI_INIT(ERR)

old_comm = MPI_COMM_WORLD
ndims = 2 
dim_size(0) = 3 
dim_size(1) = 2 
periods(0) = .true. 
periods(1) = .false. 
reorder = .true.

call MPI_Cart_create(old_comm, ndims, dim_size, periods, reorder, new_comm, ierr)
    
CALL MPI_FINALIZE(ERR)

end program tutorial_cart_shift

