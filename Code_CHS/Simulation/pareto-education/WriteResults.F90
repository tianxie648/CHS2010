module WriteResults
use globvar
implicit none

contains

subroutine estimates
implicit none
    integer :: j
    CHARACTER(len=40) :: temp1,temp2 

    open(1,file='simulation.out')
    do j = 1, nind
        write(1,'(2I,7f16.8)')j,school(j),points(j,:),x(j,1),x(j,2)  
    end do
    close(1)
      
end subroutine estimates


end module WriteResults