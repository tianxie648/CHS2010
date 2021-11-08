program main
use globvar
use normalization
use initialize 
use minimization
use pareto_mod
use matrix
use mappings
use WriteResults
implicit none

real(8), allocatable :: theta(:)
integer :: ntheta,j
real(8) :: temp,aux

allocate(x(nind,ntime))
allocate(points(nind,nstate))

call normalize_parameters
call init

call dimtheta(ntheta)
write(*,*)ntheta

allocate(theta(ntheta))
                    
pause

OPEN(1,file="point_fix.out")
DO j = 1, ntheta
    READ(1,fmt=*) theta(j)
END DO
CLOSE(1)

call getpar(ntheta,theta)
write(*,*)ntheta

call dfpmin(theta,ntheta,1.0D-6,PARETO)

deallocate(theta)

call estimates

deallocate(x)
deallocate(points)
end program main