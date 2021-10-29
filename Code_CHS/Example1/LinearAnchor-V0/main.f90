program main
use globvar
use initialize 
use minimization
use likelihood
use matrix
use mappings
use normalizations
use WriteResults
use stdev
implicit none

real(8), allocatable :: theta(:)
real(8), allocatable :: grad(:)
real(8), allocatable :: mat(:,:)
real(8), allocatable :: thetagrad(:)
integer :: ntheta,j,k1,k2,nthetagrad
real(8) :: temp,aux
real(8) :: afilter(ntime,nfac)

call init
call normalize_parameters

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

call estimates

call dfpmin(theta,ntheta,1.0D-6,LOGLIKELIHOOD)

call estimates

call dimthetagrad(nthetagrad)
allocate(thetagrad(nthetagrad))
allocate(grad(nthetagrad))
allocate(mat(nthetagrad,nthetagrad))

call transform(nthetagrad,thetagrad)
mat = 0.0d0
do igradient = 1, nind
    write(*,*)'i =', igradient, 'out of =', nind
    aux = logdensity(thetagrad,nthetagrad)
    grad = GRADIENT(thetagrad,nthetagrad,aux,logdensity)
    do k1 = 1, nthetagrad
        do k2 = 1, nthetagrad
            mat(k1,k2) = mat(k1,k2) + grad(k1)*grad(k2)
        end do
    end do
end do

open(100,file='mat.out')
do j = 1, nthetagrad
    write(100,*)mat(j,:)
end do
close(100)
    
mat = matinv(mat)
call sdpar(nthetagrad,mat)

call estimates

end program main