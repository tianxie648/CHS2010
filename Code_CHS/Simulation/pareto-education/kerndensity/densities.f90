PROGRAM MAIN
USE GLOBVAR
USE PROBABILITY
USE NONPARAMETRIC
USE STATISTICS
IMPLICIT NONE

! First index is cond set (eg conditional on being a dropout...) the second index is the counterfactual measure

integer :: i
integer :: j
integer :: k
real(8) :: plog(n_bins,n_variables)
real(8) :: point(n_bins)
real(8) :: step
CHARACTER(len=40) :: temp1
CHARACTER(len=40) :: temp2 

CALL INIT
write(*,*) 'done init'

point(1) =  0.5d0
point(n_bins) = 2.5d0
step = (point(n_bins)-point(1))/dble(n_bins-1)
do i = 2, n_bins-1
	point(i) = point(i-1) + step
end do

do k = 1, n_variables
	write(*,*)k
	plog(:,k) = KERNDENS(dataset(:,k),point,0.075d0,.TRUE.)
end do
OPEN(3,file='out/plog.out')
DO j = 1, n_bins
    WRITE(3,'(<n_variables+1>F16.8)') point(j),plog(j,:)
END DO
CLOSE(3)

END PROGRAM MAIN
