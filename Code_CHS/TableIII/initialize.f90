module initialize
use globvar
implicit none

contains

subroutine init
implicit none

    integer, parameter :: nvar = 12                 !Number of variables in dataset
    character(len=100) :: datafile='simple.raw'     !Dataset file
    real(8) :: dataset(nind,ntime,nvar)				!The dataset
    integer :: i
    integer :: t
    integer :: j
    integer :: s
    integer :: ly(ny)                               !location of cogninitive measurements
    integer :: lx(nx)
    integer :: ldy(ny) 
    integer :: ldx
    integer :: initial
    integer :: leduc
    integer :: ldeduc

    !Location of Measurement Variables and Missing Indicators
    ly  = (/3,4/)
    lx  = (/3,4,5,6,7/)
    ldy = (/8,9/)
    ldx  = 10 
    leduc = 11
    ldeduc = 12
    stage = (/1,1,1,2,2,2,2,2/)

    OPEN(1,file=datafile)
    DO i = 1, nind
        DO t = 1, ntime
            READ(1,fmt=*) dataset(i,t,:)
        END DO
    END DO
    CLOSE(1)

    do i = 1, nind
        do t = 1, ntime
            Y(i,t,:) = dataset(i,t,ly)
            dY(i,t,:) = int(dataset(i,t,ldy))
        end do
    end do
    
    do i = 1, nind
        do t = 1, ntime
            X(i,t,:) = dataset(i,t,lx)
            dx(i,t) = dataset(i,t,ldx)
        end do
    end do

    do i = 1, nind
        educ(i) = dataset(i,ntime,leduc)
        deduc(i) = int(dataset(i,ntime,ldeduc))
    end do        
  
end subroutine init

end module initialize