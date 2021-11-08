module initialize
use globvar
use probability
implicit none

contains

subroutine init
implicit none

    integer, parameter :: nvar = 174                        !Number of variables in dataset
    character(len=100) :: datafile='data.raw'       !Dataset file
    real(8) :: dataset(nind,ntime,nvar)						!The dataset
    integer :: i
    integer :: t
    integer :: j
    integer :: s
    integer :: ly(nmea)                                     !location of cogninitive measurements
    integer :: lx(ntime,nmea,nx)
    integer :: ldy(nmea), initial
    integer :: lyanch(nanch)
    integer :: ldanch(nanch)

    !Number of Measurement Equations per Period
    nequation = (/30,15,16,23,19,19,18,18/)
    stage = (/1,1,1,2,2,2,2,2/)

    !Location of Measurement Variables and Missing Indicators
    ly( 1:23)   = (/21,22,23,24,25,26,27,28,29,31,32,33,34,35,36,37,38,39,40,41,42,43,44/)
    ly(24:46)   = (/45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67/)
    ly(47:nmea) = (/68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83/)
    do j = 1, nmea
        ldy(j) = ly(j)+77
    end do

    !Location of Anchoring Equations
    lyanch = (/86,91,93,97/)
    ldanch = (/163,168,170,174/)
    PROBANCH = (/0,1,1,1/)

    !Measurement Equation in each period
    t = 1
    eqindex(t,1:nequation(t))=(/1,2,3,18,19,27,28,29,30,31,32,33,44,45,46,47,48,49,50,51,52,54,55,56,57,58,59,60,61,62/)
    t = 2
    eqindex(t,1:nequation(t))=(/3,4,5,14,15,16,18,19,27,28,29,30,31,32,33/)
    t = 3
    eqindex(t,1:nequation(t))=(/3,6,14,15,16,20,21,22,23,24,27,28,29,32,34,35/)
    t = 4
    eqindex(t,1:nequation(t))=(/6,7,8,9,20,21,22,23,24,27,28,29,32,34,35,36,37,38,39,40,41,42,43/)
    t = 5
    eqindex(t,1:nequation(t))=(/7,8,9,20,21,22,23,24,28,29,32,36,37,38,39,40,41,42,43/)
    t = 6
    eqindex(t,1:nequation(t))=(/7,8,9,20,21,22,23,24,28,29,32,36,37,38,39,40,41,42,43/)
    t = 7
    eqindex(t,1:nequation(t))=(/7,8,9,20,21,22,23,24,28,32,36,37,38,39,40,41,42,43/)
    t = 8
    eqindex(t,1:nequation(t))=(/7,8,9,20,21,22,23,24,28,32,36,37,38,39,40,41,42,43/)

    !Any measurement equations nonlinear in the factors?
    nonlinear = 0
    typefunct = 0
    
    !mx(period t, equation k)
    mx = nx

    do t = 1, ntime
        do j = 1, nmea
            lx(t,j,1:nx) = (/7,12,13,11,14,20/)
        end do
    end do

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
        YANCH(i,:)  = dataset(i,ntime,lyanch)
        dYANCH(i,:) = int(dataset(i,ntime,ldanch))
    end do

    do i = 1, nind
        do t = 1, ntime
            do j = 1, nmea
                X(i,t,1:mx(t,j)) = dataset(i,t,lx(t,j,:))
            end do
        end do
    end do

    do i = 1, nind
        caseid(i) = dataset(i,1,1)
    end do        
  
end subroutine init

end module initialize