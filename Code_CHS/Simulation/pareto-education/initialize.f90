module initialize
use globvar
use probability
implicit none

contains

subroutine init
implicit none

    integer :: i
    real(8) :: aux(nfac)

    stage = (/1,2/)
        
    do i = 1, nind
        aux = 0.0d0
        aux = Sample_Multivariate_Normal(aux,P0)
        points(i,1) = aux(1)
        points(i,2) = aux(2)
        points(i,3) = aux(4)
        points(i,4) = aux(5)
        points(i,5) = aux(6)
    end do

    open(1,file='points.out')
    do i = 1, nind
        write(1,'(I,5F16.8)')i,points(i,:)
    end do
    close(1)

  
end subroutine init

end module initialize