module mappings
use globvar
implicit none

contains

subroutine dimtheta(ntheta)
    implicit none
    integer, intent(out) :: ntheta
    integer :: j,t
    ntheta = 0
    do j = 1, nind-1
        do t = 1, ntime
            ntheta = ntheta + 1
        end do
    end do
    do t = 1, ntime-1
        ntheta = ntheta + 1
    end do                            
  
    write(*,*)ntheta

end subroutine dimtheta
   

subroutine getpar(ntheta,theta)
    implicit none
    integer, intent(inout) :: ntheta
    real(8), intent(in) :: theta(ntheta)
    integer :: j,t
    real(8) :: temp,aux

    ntheta = 0
    aux = 0.0d0
    do j = 1, nind-1
        do t = 1, ntime
            x(j,t) = dexp(theta(ntheta+1))
            ntheta = ntheta + 1
            aux = aux + x(j,t)
        end do
    end do
    do t = 1, ntime-1
        x(nind,t) = dexp(theta(ntheta+1))
        ntheta = ntheta + 1
        aux = aux + x(nind,t)
    end do        
    aux = 1.0d0 + aux
    x(nind,ntime) = 1.0d0
    
    do j = 1, nind
        do t = 1, ntime
            x(j,t) = x(j,t)/aux
        end do
    end do
    do j = 1, nind
        do t = 1, ntime
            x(j,t) = budget*x(j,t)
        end do
    end do                                        
  
end subroutine getpar
       
    
end module mappings		
