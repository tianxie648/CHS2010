module stdev
use globvar
use like_aux
use mappings
implicit none

contains

subroutine dimthetagrad(ntheta)
    implicit none
    integer, intent(out) :: ntheta
    integer :: j,k,t,s,n
    ntheta = 0

    !Variance of Shocks in Factors
    do s = 1, nstage
        ntheta = ntheta + 1       !Q(s,1,1)
        ntheta = ntheta + 1       !Q(s,2,2)
    end do

    !Technology Parameters
    do s = 1, nstage
        ntheta = ntheta + 1       !G(s,1,1)
        ntheta = ntheta + 1       !G(s,1,2)   
        ntheta = ntheta + 1       !G(s,1,3)
        ntheta = ntheta + 1       !G(s,1,4)
        ntheta = ntheta + 1       !G(s,1,5)
        ntheta = ntheta + 1       !G(s,2,1)
        ntheta = ntheta + 1       !G(s,2,2)
        ntheta = ntheta + 1       !G(s,2,3)
        ntheta = ntheta + 1       !G(s,2,4)
        ntheta = ntheta + 1       !G(s,2,5)
    end do

    do s = 1, nstage
        ntheta = ntheta + 1       !phi(s,1)
        ntheta = ntheta + 1       !phi(s,2)
    end do
    
    ntheta = ntheta + 1           !beta(1)
    ntheta = ntheta + 1           !beta(2)
    ntheta = ntheta + 1           !beta(3)
    ntheta = ntheta + 1           !vareduc


    write(*,*)ntheta

end subroutine dimthetagrad

subroutine transform(ntheta,theta)
    implicit none
    integer, intent(inout) :: ntheta
    real(8), intent(out) :: theta(ntheta)
    integer :: j,t,k,s,n
    
    ntheta = 0
    
    do s = 1, nstage
        theta(ntheta+1) = Q(s,1,1)
        ntheta = ntheta + 1
        theta(ntheta+1) = Q(s,2,2)
        ntheta = ntheta + 1        
    end do

    do s = 1, nstage
        theta(ntheta+1) = G(s,1,1)
        ntheta  = ntheta + 1
        theta(ntheta+1) = G(s,1,2)
        ntheta  = ntheta + 1
        theta(ntheta+1) = G(s,1,3)
        ntheta  = ntheta + 1
        theta(ntheta+1) = G(s,1,4)
        ntheta  = ntheta + 1
        theta(ntheta+1) = G(s,1,5)
        ntheta  = ntheta + 1
        theta(ntheta+1) = G(s,2,1)
        ntheta  = ntheta + 1
        theta(ntheta+1) = G(s,2,2)
        ntheta  = ntheta + 1
        theta(ntheta+1) = G(s,2,3)
        ntheta  = ntheta + 1
        theta(ntheta+1) = G(s,2,4)
        ntheta  = ntheta + 1
        theta(ntheta+1) = G(s,2,5)
        ntheta  = ntheta + 1
    end do			

    do s = 1, nstage
        theta(ntheta+1) = phi(s,1)
        ntheta = ntheta + 1
        theta(ntheta+1) = phi(s,2)
        ntheta = ntheta + 1
    end do        

    theta(ntheta+1) = beta(1)
    ntheta  = ntheta + 1
    theta(ntheta+1) = beta(2)
    ntheta  = ntheta + 1
    theta(ntheta+1) = beta(3)
    ntheta  = ntheta + 1
    theta(ntheta+1) = vareduc
    ntheta  = ntheta + 1

end subroutine transform

subroutine getparstdev(ntheta,theta)
    implicit none
    integer, intent(inout) :: ntheta
    real(8), intent(in) :: theta(ntheta)
    integer :: j,t,k,s,n
    real(8) :: temp,aux
    ntheta = 0

    do s = 1, nstage
        Q(s,1,1) = theta(ntheta+1)
        ntheta = ntheta + 1
        Q(s,2,2) = theta(ntheta+1)
        ntheta = ntheta + 1        
    end do

    do s = 1, nstage
        G(s,1,1) = theta(ntheta+1)
        ntheta  = ntheta + 1
        G(s,1,2) = theta(ntheta+1)
        ntheta  = ntheta + 1
        G(s,1,3) = theta(ntheta+1)
        ntheta  = ntheta + 1
        G(s,1,4) = theta(ntheta+1)
        ntheta  = ntheta + 1
        G(s,1,5) = theta(ntheta+1)
        ntheta  = ntheta + 1
        G(s,2,1) = theta(ntheta+1)
        ntheta  = ntheta + 1
        G(s,2,2) = theta(ntheta+1)
        ntheta  = ntheta + 1
        G(s,2,3) = theta(ntheta+1)
        ntheta  = ntheta + 1
        G(s,2,4) = theta(ntheta+1)
        ntheta  = ntheta + 1
        G(s,2,5) = theta(ntheta+1)
        ntheta  = ntheta + 1
    end do			

    do s = 1, nstage
        phi(s,1) = theta(ntheta+1)
        ntheta = ntheta + 1
        phi(s,2) = theta(ntheta+1)
        ntheta = ntheta + 1
    end do        

    beta(1) = theta(ntheta+1)
    ntheta = ntheta + 1
    beta(2) = theta(ntheta+1)
    ntheta = ntheta + 1
    beta(3) = theta(ntheta+1)
    ntheta = ntheta + 1
    vareduc = theta(ntheta+1)
    ntheta = ntheta + 1

end subroutine getparstdev

REAL(8) FUNCTION LOGDENSITY(theta,ntheta)

    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: ntheta
    REAL(8), INTENT(IN) :: theta(ntheta)

    call getparstdev(ntheta,theta)  
    LOGDENSITY = dlog(density(igradient))

END FUNCTION LOGDENSITY

subroutine sdpar(ntheta,mattheta)
    implicit none
    integer, intent(inout) :: ntheta
    real(8), intent(in) :: mattheta(ntheta,ntheta)
    integer :: j,t,k,s,n
    real(8) :: temp,aux
    ntheta = 0

    do s = 1, nstage
        sdQ(s,1,1) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta = ntheta + 1
        sdQ(s,2,2) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta = ntheta + 1        
    end do
      
    do s = 1, nstage
        sdG(s,1,1) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta  = ntheta + 1
        sdG(s,1,2) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta  = ntheta + 1
        sdG(s,1,3) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta  = ntheta + 1
        sdG(s,1,4) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta  = ntheta + 1
        sdG(s,1,5) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta  = ntheta + 1
        sdG(s,2,1) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta  = ntheta + 1
        sdG(s,2,2) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta  = ntheta + 1
        sdG(s,2,3) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta  = ntheta + 1
        sdG(s,2,4) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta  = ntheta + 1
        sdG(s,2,5) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta  = ntheta + 1
    end do			

    do s = 1, nstage
        sdphi(s,1) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta = ntheta + 1
        sdphi(s,2) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta = ntheta + 1
    end do        

    sdbeta(1) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1
    sdbeta(2) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1
    sdbeta(3) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1
    sdvareduc = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1


end subroutine sdpar

end module stdev