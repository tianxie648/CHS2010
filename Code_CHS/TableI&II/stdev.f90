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

    ntheta = ntheta + 1 !alpha(1,1) = 1.0d0
    ntheta = ntheta + 1 !alpha(1,2) = 1.0d0

    !Variance of Shocks in Factors
    do s = 1, nstage
        ntheta = ntheta + 1       !Q(s,1,1)
        ntheta = ntheta + 1       !Q(s,2,2)
    end do

    ntheta = ntheta + 1         !Q(s,3,3)
    ntheta = ntheta + 1         !Q(s,6,6)
    ntheta = ntheta + 1         !G(s,3,3)
    ntheta = ntheta + 1         !G(s,6,6)

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

!    !Then, the parameters of the factor distribution
!    !Weights
!    do n = 1, nemf-1
!        ntheta = ntheta + 1             !W0(n)
!    end do
!
!    !Means
!    do n = 1, nemf-1
!        do j = 1, nfac
!            ntheta = ntheta + 1         !a0(n,nfac)
!        end do
!    end do        
    write(*,*)ntheta

end subroutine dimthetagrad

subroutine transform(ntheta,theta)
    implicit none
    integer, intent(inout) :: ntheta
    real(8), intent(out) :: theta(ntheta)
    integer :: j,t,k,s,n
    
    ntheta = 0
    theta(ntheta+1) = alpha(1,1)    
    ntheta = ntheta + 1
    theta(ntheta+1) = alpha(1,2)    
    ntheta = ntheta + 1
    
    do s = 1, nstage
        theta(ntheta+1) = Q(s,1,1)
        ntheta = ntheta + 1
        theta(ntheta+1) = Q(s,2,2)
        ntheta = ntheta + 1        
    end do

    theta(ntheta+1) = Q(1,3,3)
    ntheta = ntheta + 1
    theta(ntheta+1) = Q(1,6,6)
    ntheta = ntheta + 1
    theta(ntheta+1) = G(1,3,3)
    ntheta  = ntheta + 1
    theta(ntheta+1) = G(1,6,6)
    ntheta  = ntheta + 1

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

!    !Then, the parameters of the factor distribution
!    !Weights
!    do n = 1, nemf-1
!        theta(ntheta+1) = W0(n) !W0(n)
!        ntheta = ntheta + 1                 
!    end do
!
!    !Means
!    do n = 1, nemf-1
!        do j = 1, nfac
!            theta(ntheta+1) = a0(n,j) 
!            ntheta = ntheta + 1
!        end do
!    end do

end subroutine transform

subroutine getparstdev(ntheta,theta)
    implicit none
    integer, intent(inout) :: ntheta
    real(8), intent(in) :: theta(ntheta)
    integer :: j,t,k,s,n
    real(8) :: temp,aux
    ntheta = 0

    alpha(1,1) = theta(ntheta+1)   !1.0d0
    ntheta = ntheta + 1
    alpha(1,2) = theta(ntheta+1)   !1.0d0
    ntheta = ntheta + 1

    do s = 1, nstage
        Q(s,1,1) = theta(ntheta+1)
        ntheta = ntheta + 1
        Q(s,2,2) = theta(ntheta+1)
        ntheta = ntheta + 1        
    end do

    s = 1
    Q(s,3,3) = theta(ntheta+1)
    ntheta = ntheta + 1
    Q(s,6,6) = theta(ntheta+1)
    ntheta = ntheta + 1
    G(s,3,3) = theta(ntheta+1)	        
    ntheta  = ntheta + 1
    G(s,6,6) = theta(ntheta+1)	        
    ntheta  = ntheta + 1

    do s = 2, nstage
        Q(s,3,3) = Q(1,3,3)
        Q(s,6,6) = Q(1,6,6)
        G(s,3,3) = G(1,3,3) 
        G(s,6,6) = G(1,6,6) 
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

!    !Then, the parameters of the factor distribution
!    !Weights
!    do n = 1, nemf-1
!        W0(n) = theta(ntheta+1)     !W0(n)
!        ntheta = ntheta + 1                 
!    end do
!    !Imposing sum of the weights is one
!    aux = sum(W0(1:nemf-1)) + 1.0d0
!    do n = 1, nemf-1
!        W0(n) = W0(n)/aux
!    end do
!    W0(nemf) = 1.0d0/aux
!
!    !Means
!    do n = 1, nemf-1
!        do j = 1, nfac
!            a0(n,j) = theta(ntheta+1)
!            ntheta = ntheta + 1
!        end do
!    end do
!    do j = 1, nfac
!        aux = 0.0d0
!        do n = 1, nemf-1
!            aux = aux + W0(n)*a0(n,j)
!        end do
!        a0(nemf,j) = -aux/W0(nemf)
!    end do

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

    sdalpha(1,1) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1
    sdalpha(1,2) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1

    do s = 1, nstage
        sdQ(s,1,1) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta = ntheta + 1
        sdQ(s,2,2) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta = ntheta + 1        
    end do

    s = 1
    sdQ(s,3,3) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1
    sdQ(s,6,6) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1
    sdG(s,3,3) = dsqrt(mattheta(ntheta+1,ntheta+1))	        
    ntheta  = ntheta + 1
    sdG(s,6,6) = dsqrt(mattheta(ntheta+1,ntheta+1))	        
    ntheta  = ntheta + 1

    do s = 2, nstage
        sdQ(s,3,3) = sdQ(1,3,3)
        sdQ(s,6,6) = sdQ(1,6,6)
        sdG(s,3,3) = sdG(1,3,3)
        sdG(s,6,6) = sdG(1,6,6)
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

!    !Then, the parameters of the factor distribution
!    !Weights
!    do n = 1, nemf-1
!        sdW0(n) = dsqrt(mattheta(ntheta+1,ntheta+1))      !W0(n)
!        ntheta = ntheta + 1                 
!    end do
!
!    !Means
!    do n = 1, nemf-1
!        do j = 1, nfac
!            sda0(n,j) = dsqrt(mattheta(ntheta+1,ntheta+1))  
!            ntheta = ntheta + 1
!        end do
!    end do

end subroutine sdpar


end module stdev