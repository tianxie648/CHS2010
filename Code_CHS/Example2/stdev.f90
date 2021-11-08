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

    !Measurement Equations
    !First, the betas (measurement equations)
    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            ntheta = ntheta + 1     !x1
            ntheta = ntheta + 1     !x2
        end do
    end do

    !Second, the deltas (anchoring equations)
    t = ntime
    do j = 1, nanch
        ntheta = ntheta + 1         !x1
        ntheta = ntheta + 1         !x2
    end do

    !Third, the variances in measurement equations
    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            ntheta = ntheta + 1 
        end do
    end do

    !Fourth, the variances in the linear anchoring equations
    t = ntime
    do j = 1, nanch
        if (PROBANCH(j).ne.1) then
            ntheta = ntheta + 1 
        end if
    end do

    !Fifth, the factor loadings in the measurement equations
    t = 1
    ntheta = ntheta + 1     !Z(t,2,1) = 1.0d0
    ntheta = ntheta + 1     !Z(t,3,1) = 1.0d0
    ntheta = ntheta + 1     !Z(t,5,2) = 1.0d0  
    ntheta = ntheta + 1     !Z(t,6,2) = 1.0d0  
    ntheta = ntheta + 1     !Z(t,8,3) = 1.0d0  
    ntheta = ntheta + 1     !Z(t,9,3) = 1.0d0  

    do t = 2, ntime
        ntheta = ntheta + 1     !Z(t,2,1) = 1.0d0
        ntheta = ntheta + 1     !Z(t,3,1) = 1.0d0
        ntheta = ntheta + 1     !Z(t,5,2) = 1.0d0  
        ntheta = ntheta + 1     !Z(t,6,2) = 1.0d0  
    end do

    !Sixth, the factor loadings in the anchoring equations
    ntheta = ntheta + 1         !alpha(1,1) = 1.0d0

    !Variance of Shocks in Factors
    do s = 1, nstage
        ntheta = ntheta + 1     !Q(s,1,1)
        ntheta = ntheta + 1     !Q(s,2,2)
    end do

    !Technology Parameters
    do s = 1, nstage
        ntheta = ntheta + 1     !G(s,1,1)
        ntheta = ntheta + 1     !G(s,1,2)   
        ntheta = ntheta + 1     !G(s,1,3)   
        ntheta = ntheta + 1     !G(s,2,2)
    end do

    do s = 1, nstage
        ntheta = ntheta + 1     !phi(s,1)
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
    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            theta(ntheta+1) = beta(t,k,1)   !x1
            ntheta = ntheta + 1 
            theta(ntheta+1) = beta(t,k,2)   !x2
            ntheta = ntheta + 1 
        end do
    end do
 
    t = ntime
    do j = 1, nanch
        theta(ntheta+1) = delta(j,1)        !x1
        ntheta = ntheta + 1 
        theta(ntheta+1) = delta(j,2)        !x2
        ntheta = ntheta + 1 
    end do

    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            theta(ntheta+1) = H(t,k,k)      !variance
            ntheta = ntheta + 1 
        end do
    end do

    do j = 1, nanch
        if (PROBANCH(j).ne.1) then
            theta(ntheta+1) = HANCH(j)      !variance
            ntheta = ntheta + 1 
        end if
    end do

    t = 1
    theta(ntheta+1) = Z(t,2,1)
    ntheta = ntheta + 1
    theta(ntheta+1) = Z(t,3,1)
    ntheta = ntheta + 1
    theta(ntheta+1) = Z(t,5,2)
    ntheta = ntheta + 1
    theta(ntheta+1) = Z(t,6,2)
    ntheta = ntheta + 1
    theta(ntheta+1) = Z(t,8,3)
    ntheta = ntheta + 1
    theta(ntheta+1) = Z(t,9,3)
    ntheta = ntheta + 1

    do t = 2, ntime
        theta(ntheta+1) = Z(t,2,1)
        ntheta = ntheta + 1
        theta(ntheta+1) = Z(t,3,1)
        ntheta = ntheta + 1
        theta(ntheta+1) = Z(t,5,2)
        ntheta = ntheta + 1
        theta(ntheta+1) = Z(t,6,2)
        ntheta = ntheta + 1
    end do
    
    theta(ntheta+1) = alpha(1,1)    
    ntheta = ntheta + 1
    
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
        theta(ntheta+1) = G(s,2,2)
        ntheta  = ntheta + 1
    end do			

    do s = 1, nstage
        theta(ntheta+1) = phi(s,1)
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
    
    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            beta(t,k,1) = theta(ntheta+1)   !x1
            ntheta = ntheta + 1 
            beta(t,k,2) = theta(ntheta+1)   !x2
            ntheta = ntheta + 1 
        end do
    end do
 
    t = ntime
    do j = 1, nanch
        delta(j,1) = theta(ntheta+1)        !x1
        ntheta = ntheta + 1 
        delta(j,2) = theta(ntheta+1)        !x2
        ntheta = ntheta + 1 
    end do

    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            H(t,k,k) = theta(ntheta+1)      !variance
            ntheta = ntheta + 1 
        end do
    end do

    do j = 1, nanch
        if (PROBANCH(j).ne.1) then
            HANCH(j) = theta(ntheta+1)      !variance
            ntheta = ntheta + 1 
        end if
    end do

    !Period 1
    t = 1
    Z(t,2,1) = theta(ntheta+1)
    ntheta = ntheta + 1
    Z(t,3,1) = theta(ntheta+1)
    ntheta = ntheta + 1
    Z(t,5,2) = theta(ntheta+1)
    ntheta = ntheta + 1
    Z(t,6,2) = theta(ntheta+1)
    ntheta = ntheta + 1
    Z(t,8,3) = theta(ntheta+1)
    ntheta = ntheta + 1
    Z(t,9,3) = theta(ntheta+1)
    ntheta = ntheta + 1

    do t = 2, ntime
        Z(t,2,1) = theta(ntheta+1)
        ntheta = ntheta + 1
        Z(t,3,1) = theta(ntheta+1)
        ntheta = ntheta + 1
        Z(t,5,2) = theta(ntheta+1)
        ntheta = ntheta + 1
        Z(t,6,2) = theta(ntheta+1)
        ntheta = ntheta + 1
    end do
    
    alpha(1,1) = theta(ntheta+1)
    ntheta = ntheta + 1

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
        G(s,2,2) = theta(ntheta+1)
        ntheta  = ntheta + 1
    end do			

    do s = 1, nstage
        phi(s,1) = theta(ntheta+1)
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

    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            sdbeta(t,k,1) = dsqrt(mattheta(ntheta+1,ntheta+1))   !x1
            ntheta = ntheta + 1 
            sdbeta(t,k,2) = dsqrt(mattheta(ntheta+1,ntheta+1))   !x2
            ntheta = ntheta + 1 
        end do
    end do
 
    t = ntime
    do j = 1, nanch
        sddelta(j,1) = dsqrt(mattheta(ntheta+1,ntheta+1))        !x1
        ntheta = ntheta + 1 
        sddelta(j,2) = dsqrt(mattheta(ntheta+1,ntheta+1))        !x2
        ntheta = ntheta + 1 
    end do

    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            sdH(t,k,k) = dsqrt(mattheta(ntheta+1,ntheta+1))      !variance
            ntheta = ntheta + 1 
        end do
    end do

    do j = 1, nanch
        if (PROBANCH(j).ne.1) then
            sdHANCH(j) = dsqrt(mattheta(ntheta+1,ntheta+1))      !variance
            ntheta = ntheta + 1 
        end if
    end do

    !Period 1
    t = 1
    sdZ(t,2,1) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1
    sdZ(t,3,1) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1
    sdZ(t,5,2) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1
    sdZ(t,6,2) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1
    sdZ(t,8,3) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1
    sdZ(t,9,3) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1
    
    do t = 2, ntime
        sdZ(t,2,1) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta = ntheta + 1
        sdZ(t,3,1) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta = ntheta + 1
        sdZ(t,5,2) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta = ntheta + 1
        sdZ(t,6,2) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta = ntheta + 1
    end do

    sdalpha(1,1) = dsqrt(mattheta(ntheta+1,ntheta+1))
    ntheta = ntheta + 1

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
        sdG(s,2,2) = dsqrt(mattheta(ntheta+1,ntheta+1))
        ntheta  = ntheta + 1
    end do			

    do s = 1, nstage
        sdphi(s,1) = dsqrt(mattheta(ntheta+1,ntheta+1))
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