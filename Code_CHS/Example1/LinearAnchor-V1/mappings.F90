module mappings
use globvar
implicit none

contains

subroutine dimtheta(ntheta)
    implicit none
    integer, intent(out) :: ntheta
    integer :: j,k,t,s,n
    ntheta = 0

    open(200,file='check.out')
    !Measurement Equations
    !First, the betas (measurement equations)
    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta      !x1
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta      !x2
        end do
    end do

    !Second, the deltas (anchoring equations)
    t = ntime
    do j = 1, nanch
        ntheta = ntheta + 1
        write(200,*)t,j,ntheta          !x1
        ntheta = ntheta + 1
        write(200,*)t,j,ntheta          !x2
    end do

    !Third, the variances in measurement equations
    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         !variance
        end do
    end do

    !Fourth, the variances in the linear anchoring equations
    t = ntime
    do j = 1, nanch
        if (PROBANCH(j).ne.1) then
            ntheta = ntheta + 1 
            write(200,*)t,j,ntheta         !variance
        end if
    end do

    !Fifth, the factor loadings in the measurement equations
    do t = 1, ntime
        ntheta = ntheta + 1     !Z(t,2,1) = 1.0d0
        ntheta = ntheta + 1     !Z(t,3,1) = 1.0d0
        ntheta = ntheta + 1     !Z(t,5,2) = 1.0d0  
        ntheta = ntheta + 1     !Z(t,6,2) = 1.0d0  
    end do

    !Sixth, the factor loadings in the anchoring equations
    ntheta = ntheta + 1 !alpha(1,1) = 1.0d0

    !Seventh, the Initial Variances Matrix of the Factors
    !Diagonal Elements
    do j = 1, nfac
        ntheta = ntheta + 1         !P0(m,j,j)
    end do
    !Off-Diagonal Elements
    do j = 1, nfac-1
        do k = j+1, nfac
            ntheta = ntheta + 1
        end do
    end do
  
    !Eighth, the Variance of Shocks in Factors
    do s = 1, nstage
        ntheta = ntheta + 1       !Q(s,1,1)
        ntheta = ntheta + 1       !Q(s,2,2)
    end do

    !Ninth, the Technology Parameters
    do s = 1, nstage
        ntheta = ntheta + 1       !G(s,1,1)
        ntheta = ntheta + 1       !G(s,2,2)
    end do
    do s = 1, nstage
        ntheta = ntheta + 1       !phi(s,1)
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

    close(200)

end subroutine dimtheta
   

subroutine getpar(ntheta,theta)
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
            H(t,k,k) = dexp(theta(ntheta+1))  !variance
            ntheta = ntheta + 1 
        end do
    end do

    do j = 1, nanch
        if (PROBANCH(j).ne.1) then
            HANCH(j) = dexp(theta(ntheta+1))  !variance
            ntheta = ntheta + 1 
        end if
    end do

    !Period 1
    do t = 1, ntime
        Z(t,2,1) = theta(ntheta+1)   !1.0d0
        ntheta = ntheta + 1
        Z(t,3,1) = theta(ntheta+1)   !1.0d0
        ntheta = ntheta + 1
        Z(t,5,2) = theta(ntheta+1)   !1.0d0  
        ntheta = ntheta + 1
        Z(t,6,2) = theta(ntheta+1)   !1.0d0  
        ntheta = ntheta + 1
    end do

    alpha(1,1) = theta(ntheta+1)
    ntheta = ntheta + 1

    !Variances
    !Diagonal Elements
    do j = 1, nfac
        P0(1,j,j) = dexp(theta(ntheta+1)) !P0(m,j,j)
        ntheta = ntheta + 1
    end do

    !Variances
    !Off-Diagonal Elements
    do j = 1, nfac-1
        do k = j+1, nfac
            aux = tanh(theta(ntheta+1))
            ntheta  = ntheta + 1
            P0(1,j,k) = aux*dsqrt(P0(1,j,j))*dsqrt(P0(1,k,k))
            P0(1,k,j) = P0(1,j,k)
        end do
    end do

    do n = 2, nemf
        P0(n,:,:) = P0(1,:,:)
    end do

    do s = 1, nstage
        Q(s,1,1) = dexp(theta(ntheta+1))
        ntheta = ntheta + 1
        Q(s,2,2) = dexp(theta(ntheta+1))
        ntheta = ntheta + 1        
    end do
            
    do s = 1, nstage
        G(s,1,1) = dexp(theta(ntheta+1))	
        ntheta  = ntheta + 1
        G(s,2,2) = theta(ntheta+1)
        ntheta  = ntheta + 1

        aux = 1.0d0 + G(s,1,1)
        G(s,1,1) = G(s,1,1)/aux
        G(s,1,2) = 1.0000d0/aux
    end do			

    do s = 1, nstage
        aux = dexp(theta(ntheta+1))
        phi(s,1) = 1.0d0-aux
        ntheta = ntheta + 1
    end do        

!    !Then, the parameters of the factor distribution
!    !Weights
!    do n = 1, nemf-1
!        W0(n) = dexp(theta(ntheta+1))       !W0(n)
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

end subroutine getpar
       
    
end module mappings		
