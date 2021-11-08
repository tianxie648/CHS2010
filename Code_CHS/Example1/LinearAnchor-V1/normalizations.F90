module normalizations
use globvar
implicit none

contains


subroutine normalize_parameters
    implicit none
    integer :: t,s,j,n,k
    real(8) :: aux
    
    !Initializing Parameters
    a0 = 0.0d0							
    P0 = 0.0d0
    Z = 0.0d0
    H = 0.0d0	
    HANCH = 1.0d0							
    Q = 0.0d0								
    G = 0.0d0
    beta = 0.0d0
    alpha = 0.0d0
    delta = 0.0d0
    const = 0.0d0
    sda0 = 0.0d0
    sdP0 = 0.0d0
    sdZ = 0.0d0
    sdH = 0.0d0	
    sdHANCH = 0.0d0							
    sdQ = 0.0d0								
    sdG = 0.0d0
    sdbeta = 0.0d0
    sddelta = 0.0d0
    sdalpha = 0.0d0
    sdconst = 0.0d0

    !Imposing normalizations
    !Period 1
    do t = 1, ntime
        Z(t,1,1) = 1.0d0   !Normalize Child's Cog Factor
        Z(t,4,2) = 1.0d0   !Normalize Child's Noncog Factor 
    end do

    !Factor Loadings
    !Period 1
    do t = 1, ntime
        Z(t,2,1) = 1.0d0
        Z(t,3,1) = 1.0d0
        Z(t,5,2) = 1.0d0  
        Z(t,6,2) = 1.0d0  
    end do

    t = ntime
    alpha(1,1) = 1.0d0
    
    !Initial Variance of Shocks
    do s = 1, nstage
        Q(s,1,1) = 0.1d0						
        Q(s,2,2) = 0.1d0						
    end do									

    !Initial Technology

    do s = 1, nstage
        G(s,1,:)   = (/0.5d0, 0.5d0/)           
        G(s,2,:)   = (/0.0d0, 0.8d0/)          
    end do									
    
    do s = 1, nstage
        phi(s,1) = -0.5d0
        rho(s,1) =  1.0d0
    end do
       
    !Then, the parameters of the factor distribution
    !Initial Covariance Matrix
    do n = 1, nemf
        do j = 1, nfac
            P0(n,j,j) = 0.1d0
        end do
    end do
 
    !Weights
    do n = 1, nemf
        W0(n) = 1.0d0/dble(nemf)
    end do

    !Means
    do n = 1, nemf-1
        do j = 1, nfac
            a0(n,j) = 0.0d0
        end do
    end do
    do j = 1, nfac
        aux = 0.0d0
        do n = 1, nemf-1
            aux = aux + W0(n)*a0(n,j)
        end do
        a0(nemf,j) = -aux/W0(nemf)
    end do

end subroutine normalize_parameters

end module normalizations