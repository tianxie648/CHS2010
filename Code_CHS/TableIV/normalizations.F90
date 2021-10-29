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
    do s = 1, nstage
        G(s,4,4) = 1.0d0      !Factor 4 is a static Factor (Mother's Cognitive)
        Q(s,4,4) = 0.0d0      !Factor 4 is a static Factor (Mother's Cognitive)
        G(s,5,5) = 1.0d0      !Factor 5 is a static Factor (Mother's Noncognitive)
        Q(s,5,5) = 0.0d0      !Factor 5 is a static Factor (Mother's Noncognitive)
        G(s,6,6) = 1.0d0      !Factor 5 is a static Factor (Mother's Noncognitive)
        Q(s,6,6) = 0.0d0      !Factor 5 is a static Factor (Mother's Noncognitive)
    end do
    !Period 1
    t = 1
    Z(t, 2,1) = 1.0d0   !Normalize Child's Cog Factor
    Z(t,18,2) = 1.0d0   !Normalize Child's Noncog Factor 
    Z(t,29,3) = 1.0d0   !Normalize Parent's Investment Factor
    Z(t,44,4) = 1.0d0   !Normalize Mom's Cog Factor
    Z(t,50,5) = 1.0d0   !Normalize Mom's Noncog Factor

    !Period 2
    t = 2
    Z(t, 3,1) = 1.0d0   !Normalize Child's Cog Factor
    Z(t,18,2) = 1.0d0   !Normalize Child's Noncog Factor 
    Z(t,29,3) = 1.0d0   !Normalize Parent's Investment Factor

    !Period 3
    t = 3
    Z(t, 6,1) = 1.0d0   !Normalize Child's Cog Factor
    Z(t,20,2) = 1.0d0   !Normalize Child's Noncog Factor 
    Z(t,29,3) = 1.0d0   !Normalize Parent's Investment Factor

    do t = 4, ntime 
        Z(t, 7,1) = 1.0d0   !Normalize Child's Cog Factor
        Z(t,20,2) = 1.0d0   !Normalize Child's Noncog Factor 
        Z(t,36,3) = 1.0d0   !Normalize Parent's Investment Factor
    end do

    !Factor Loadings
    !Period 1
    t = 1
    Z(t, 1,1) = 1.0d0
    Z(t, 3,1) = 1.0d0
    Z(t,18,2) = 1.0d0  
    Z(t,19,2) = 1.0d0  
    Z(t,27,3) = 1.0d0  
    Z(t,28,3) = 1.0d0  
    Z(t,30,3) = 1.0d0  
    Z(t,31,3) = 1.0d0  
    Z(t,32,3) = 1.0d0  
    Z(t,33,3) = 1.0d0  
    Z(t,45,4) = 1.0d0  
    Z(t,46,4) = 1.0d0  
    Z(t,47,4) = 1.0d0  
    Z(t,48,4) = 1.0d0  
    Z(t,49,4) = 1.0d0  
    Z(t,51,5) = 1.0d0  
    Z(t,52,5) = 1.0d0  
    Z(t,54,5) = 1.0d0  
    Z(t,55,5) = 1.0d0  
    Z(t,56,5) = 1.0d0  
    Z(t,57,5) = 1.0d0  
    Z(t,58,5) = 1.0d0  
    Z(t,59,5) = 1.0d0  
    Z(t,60,5) = 1.0d0  
    Z(t,61,5) = 1.0d0  
    Z(t,62,5) = 1.0d0  

    !Period 2
    t = 2
    Z(t, 4,1) = 1.0d0
    Z(t, 5,1) = 1.0d0
    Z(t,14,2) = 1.0d0
    Z(t,15,2) = 1.0d0
    Z(t,16,2) = 1.0d0
    Z(t,18,2) = 1.0d0
    Z(t,19,2) = 1.0d0
    Z(t,27,3) = 1.0d0
    Z(t,28,3) = 1.0d0
    Z(t,30,3) = 1.0d0
    Z(t,31,3) = 1.0d0
    Z(t,32,3) = 1.0d0
    Z(t,33,3) = 1.0d0

    !Period 3
    t = 3
    Z(t, 3,1) = 1.0d0
    Z(t,14,2) = 1.0d0
    Z(t,15,2) = 1.0d0
    Z(t,16,2) = 1.0d0
    Z(t,21,2) = 1.0d0
    Z(t,22,2) = 1.0d0
    Z(t,23,2) = 1.0d0
    Z(t,24,2) = 1.0d0
    Z(t,27,3) = 1.0d0
    Z(t,28,3) = 1.0d0
    Z(t,32,3) = 1.0d0
    Z(t,34,3) = 1.0d0
    Z(t,35,3) = 1.0d0
  
    !Period 4
    t = 4
    Z(t, 6,1) = 1.0d0
    Z(t, 8,1) = 1.0d0
    Z(t, 9,1) = 1.0d0
    Z(t,21,2) = 1.0d0
    Z(t,22,2) = 1.0d0
    Z(t,23,2) = 1.0d0
    Z(t,24,2) = 1.0d0
    Z(t,27,3) = 1.0d0
    Z(t,28,3) = 1.0d0
    Z(t,29,3) = 1.0d0
    Z(t,32,3) = 1.0d0
    Z(t,34,3) = 1.0d0
    Z(t,35,3) = 1.0d0
    Z(t,37,3) = 1.0d0
    Z(t,38,3) = 1.0d0
    Z(t,39,3) = 1.0d0
    Z(t,40,3) = 1.0d0
    Z(t,41,3) = 1.0d0
    Z(t,42,3) = 1.0d0
    Z(t,43,3) = 1.0d0

    !Periods 5 and 6
    do t = 5, 6
        Z(t, 8,1) = 1.0d0
        Z(t, 9,1) = 1.0d0
        Z(t,21,2) = 1.0d0
        Z(t,22,2) = 1.0d0
        Z(t,23,2) = 1.0d0
        Z(t,24,2) = 1.0d0
        Z(t,28,3) = 1.0d0
        Z(t,29,3) = 1.0d0
        Z(t,32,3) = 1.0d0
        Z(t,37,3) = 1.0d0
        Z(t,38,3) = 1.0d0
        Z(t,39,3) = 1.0d0
        Z(t,40,3) = 1.0d0
        Z(t,41,3) = 1.0d0
        Z(t,42,3) = 1.0d0
        Z(t,43,3) = 1.0d0
    end do

    !Periods 7 and 8
    do t = 7, ntime
        Z(t, 8,1) = 1.0d0
        Z(t, 9,1) = 1.0d0
        Z(t,21,2) = 1.0d0
        Z(t,22,2) = 1.0d0
        Z(t,23,2) = 1.0d0
        Z(t,24,2) = 1.0d0
        Z(t,28,3) = 1.0d0
        Z(t,32,3) = 1.0d0
        Z(t,37,3) = 1.0d0
        Z(t,38,3) = 1.0d0
        Z(t,39,3) = 1.0d0
        Z(t,40,3) = 1.0d0
        Z(t,41,3) = 1.0d0
        Z(t,42,3) = 1.0d0
        Z(t,43,3) = 1.0d0
    end do

    t = ntime
    alpha(1,1) =  1.0000000d0
    alpha(1,2) =  1.0000000d0
    alpha(1,6) =  1.0000000d0
    alpha(2,1) = -1.0000000d0
    alpha(2,2) = -1.0000000d0
    alpha(2,6) = -1.0000000d0
    alpha(3,1) = -1.0000000d0
    alpha(3,2) = -1.0000000d0
    alpha(3,6) = -1.0000000d0
    alpha(4,1) = -1.0000000d0
    alpha(4,2) = -1.0000000d0
    alpha(4,6) = -1.0000000d0
    
    !Initial Variance of Shocks
    do s = 1, nstage
        Q(s,1,1) = 0.1d0						
        Q(s,2,2) = 0.1d0						
        Q(s,3,3) = 0.1d0						
    end do									

    !Initial Technology

    !Initial Covariance Matrix

    do s = 1, nstage
        G(s,1,1) = 0.6d0						!Initial "technology"
        G(s,1,2) = 0.1d0						!Initial "technology"
        G(s,1,3) = 0.1d0						!Initial "technology"
        G(s,1,4) = 0.1d0						!Initial "technology"
        G(s,1,5) = 0.1d0						!Initial "technology"
        G(s,1,6) = 1.0d0						!Initial "technology"
        G(s,2,1) = 0.1d0						!Initial "technology"
        G(s,2,2) = 0.6d0						!Initial "technology"
        G(s,2,3) = 0.1d0						!Initial "technology"
        G(s,2,4) = 0.1d0						!Initial "technology"
        G(s,2,5) = 0.1d0						!Initial "technology"
        G(s,2,6) = 1.0d0						!Initial "technology"
        G(s,3,3) = 0.8d0						!Initial "technology"
    end do									
    
    do s = 1, nstage
        phi(s,1) = -0.2d0
        phi(s,2) = -0.1d0
        rho(s,1) =  1.0d0
        rho(s,2) =  1.0d0
    end do

    phi(1,1) = 0.1d0
        
    !Then, the parameters of the factor distribution
    !Only to obtain good initial values
    do n = 1, nemf
        do j = 1, nfac
            P0(n,j,j) = 0.1d0
        end do
    end do
    
    !Variance
    
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