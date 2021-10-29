module mappings
use globvar
implicit none

contains

subroutine dimtheta(ntheta)
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
        ntheta = ntheta + 1       !G(s,2,1)
        ntheta = ntheta + 1       !G(s,2,2)
        ntheta = ntheta + 1       !G(s,2,3)
        ntheta = ntheta + 1       !G(s,2,4)
    end do

    do s = 1, nstage
        ntheta = ntheta + 1       !phi(s,1)
        ntheta = ntheta + 1       !phi(s,2)
    end do

    ntheta = ntheta + 1           !beta(1) - cognitive
    ntheta = ntheta + 1           !beta(2) - noncognitive
    ntheta = ntheta + 1           !beta(1) - intercept
    ntheta = ntheta + 1           !var     - variance   

    write(*,*)ntheta

end subroutine dimtheta
   

subroutine getpar(ntheta,theta)
    implicit none
    integer, intent(inout) :: ntheta
    real(8), intent(in) :: theta(ntheta)
    integer :: j,t,k,s,n
    real(8) :: temp,aux

    ntheta = 0
    do s = 1, nstage
        Q(s,1,1) = dexp(theta(ntheta+1))
        ntheta = ntheta + 1
        Q(s,2,2) = dexp(theta(ntheta+1))
        ntheta = ntheta + 1        
    end do
            
    do s = 1, nstage
        G(s,1,1) = dexp(theta(ntheta+1))	
        ntheta  = ntheta + 1
        G(s,1,2) = dexp(theta(ntheta+1))	
        ntheta  = ntheta + 1
        G(s,1,3) = dexp(theta(ntheta+1))	
        ntheta  = ntheta + 1
        G(s,1,4) = dexp(theta(ntheta+1))	
        ntheta  = ntheta + 1
        G(s,2,1) = dexp(theta(ntheta+1))	
        ntheta  = ntheta + 1
        G(s,2,2) = dexp(theta(ntheta+1))	
        ntheta  = ntheta + 1
        G(s,2,3) = dexp(theta(ntheta+1))	
        ntheta  = ntheta + 1
        G(s,2,4) = dexp(theta(ntheta+1))	
        ntheta  = ntheta + 1

        aux = 1.0d0 + G(s,1,1) + G(s,1,2) + G(s,1,3) + G(s,1,4)
        G(s,1,1) = G(s,1,1)/aux
        G(s,1,2) = G(s,1,2)/aux
        G(s,1,3) = G(s,1,3)/aux
        G(s,1,4) = G(s,1,4)/aux
        G(s,1,5) = 1.0000d0/aux

        aux = 1.0d0 + G(s,2,1) + G(s,2,2) + G(s,2,3) + G(s,2,4) 
        G(s,2,1) = G(s,2,1)/aux
        G(s,2,2) = G(s,2,2)/aux
        G(s,2,3) = G(s,2,3)/aux
        G(s,2,4) = G(s,2,4)/aux
        G(s,2,5) = 1.0000d0/aux
    end do			

    do s = 1, nstage
        aux = dexp(theta(ntheta+1))
        phi(s,1) = 1.0d0-aux
        ntheta = ntheta + 1
        aux = dexp(theta(ntheta+1))
        phi(s,2) = 1.0d0-aux
        ntheta = ntheta + 1
    end do        

    beta(1) = theta(ntheta+1)
    ntheta  = ntheta + 1
    beta(2) = theta(ntheta+1)
    ntheta  = ntheta + 1
    beta(3) = theta(ntheta+1)
    ntheta  = ntheta + 1
    vareduc = dexp(theta(ntheta+1))	
    ntheta  = ntheta + 1
            
end subroutine getpar
       
    
end module mappings		
