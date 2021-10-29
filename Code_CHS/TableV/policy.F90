module policy
use globvar
implicit none

contains

subroutine policy_parameters
implicit none
integer :: t,s,k

    do k = 27, 43
        Z(t,k,1) = Z(t,k,3)*psi(1)
        Z(t,k,2) = Z(t,k,3)*psi(2)
        Z(t,k,4) = Z(t,k,3)*psi(3)
        Z(t,k,5) = Z(t,k,3)*psi(4)
        Z(t,k,6) = Z(t,k,3)*psi(5)
    end do
    
end subroutine policy_parameters

end module policy