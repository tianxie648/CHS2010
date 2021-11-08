module utilities
use globvar
use matrix
implicit none

contains


real(8) function transition(s,iskill,a)
    implicit none
    integer, intent(in) :: s
    integer, intent(in) :: iskill
    real(8), intent(in) :: a(nx)
    real(8) :: b(nx), temp
    integer :: j
    transition = 0.0d0

    !Transforming so technology function is written in log(skills).
    do j = 1, 2
        b(j) = dexp(phi(s,iskill)*beta(j)*a(j))
    end do
    do j = 3, nx
        b(j) = dexp(phi(s,iskill)*a(j))
    end do
    transition = 0.0d0

    do j = 1, nx
        transition = transition + G(s,iskill,j)*b(j) 
    end do

    transition = const(s,iskill)/beta(iskill) + (rho(s,iskill)/(beta(iskill)*phi(s,iskill)))*dlog(transition)

end function transition

end module utilities