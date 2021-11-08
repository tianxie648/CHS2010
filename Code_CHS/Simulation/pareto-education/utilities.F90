module utilities
use globvar
use matrix
implicit none

contains

real(8) function technology(stag,ifacc,nfacc,a)
    implicit none
    integer, intent(in) :: stag
    integer, intent(in) :: ifacc
    integer, intent(in) :: nfacc
    real(8), intent(in) :: a(nfacc)
    real(8) :: b(nfacc), temp
    integer :: j
    technology = 0.0d0
    if (ifacc <= 2) then
        !Transforming so technology function 
        !is written in log(skills).
        b(1) = dexp(phi(stag,ifacc)*a(1))
        b(2) = dexp(phi(stag,ifacc)*a(2))
        b(3) = dexp(phi(stag,ifacc)*a(3))
        b(4) = dexp(phi(stag,ifacc)*a(4))
        b(5) = dexp(phi(stag,ifacc)*a(5))
        b(6) = dexp(phi(stag,ifacc)*a(6))

        technology = 0.0d0
        technology = technology + G(stag,ifacc,1)*b(1) 
        technology = technology + G(stag,ifacc,2)*b(2) 
        technology = technology + G(stag,ifacc,3)*b(3) 
        technology = technology + G(stag,ifacc,4)*b(4) 
        technology = technology + G(stag,ifacc,5)*b(5) 
        technology = rho(stag,ifacc)/phi(stag,ifacc)*dlog(technology) + G(stag,ifacc,6)*a(6)

    end if
end function technology

real(8) function Schooling(mS,aS,theta)
implicit none
    real(8), intent(in) :: mS
    real(8), intent(in) :: aS(3)
    real(8), intent(in) :: theta(3)
    real(8) :: indexx
    indexx = mS + aS(1)*theta(1) + aS(2)*theta(2) + aS(3)*theta(3)
    Schooling = indexx
end function Schooling

end module utilities