module utilities
use globvar
use matrix
use probability
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
        do j = 1, nfacc-1
            b(j) = dexp(phi(stag,ifacc)*a(j))
            technology = technology + G(stag,ifacc,j)*b(j) 
        end do                            
        technology = rho(stag,ifacc)/phi(stag,ifacc)*dlog(technology) + G(stag,ifacc,6)*a(6)
    end if
end function technology

real(8) function Crime(mC,aC,theta)
implicit none
    real(8), intent(in) :: mC
    real(8), intent(in) :: aC(3)
    real(8), intent(in) :: theta(3)
    real(8) :: indexx
    indexx = mC + aC(1)*theta(1) + aC(2)*theta(2) + aC(3)*theta(3)
    Crime = NORMCDF(indexx,0.0d0,1.0d0)
end function Crime

end module utilities