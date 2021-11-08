module utilities
use globvar
use matrix
implicit none

contains

subroutine UT(n,m,a,k,P,asigma,wsigma)
    implicit none
    integer, intent(in) :: n,m
    real(8), intent(in) :: a(n),k,P(n,n)
    real(8), intent(out):: asigma(m,n),wsigma(m)
    integer :: i,j
    real(8) :: sqrtP(n,n)
    real(8) :: sqrtnpk,den

    asigma = 0.0d0
    wsigma = 0.0d0
    den    = dble(n) + k

    sqrtP = 0.0d0
    do j = 1, n
        sqrtP(j,j) = dsqrt(P(j,j))
    end do

!   The option below calculates the square root matrix
!   We do not use it here for three reasons: (1)It is computationally costly. 
!   (2)It does not work well when P is of dimension 4 or higher.
!   (3)Experiments indicate that there is not much improvement either. 
!   call sqrtmatrix(n,P,sqrtP)

    sqrtnpk = dsqrt(den)
    asigma(n+1,:) = a
    wsigma(n+1) = k/den
    do i = 1, n
        asigma(i,:) = a - sqrtnpk*sqrtP(i,:)
        wsigma(i) = 1.0d0/(2.0d0*den)
    end do
    do i = 1, n
        j = i + n+1
        asigma(j,:) = a + sqrtnpk*sqrtP(i,:)
        wsigma(j) = 1.0d0/(2.0d0*den)
    end do    
end subroutine UT

real(8) function transition(s,ifacc,nfacc,a)
    implicit none
    integer, intent(in) :: s
    integer, intent(in) :: ifacc
    integer, intent(in) :: nfacc
    real(8), intent(in) :: a(nfacc)
    real(8) :: b(nfacc), temp
    integer :: j
    transition = 0.0d0
    if (ifacc <= 2) then
        !Transforming so technology function 
        !is written in log(skills).
        do j = 1, 2
            b(j) = dexp(phi(s,ifacc)*alpha(1,j)*a(j))
        end do
        do j = 3, nfacc-1
            b(j) = dexp(phi(s,ifacc)*a(j))
        end do
        transition = 0.0d0
        do j = 1, nfacc-1
            transition = transition + G(s,ifacc,j)*b(j) 
        end do
        temp = phi(s,ifacc)*alpha(1,ifacc)
        transition = const(s,ifacc)/alpha(1,ifacc) + (rho(s,ifacc)/temp)*dlog(transition) + G(s,ifacc,nfacc)*a(nfacc)/alpha(1,ifacc)
    end if
    if (ifacc >= 3) then
        transition = const(s,ifacc) + dot_product(G(s,ifacc,:),a)
    end if
end function transition

real(8) function fnl(x)
    implicit none
    real(8), intent(in) :: x
    fnl = dexp(x)/(1.0d0+dexp(x))
end function fnl


subroutine UT2(n,m,a,k,P,asigma,wsigma)
    implicit none
    integer, intent(in) :: n,m
    real(8), intent(in) :: a(n),k,P(n,n)
    real(8), intent(out):: asigma(m,n),wsigma(m)
    integer :: i,j
    real(8) :: sqrtP(n,n)
    real(8) :: sqrtnpk,den

    asigma = 0.0d0
    wsigma = 0.0d0
    den    = dble(n) + k
    call sqrtmatrix(n,P,sqrtP)
    sqrtnpk = dsqrt(den)
    asigma(n+1,:) = a
    wsigma(n+1) = k/den
    do i = 1, n
        asigma(i,:) = a - sqrtnpk*sqrtP(i,:)
        wsigma(i) = 1.0d0/(2.0d0*den)
    end do
    do i = 1, n
        j = i + n+1
        asigma(j,:) = a + sqrtnpk*sqrtP(i,:)
        wsigma(j) = 1.0d0/(2.0d0*den)
    end do    
end subroutine UT2


end module utilities