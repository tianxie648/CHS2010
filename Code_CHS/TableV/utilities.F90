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
    integer, parameter :: nx = npsi
    real(8) :: xtemp(nx)

    transition = 0.0d0
    if (ifacc <= 2) then
        !Transforming so technology function 
        !is written in log(skills).
        b(1) = dexp(phi(s,ifacc)*alpha(1,1)*a(1))
        b(2) = dexp(phi(s,ifacc)*alpha(1,2)*a(2))
        b(4) = dexp(phi(s,ifacc)*a(4))
        b(5) = dexp(phi(s,ifacc)*a(5))
        !Factor 6 is family income, so it does not enter technology of skill formation
        b(6) = 0.0d0

        !Factor 3 is log investment, so we replace with policy function (log linear)
        xtemp = (/alpha(1,1)*a(1),alpha(1,2)*a(2),a(4),a(5),a(6)/)
        temp = polfunct(nx,xtemp) + a(3)
        b(3) = dexp(phi(s,ifacc)*temp)

        !Construct Technology        
        transition = G(s,ifacc,1)*b(1)                  !Contribution of Log Cognitive Skill
        transition = transition + G(s,ifacc,2)*b(2)     !Contribution of Log Noncognitive Skill
        transition = transition + G(s,ifacc,3)*b(3)     !Contribution of Log Investment
        transition = transition + G(s,ifacc,4)*b(4)     !Contribution of Log Maternal Cognitive Skill
        transition = transition + G(s,ifacc,5)*b(5)     !Contribution of Log Maternal Noncognitive Skill
        
        temp = phi(s,ifacc)*alpha(1,ifacc)
        transition = const(s,ifacc)/alpha(1,ifacc) + (rho(s,ifacc)/temp)*dlog(transition) + (G(s,ifacc,nfacc)/alpha(1,ifacc))*a(3)
    else if (ifacc >= 3) then
        transition = const(s,ifacc) + dot_product(G(s,ifacc,:),a)
    end if

end function transition

real(8) function fnl(x)
    implicit none
    real(8), intent(in) :: x
    fnl = dexp(x)/(1.0d0+dexp(x))
end function fnl

real(8) function polfunct(nx,x)
    implicit none
    integer, intent(in) :: nx
    real(8), intent(in) :: x(nx)
    integer :: i1,i2,ipsi
    polfunct = 0.0d0
    ipsi = 1
    do i1 = 1, nx
        polfunct = polfunct + psi(ipsi)*x(i1)
        ipsi = ipsi + 1
    end do
end function polfunct


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