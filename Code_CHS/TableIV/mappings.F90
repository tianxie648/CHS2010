module mappings
use globvar
implicit none

contains

subroutine dimtheta(ntheta)
    implicit none
    integer, intent(out) :: ntheta
    integer :: j,k,t,s,n
    ntheta = 0

    !Measurement Equations
    open(200,file='check.out')
    !First the betas
    t = 1
    do j = 1, nequation(t)
        k = eqindex(t,j)
        if ((k>=3).and.(k<=33)) then
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! sex
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! d19872001
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! constant
        else
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! sex
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! d19872001
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! teenmom
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! constant
        end if
    end do

    t = 2
        do j = 1, nequation(t)
        k = eqindex(t,j)
        if ((k==14).or.(k==15)) then
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! sex
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! d19872001
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! teenmom
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! constant
        else
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! sex
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! d19872001
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! teenmom
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! dpar
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! constant
        end if
        end do

    t = 3
    do j = 1, nequation(t)
        k = eqindex(t,j)
        if ((k>=20).and.(k<=24)) then
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! sex
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! d19872001
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! teenmom
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! constant
        else 
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! sex
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! d19872001
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! teenmom
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! dpar
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! constant
        end if
    end do
  
    t = 4
    do j = 1, nequation(t)
        k = eqindex(t,j)
        if ((k==27).or.(k==34).or.(k==35).or.(k>=37)) then
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! sex
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! d19872001
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! teenmom
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! constant
        else 
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! sex
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! d19872001
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! teenmom
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! dpar
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! constant
        end if
    end do

    t = 5
    do j = 1, nequation(t)
        k = eqindex(t,j)
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! sex
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! d19872001
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! teenmom
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! dpar
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! constant
    end do
 
    t = 6
    do j = 1, nequation(t)
        k = eqindex(t,j)
        if (k==29) then
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! sex
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! d19872001
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! teenmom
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! constant
        else 
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! sex
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! d19872001
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! teenmom
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! dpar
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! constant
        end if
    end do

    t = 7
    do j = 1, nequation(t)
        k = eqindex(t,j)
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! sex
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! d19872001
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! teenmom
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! dpar
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! constant
    end do

    t = ntime
    do j = 1, nequation(t)
        k = eqindex(t,j)
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! sex
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! d19872001
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! teenmom
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! dpar
        ntheta = ntheta + 1 
        write(200,*)t,k,ntheta         ! constant
    end do

    t = ntime
    do j = 1, nanch
        ntheta = ntheta + 1
        write(200,*)t,j,ntheta         ! constant
    end do

    !Then the variances
    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            ntheta = ntheta + 1 
            write(200,*)t,k,ntheta         ! variance
        end do
    end do

    t = ntime
    do j = 1, nanch
        if (PROBANCH(j).ne.1) then
            ntheta = ntheta + 1 
            write(200,*)t,j,ntheta         ! variance
        end if
    end do
    
    !Period 1
    t = 1
    ntheta = ntheta + 1 !  Z(t, 1,1) = 1.0d0
    ntheta = ntheta + 1 !  Z(t, 3,1) = 1.0d0
    ntheta = ntheta + 1 !  Z(t,27,3) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,28,3) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,30,3) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,31,3) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,32,3) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,33,3) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,45,4) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,46,4) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,47,4) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,48,4) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,49,4) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,51,5) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,52,5) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,54,5) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,55,5) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,56,5) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,57,5) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,58,5) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,59,5) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,60,5) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,61,5) = 1.0d0  
    ntheta = ntheta + 1 !  Z(t,62,5) = 1.0d0  

    !Period 2
    t = 2
    ntheta = ntheta + 1 !  Z(t, 4,1) = 1.0d0
    ntheta = ntheta + 1 !  Z(t, 5,1) = 1.0d0
    ntheta = ntheta + 1 !  Z(t,14,2) = 1.0d0
    ntheta = ntheta + 1 !  Z(t,15,2) = 1.0d0
    ntheta = ntheta + 1 !  Z(t,16,2) = 1.0d0
    ntheta = ntheta + 1 !  Z(t,19,2) = 1.0d0
    ntheta = ntheta + 1 !  Z(t,27,3) = 1.0d0
    ntheta = ntheta + 1 !  Z(t,28,3) = 1.0d0
    ntheta = ntheta + 1 !  Z(t,30,3) = 1.0d0
    ntheta = ntheta + 1 !  Z(t,31,3) = 1.0d0
    ntheta = ntheta + 1 !  Z(t,32,3) = 1.0d0
    ntheta = ntheta + 1 !  Z(t,33,3) = 1.0d0

    !Period 3
    t = 3
    ntheta = ntheta + 1 !Z(t, 3,1) = 1.0d0
    ntheta = ntheta + 1 !Z(t,14,2) = 1.0d0
    ntheta = ntheta + 1 !Z(t,15,2) = 1.0d0
    ntheta = ntheta + 1 !Z(t,16,2) = 1.0d0
    ntheta = ntheta + 1 !Z(t,21,2) = 1.0d0
    ntheta = ntheta + 1 !Z(t,22,2) = 1.0d0
    ntheta = ntheta + 1 !Z(t,23,2) = 1.0d0
    ntheta = ntheta + 1 !Z(t,24,2) = 1.0d0
    ntheta = ntheta + 1 !Z(t,27,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,28,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,32,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,34,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,35,3) = 1.0d0

    !Period 4
    t = 4
    ntheta = ntheta + 1 !Z(t, 6,1) = 1.0d0
    ntheta = ntheta + 1 !Z(t, 8,1) = 1.0d0
    ntheta = ntheta + 1 !Z(t, 9,1) = 1.0d0
    ntheta = ntheta + 1 !Z(t,21,2) = 1.0d0
    ntheta = ntheta + 1 !Z(t,22,2) = 1.0d0
    ntheta = ntheta + 1 !Z(t,23,2) = 1.0d0
    ntheta = ntheta + 1 !Z(t,24,2) = 1.0d0
    ntheta = ntheta + 1 !Z(t,27,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,28,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,29,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,32,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,34,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,35,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,37,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,38,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,39,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,40,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,41,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,42,3) = 1.0d0
    ntheta = ntheta + 1 !Z(t,43,3) = 1.0d0

    !Periods 5 and 6
    do t = 5, 6
        ntheta = ntheta + 1 !Z(t, 8,1) = 1.0d0
        ntheta = ntheta + 1 !Z(t, 9,1) = 1.0d0
        ntheta = ntheta + 1 !Z(t,21,2) = 1.0d0
        ntheta = ntheta + 1 !Z(t,22,2) = 1.0d0
        ntheta = ntheta + 1 !Z(t,23,2) = 1.0d0
        ntheta = ntheta + 1 !Z(t,24,2) = 1.0d0
        ntheta = ntheta + 1 !Z(t,28,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,29,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,32,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,37,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,38,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,39,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,40,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,41,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,42,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,43,3) = 1.0d0
    end do

    !Periods 7 and 8
    do t = 7, ntime
        ntheta = ntheta + 1 !Z(t, 8,1) = 1.0d0
        ntheta = ntheta + 1 !Z(t, 9,1) = 1.0d0
        ntheta = ntheta + 1 !Z(t,21,2) = 1.0d0
        ntheta = ntheta + 1 !Z(t,22,2) = 1.0d0
        ntheta = ntheta + 1 !Z(t,23,2) = 1.0d0
        ntheta = ntheta + 1 !Z(t,24,2) = 1.0d0
        ntheta = ntheta + 1 !Z(t,28,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,32,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,37,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,38,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,39,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,40,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,41,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,42,3) = 1.0d0
        ntheta = ntheta + 1 !Z(t,43,3) = 1.0d0
    end do
   
    ntheta = ntheta + 1 !alpha(2,1) = 1.0d0
    ntheta = ntheta + 1 !alpha(2,2) = 1.0d0
    ntheta = ntheta + 1 !alpha(2,6) = 1.0d0
    ntheta = ntheta + 1 !alpha(3,1) = 1.0d0
    ntheta = ntheta + 1 !alpha(3,2) = 1.0d0
    ntheta = ntheta + 1 !alpha(3,6) = 1.0d0
    ntheta = ntheta + 1 !alpha(4,1) = 1.0d0
    ntheta = ntheta + 1 !alpha(4,2) = 1.0d0
    ntheta = ntheta + 1 !alpha(4,6) = 1.0d0

    !Variances
    !Diagonal Elements
    do j = 1, nfac
        ntheta = ntheta + 1         !P0(m,j,j)
    end do
    
    !Off-Diagonal Elements
    do j = 1, 2
        do k = j+1, nfac-1
            ntheta = ntheta + 1
        end do
    end do
    do j = 3, 6
        do k = j+1, nfac
            ntheta = ntheta + 1
        end do
    end do

    !Variance of Shocks in Factors
    do s = 1, nstage
        ntheta = ntheta + 1       !Q(s,1,1)
        ntheta = ntheta + 1       !Q(s,2,2)
        ntheta = ntheta + 1       !Q(s,3,3)
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
        ntheta = ntheta + 1       !Const(s,3)
        ntheta = ntheta + 1       !G(s,3,3)
    end do

    do s = 1, nstage
        ntheta = ntheta + 1       !phi(s,1)
        ntheta = ntheta + 1       !phi(s,2)
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

    t = 1
    do j = 1, nequation(t)
        k = eqindex(t,j)
        if ((k>=3).and.(k<=33)) then
            beta(t,k,1) = theta(ntheta+1)   ! sex
            ntheta = ntheta + 1 
            beta(t,k,2) = theta(ntheta+1)   ! d19872001
            ntheta = ntheta + 1 
            beta(t,k,6) = theta(ntheta+1)  ! constant
            ntheta = ntheta + 1 
        else
            beta(t,k,1) = theta(ntheta+1)   ! sex
            ntheta = ntheta + 1 
            beta(t,k,2) = theta(ntheta+1)   ! d19872001
            ntheta = ntheta + 1 
            beta(t,k,3) = theta(ntheta+1)   ! teenmom
            ntheta = ntheta + 1 
            beta(t,k,6) = theta(ntheta+1)  ! constant
            ntheta = ntheta + 1 
        end if
    end do

    t = 2
    do j = 1, nequation(t)
        k = eqindex(t,j)
        if ((k==14).or.(k==15)) then
            beta(t,k,1) = theta(ntheta+1)   ! sex
            ntheta = ntheta + 1 
            beta(t,k,2) = theta(ntheta+1)   ! d19872001
            ntheta = ntheta + 1 
            beta(t,k,3) = theta(ntheta+1)   ! teenmom
            ntheta = ntheta + 1 
            beta(t,k,6) = theta(ntheta+1)   ! constant
            ntheta = ntheta + 1 
        else
            beta(t,k,1) = theta(ntheta+1)   ! sex
            ntheta = ntheta + 1 
            beta(t,k,2) = theta(ntheta+1)   ! d19872001
            ntheta = ntheta + 1 
            beta(t,k,3) = theta(ntheta+1)   ! teenmom
            ntheta = ntheta + 1 
            beta(t,k,4) = theta(ntheta+1)   ! dpar
            ntheta = ntheta + 1 
            beta(t,k,6) = theta(ntheta+1)   ! constant
            ntheta = ntheta + 1 
        end if
    end do

    t = 3
    do j = 1, nequation(t)
        k = eqindex(t,j)
        if ((k>=20).and.(k<=24)) then
            beta(t,k,1) = theta(ntheta+1)     !sex
            ntheta = ntheta + 1 
            beta(t,k,2) = theta(ntheta+1)     !d19872001
            ntheta = ntheta + 1 
            beta(t,k,3) = theta(ntheta+1)     !teenmom
            ntheta = ntheta + 1 
            beta(t,k,6) = theta(ntheta+1)     !const
            ntheta = ntheta + 1 
        else
            beta(t,k,1) = theta(ntheta+1)     !sex
            ntheta = ntheta + 1 
            beta(t,k,2) = theta(ntheta+1)     !d19872001
            ntheta = ntheta + 1 
            beta(t,k,3) = theta(ntheta+1)     !teenmom
            ntheta = ntheta + 1 
            beta(t,k,4) = theta(ntheta+1)     !dpar
            ntheta = ntheta + 1 
            beta(t,k,6) = theta(ntheta+1)     !const
            ntheta = ntheta + 1 
        end if
    end do

    t = 4
    do j = 1, nequation(t)
        k = eqindex(t,j)
        if ((k==27).or.(k==34).or.(k==35).or.(k>=37)) then
            beta(t,k,1) = theta(ntheta+1)     !sex
            ntheta = ntheta + 1 
            beta(t,k,2) = theta(ntheta+1)     !d19872001
            ntheta = ntheta + 1 
            beta(t,k,3) = theta(ntheta+1)     !teenmom
            ntheta = ntheta + 1 
            beta(t,k,6) = theta(ntheta+1)     !const
            ntheta = ntheta + 1 
        else
            beta(t,k,1) = theta(ntheta+1)     !sex
            ntheta = ntheta + 1 
            beta(t,k,2) = theta(ntheta+1)     !d19872001
            ntheta = ntheta + 1 
            beta(t,k,3) = theta(ntheta+1)     !teenmom
            ntheta = ntheta + 1 
            beta(t,k,4) = theta(ntheta+1)     !dpar
            ntheta = ntheta + 1 
            beta(t,k,6) = theta(ntheta+1)     !const
            ntheta = ntheta + 1 
        end if
    end do

    t = 5
    do j = 1, nequation(t)
        k = eqindex(t,j)
        beta(t,k,1) = theta(ntheta+1)     !sex
        ntheta = ntheta + 1 
        beta(t,k,2) = theta(ntheta+1)     !d19872001
        ntheta = ntheta + 1 
        beta(t,k,3) = theta(ntheta+1)     !teenmom
        ntheta = ntheta + 1 
        beta(t,k,4) = theta(ntheta+1)     !dpar
        ntheta = ntheta + 1 
        beta(t,k,6) = theta(ntheta+1)     !const
        ntheta = ntheta + 1 
    end do

    t = 6
    do j = 1, nequation(t)
        k = eqindex(t,j)
        if (k==29) then
            beta(t,k,1) = theta(ntheta+1)     !sex
            ntheta = ntheta + 1 
            beta(t,k,2) = theta(ntheta+1)     !d19872001
            ntheta = ntheta + 1 
            beta(t,k,3) = theta(ntheta+1)     !teenmom
            ntheta = ntheta + 1 
            beta(t,k,6) = theta(ntheta+1)     !const
            ntheta = ntheta + 1 
        else
            beta(t,k,1) = theta(ntheta+1)     !sex
            ntheta = ntheta + 1 
            beta(t,k,2) = theta(ntheta+1)     !d19872001
            ntheta = ntheta + 1 
            beta(t,k,3) = theta(ntheta+1)     !teenmom
            ntheta = ntheta + 1 
            beta(t,k,4) = theta(ntheta+1)     !dpar
            ntheta = ntheta + 1 
            beta(t,k,6) = theta(ntheta+1)     !const
            ntheta = ntheta + 1 
        end if
    end do

    do t = 7, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            beta(t,k,1) = theta(ntheta+1)     !sex
            ntheta = ntheta + 1 
            beta(t,k,2) = theta(ntheta+1)     !d19872001
            ntheta = ntheta + 1 
            beta(t,k,3) = theta(ntheta+1)     !teenmom
            ntheta = ntheta + 1 
            beta(t,k,4) = theta(ntheta+1)     !dpar
            ntheta = ntheta + 1 
            beta(t,k,6) = theta(ntheta+1)     !const
            ntheta = ntheta + 1 
        end do
    end do
    
    t = ntime
    do j = 1, nanch
        delta(j,6) = theta(ntheta+1)     !const
        ntheta = ntheta + 1 
    end do

    !Then, the variances
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
    t = 1
    Z(t, 1,1) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t, 3,1) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,27,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,28,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,30,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,31,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,32,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,33,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,45,4) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,46,4) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,47,4) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,48,4) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,49,4) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,51,5) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,52,5) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,54,5) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,55,5) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,56,5) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,57,5) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,58,5) = theta(ntheta+1)  
    ntheta = ntheta + 1
    Z(t,59,5) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,60,5) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,61,5) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,62,5) = theta(ntheta+1)   
    ntheta = ntheta + 1

    !Period 2
    t = 2
    Z(t, 4,1) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t, 5,1) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,14,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,15,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,16,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,19,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,27,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,28,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,30,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,31,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,32,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,33,3) = theta(ntheta+1)   
    ntheta = ntheta + 1

    !Period 3
    t = 3
    Z(t, 3,1) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,14,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,15,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,16,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,21,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,22,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,23,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,24,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,27,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,28,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,32,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,34,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,35,3) = theta(ntheta+1)   
    ntheta = ntheta + 1

    !Period 4
    t = 4
    Z(t, 6,1) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t, 8,1) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t, 9,1) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,21,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,22,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,23,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,24,2) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,27,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,28,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,29,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,32,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,34,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,35,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,37,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,38,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,39,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,40,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,41,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,42,3) = theta(ntheta+1)   
    ntheta = ntheta + 1
    Z(t,43,3) = theta(ntheta+1)   
    ntheta = ntheta + 1

    !Periods 5 and 6
    do t = 5, 6
        Z(t, 8,1) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t, 9,1) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,21,2) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,22,2) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,23,2) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,24,2) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,28,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,29,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,32,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,37,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,38,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,39,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,40,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,41,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,42,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,43,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
    end do

    !Periods 7 and 8
    do t = 7, ntime
        Z(t, 8,1) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t, 9,1) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,21,2) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,22,2) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,23,2) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,24,2) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,28,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,32,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,37,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,38,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,39,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,40,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,41,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,42,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
        Z(t,43,3) = theta(ntheta+1)   
        ntheta = ntheta + 1
    end do

    alpha(2,1) = theta(ntheta+1) 
    ntheta = ntheta + 1
    alpha(2,2) = theta(ntheta+1)
    ntheta = ntheta + 1
    alpha(2,6) = theta(ntheta+1)
    ntheta = ntheta + 1
    alpha(3,1) = theta(ntheta+1)
    ntheta = ntheta + 1
    alpha(3,2) = theta(ntheta+1)
    ntheta = ntheta + 1
    alpha(3,6) = theta(ntheta+1)
    ntheta = ntheta + 1
    alpha(4,1) = theta(ntheta+1)
    ntheta = ntheta + 1
    alpha(4,2) = theta(ntheta+1)
    ntheta = ntheta + 1
    alpha(4,6) = theta(ntheta+1)
    ntheta = ntheta + 1

    !Variances
    !Diagonal Elements
    do j = 1, nfac
        P0(1,j,j) = dexp(theta(ntheta+1)) !P0(m,j,j)
        ntheta = ntheta + 1
    end do

    !Variances
    !Off-Diagonal Elements
    do j = 1, 2
        do k = j+1, nfac-1
            aux = tanh(theta(ntheta+1))
            ntheta  = ntheta + 1
            P0(1,j,k) = aux*dsqrt(P0(1,j,j))*dsqrt(P0(1,k,k))
            P0(1,k,j) = P0(1,j,k)
        end do
    end do
    do j = 3,6
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
        Q(s,3,3) = dexp(theta(ntheta+1))
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
        Const(s,3) = theta(ntheta+1)       
        ntheta = ntheta + 1
        G(s,3,3) = theta(ntheta+1)	        
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
