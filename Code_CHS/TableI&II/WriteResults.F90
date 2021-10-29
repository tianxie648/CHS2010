module WriteResults
use globvar
use utilities
implicit none

contains

subroutine estimates
implicit none
    integer, parameter :: nsigma = 2*nfac + 1
    real(8), parameter :: kappa = 2.0d0
    integer :: t,j,k,s,i,m,k1,k2
    CHARACTER(len=40) :: temp1,temp2 
    real(8) :: a(ntime,nemf,nfac)									!Mean of factor
    real(8) :: P(ntime,nemf,nfac,nfac)					            !Variance-Covariance Matrix
    real(8) :: R(ntime,nemf,nfac,nfac)                              !Correlation Matrix
    real(8) :: aup(nfac)                                            !Updated Mean of the Factor (For Unscent Transform)
    real(8) :: bup(nfac,nfac)
    real(8) :: diffvec(nsigma,nfac)
    real(8) :: diffout(nsigma,nfac,nfac)
    real(8) :: aux
    real(8) :: asigma(nsigma,nfac)
    real(8) :: wsigma(nsigma)
    
    open(1,file='observation.out')
    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            write(1,'(2I,<nx+nfac+1>f16.8)')t,k,  beta(t,k,:),  Z(t,k,:),  H(t,k,k)
            write(1,'(2I,<nx+nfac+1>f16.8)')t,k,sdbeta(t,k,:),sdZ(t,k,:),sdH(t,k,k) 
            write(1,*)
        end do
        write(1,*)
    end do
    write(1,*)'Parameters in Anchoring Equations'
    t = ntime
    do j = 1, nanch
        write(1,'(2I,<nx+nfac+1>f16.8)')t,j,  delta(j,:),  alpha(j,:),  HANCH(j)
        write(1,'(2I,<nx+nfac+1>f16.8)')t,j,sddelta(j,:),sdalpha(j,:),sdHANCH(j) 
        write(1,*)
    end do
    close(1)

    do j = 1, 2
        write(temp1,fmt=*) j
        temp2='transition'//trim(adjustl(temp1))
        temp2=trim(adjustl(temp2))//'.out'
        open(1,file=temp2)
        write(1,*)'Transition Equation for Factor', j
        do s = 1, nstage
            write(1,*)'Developmental Stage #', s
            write(1,'(<nfac+4>f16.8)')  const(s,j),  G(s,j,:),  phi(s,j),  rho(s,j),  Q(s,j,j)
            write(1,'(<nfac+4>f16.8)')sdconst(s,j),sdG(s,j,:),sdphi(s,j),sdrho(s,j),sdQ(s,j,j)
            write(1,*)
        end do
        close(1)
    end do

    open(1,file='factor_distribution.out')
    !Weights
    write(1,*)'Weights'
    do j = 1, nemf
        write(1,'(I,f16.8)')j,W0(j)
    end do
    !Means
    write(1,*)'Means'
    do j = 1, nemf
        write(1,'(I,10f16.8)')j,a0(j,:)
    end do
    write(1,*)
    !Variances
    write(1,*)'Variances'    
    do j = 1, nemf
        write(1,*)'Covariance Matrix for Mixture #', j
        do k = 1, nfac
            write(1,'(<nfac>f16.8)')P0(j,k,:)
        end do
        write(1,*)
    end do
    close(1)

    a(1,:,:) = a0
    P(1,:,:,:) = P0
    do t = 1, ntime-1
	    s = stage(t)
	    do m = 1, nemf
            !-------------------------------------------------------------------
            !Prediction according to the Unscented Kalman Filter
            aup = a(t,m,:)
            bup = P(t,m,:,:)
            call UT2(nfac,nsigma,aup,kappa,bup,asigma,wsigma)
            a(t+1,m,:) = 0.0d0
            do k1 = 1, nfac
                do k2 = 1, nsigma
                    a(t+1,m,k1) = a(t+1,m,k1) + wsigma(k2)*transition(s,k1,nfac,asigma(k2,:))
                end do
            end do
            P(t+1,m,:,:) = Q(s,:,:)
            diffvec = 0.0d0
            diffout = 0.0d0
            do j = 1, nsigma
                do k1 = 1, nfac
                    aux = transition(s,k1,nfac,asigma(j,:))
                    diffvec(j,k1) = aux - a(t+1,m,k1)
                end do
                do k1 = 1, nfac
                    do k2 = 1, nfac
                        diffout(j,k1,k2) = diffvec(j,k1)*diffvec(j,k2)
                    end do
                end do
                P(t+1,m,:,:) = P(t+1,m,:,:) + wsigma(j)*diffout(j,:,:)
            end do
            !-------------------------------------------------------------------
        end do
    end do


    open(100,file='covariance.out')
    do t = 1, ntime
        do m = 1, nemf
            do k = 1, nfac
                write(100,'(<nfac>f16.8)')P(t,m,k,:)
            end do
            write(100,*)
        end do
        write(100,*)
    end do        
    close(100)

    do t = 1, ntime
        do m = 1, nemf
            do i = 1, nfac
                do j = i, nfac
                    R(t,m,i,j) = P(t,m,i,j)/(dsqrt(P(t,m,i,i))*dsqrt(P(t,m,j,j)))
                end do
            end do
        end do
    end do

    do t = 1, ntime
        do m = 1, nemf
            do i = 2, nfac
                do j = 1, i-1
                    R(t,m,i,j) = R(t,m,j,i)
                end do
            end do                
        end do
    end do

    open(100,file='correlation.out')
    do t = 1, ntime
        do m = 1, nemf
            do j = 1, nfac
                write(100,'(<nfac>f16.8)')R(t,m,j,:)
            end do
            write(100,*)
        end do
        write(100,*)
    end do
    close(100)

    open(2,file='factor_loadings.out')
    do t = 1, ntime
        do j = 1, nequation(t)
            k = eqindex(t,j)
            write(2,'(2I,<2*nfac+1>f16.8)')t,k,Z(t,k,:),P(t,1,1,1),P(t,1,2,2),P(t,1,3,3),P(t,1,4,4),P(t,1,5,5),H(t,k,k)
        end do
    end do
    close(2)

  
end subroutine estimates


end module WriteResults