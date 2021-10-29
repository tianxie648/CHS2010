module WriteResults
use globvar
implicit none

contains

subroutine estimates
implicit none
    integer :: t,j,k,s,i
    CHARACTER(len=40) :: temp1,temp2 

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

    do j = 1, nfac
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
  
end subroutine estimates


end module WriteResults