module WriteResults
use globvar
implicit none

contains

subroutine estimates
implicit none
    integer :: t,j,k,s,i
    CHARACTER(len=40) :: temp1,temp2 

    do j = 1, ny
        write(temp1,fmt=*) j
        temp2='transition'//trim(adjustl(temp1))
        temp2=trim(adjustl(temp2))//'.out'
        open(1,file=temp2)
        write(1,*)'Transition Equation for Skill', j
        do s = 1, nstage
            write(1,*)'Developmental Stage #', s
            write(1,'(<nx+4>f16.8)')  const(s,j),  G(s,j,:),  phi(s,j),  rho(s,j),  Q(s,j,j)
            write(1,'(<nx+4>f16.8)')sdconst(s,j),sdG(s,j,:),sdphi(s,j),sdrho(s,j),sdQ(s,j,j)
            write(1,*)
        end do
        close(1)
    end do

    open(10,file='anchor.out')
    write(10,'(4f16.8)')  beta,  vareduc
    write(10,'(4f16.8)')sdbeta,sdvareduc
    close(10)
  
end subroutine estimates


end module WriteResults