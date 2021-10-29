module like_aux
    use probability
    use matrix
    use globvar
    use utilities
    implicit none

contains

real(8) function density(i)
    implicit none
    integer, intent(in) :: i
    integer :: j, t, s
    real(8) :: mu(ntime,ny)
    real(8) :: temp,aux,var
    
    density = 1.0d0
    do t = 1, ntime-1
        s = stage(t)
        do j = 1, ny
            if ((dy(i,t+1,j) == 1).and.(dx(i,t) == 1)) then
                mu(t,j) = transition(s,j,x(i,t,:))
                var = Q(s,j,j)/(beta(j)*beta(j))
!                temp = normpdf(y(i,t+1,j),mu(t,j),var)
                temp = normpdf(y(i,t+1,j),mu(t,j),Q(s,j,j))
                density = density*temp
            end if                        
        end do
    end do
    if (deduc(i) == 1) then
        aux = beta(1)*y(i,ntime,1) + beta(2)*y(i,ntime,2) + beta(3)
        temp = normpdf(educ(i),aux,vareduc)
        density = density*temp
    end if
	
end function density

end module like_aux
