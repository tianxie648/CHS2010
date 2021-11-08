module pareto_aux
    use globvar
    use utilities
    implicit none

contains

real(8) function person(i)
    implicit none
    integer, intent(in) :: i
    integer :: j,t
    real(8) :: input(nfac)
    real(8) :: cog(3),ncog(3),pcog,pncog,het,skills(3)
    
    cog(1)  = points(i,1)
    ncog(1) = points(i,2)
    pcog    = points(i,3)
    pncog   = points(i,4)
    het     = points(i,5)
    
    input   = (/cog(1),ncog(1),dlog(x(i,1)),pcog,pncog,het/)
    cog(2)  = technology(1,1,nfac,input)
    ncog(2) = technology(1,2,nfac,input)

    input   = (/cog(2),ncog(2),dlog(x(i,2)),pcog,pncog,het/)
    cog(3)  = technology(2,1,nfac,input)
    ncog(3) = technology(2,2,nfac,input)

    skills  = (/cog(3),ncog(3),het/)
    person = Crime(constant,alpha,skills)
    criminal(i) = Crime(constant,alpha,skills)
	
end function person

end module pareto_aux
