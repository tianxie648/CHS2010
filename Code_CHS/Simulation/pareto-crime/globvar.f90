module globvar
implicit none

integer, parameter :: nind	  = 2500        !Number of Individuals
integer, parameter :: ntime	  =    2        !Number of Time Periods
integer, parameter :: nstage  =    2        !Number of stages in developmental process
integer, parameter :: nfac    =    6        !Number of factors in the estimation program
integer, parameter :: nstate  =    5        !Initial Conditions (2), Parental Skills (2), Heterogeneity (1)
!Choice variables
real(8), allocatable :: x(:,:)              !Investments in skills

!Constraint
real(8), parameter :: budget = dble(nind)*dble(ntime)*1.0d0
real(8), allocatable :: points(:,:)         !the points
    
!Parameters in the Trasition Functions
integer :: stage(ntime)                     !Stage 
real(8) :: criminal(nind)
real(8) :: G(nstage,nfac,nfac)              !Estimated Coefficients in transition equations
real(8) :: phi(nstage,nfac)                 !Parameters for nonlinear transition 
real(8) :: rho(nstage,nfac)                 !Parameters for nonlinear transition 
real(8) :: P0(nfac,nfac)                    !Initial variance of the factors
real(8) :: alpha(3)                         !Parameters in the Education equation
real(8) :: constant                         !Parameters in the Education equation
real(8) :: price(ntime)                     !price of investments

end module globvar
