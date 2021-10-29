module globvar
implicit none

integer, parameter :: nind	  = 2148  	    !Number of Individuals
integer, parameter :: ntime	  =    8        !Number of Time Periods
integer, parameter :: ny      =    2        !Number of measurements
integer, parameter :: nx	  =    5	    !Number of Observables in dynamic measurements
integer, parameter :: nstage  =    2        !Number of stages in developmental process

!Data
integer :: igradient                        !Counter for Standard Error Computation
integer :: stage(ntime)                     !Stage 
integer :: dY(nind,ntime,ny)                !Indicator of inclusion in dynamic measurements(data not missing)
integer :: dX(nind,ntime)                   !Indicator of inclusion in dynamic measurements(data not missing)
integer :: deduc(nind)                      !Indicator of inclusion in education equation
real(8) :: Y(nind,ntime,ny)                 !measurements on dynamic factors
real(8) :: X(nind,ntime,nx)                 !Explanatory variables in dynamic measurements
real(8) :: educ(nind)                       !Final Education Attainment

!Parameters in the Trasition Functions
real(8) :: G(nstage,ny,nx)		            !Estimated Coefficients in transition equations
real(8) :: sdG(nstage,ny,nx)		        !Standard Error of Estimated Coefficients in transition equations
real(8) :: Q(nstage,ny,ny)                  !Estimated Variance of uniquenesses in transition equation
real(8) :: sdQ(nstage,ny,ny)                !Std Error of Variance of uniquenesses in transition equation
real(8) :: const(nstage,ny)                 !Constants in Law of Motion
real(8) :: sdconst(nstage,ny)               !Standard Error of Estimated Constants in Law of Motion 
real(8) :: phi(nstage,ny)                   !Parameters for nonlinear transition 
real(8) :: sdphi(nstage,ny)                 !Standard Error of Parameters for nonlinear transition 
real(8) :: rho(nstage,ny)                   !Parameters for nonlinear transition 
real(8) :: sdrho(nstage,ny)                 !Standard Error of Parameters for nonlinear 
real(8) :: beta(3),sdbeta(3)                !Parameters in the education equation
real(8) :: vareduc,sdvareduc

end module globvar
