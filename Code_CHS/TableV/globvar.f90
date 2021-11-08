module globvar
implicit none

integer, parameter :: nind	  = 2207  	    !Number of Individuals
integer, parameter :: ntime	  =    8        !Number of Time Periods
integer, parameter :: nfac	  =    6        !Number of dynamic factors
integer, parameter :: nmea    =   64        !Number of measurements
integer, parameter :: nx	  =    6	    !Number of Observables in dynamic measurements
integer, parameter :: nstage  =    2        !Number of stages in developmental process
integer, parameter :: nemf    =    2        !Number of elements in mixture distribution of the factor
integer, parameter :: nanch   =    1        !Number of anchoring equations
integer, parameter :: npsi    =    5        !Dimension of psi

!Data
integer :: igradient                        !Counter for Standard Error Computation
integer :: caseid(nind)                     !CNLSY/79 caseid
integer :: nequation(ntime)                 !Number of Equations per Period
integer :: eqindex(ntime,nmea)              !Measurement Equation Number in each Period
integer :: nonlinear(ntime,nmea)            !Indicator of whether measurement equation is linear or nonlinear in factors
integer :: typefunct(ntime,nmea)            !Indicator of type of nonlinear function in measurement equation
integer :: mx(ntime,nmea)                   !Number of covariates in each equation
integer :: dY(nind,ntime,nmea)              !Indicator of inclusion in dynamic measurements(data not missing)
integer :: dYANCH(nind,nanch)               !Indicator of inclusion in anchoring outcomes
integer :: PROBANCH(nanch)                  !Indicator that anchoring outcome is binary choice
integer :: stage(ntime)                     !Indicator of developmental stage in lifecycle
real(8) :: Y(nind,ntime,nmea)               !measurements on dynamic factors
real(8) :: X(nind,ntime,nx)                 !Explanatory variables in dynamic measurements
real(8) :: YANCH(nind,nanch)                !Anchoring outcomes

!Coefficients and Loadings on Measurement Equations
real(8) :: beta(ntime,nmea,nx)              !Estimated Coefficients on covariates of dynamic measurements
real(8) :: sdbeta(ntime,nmea,nx)            !Standard Error of Estimated Coefficients on dynamic measurements
real(8) :: Z(ntime,nmea,nfac)               !Estimated Factor Loadings
real(8) :: sdZ(ntime,nmea,nfac)             !Standard Error of Estimated Factor Loadings
real(8) :: delta(nanch,nx)                  !Estimated Coefficients on covariates of anchoring equations
real(8) :: sddelta(nanch,nx)                !Standard Error of Estimated Coefficients on covariates of anchoring equations
real(8) :: alpha(nanch,nfac)                !Estimated Factor Loadings on anchoring equations
real(8) :: sdalpha(nanch,nfac)              !Standard Error of Estimated Factor Loadings on anchoring equations

!Parameters in the Trasition Functions
real(8) :: G(nstage,nfac,nfac)		        !Estimated Coefficients in transition equations
real(8) :: sdG(nstage,nfac,nfac)		    !Standard Error of Estimated Coefficients in transition equations
real(8) :: Q(nstage,nfac,nfac)              !Estimated Variance of uniquenesses in transition equation
real(8) :: sdQ(nstage,nfac,nfac)            !Std Error of Variance of uniquenesses in transition equation
real(8) :: const(nstage,nfac)               !Constants in Law of Motion
real(8) :: sdconst(nstage,nfac)             !Standard Error of Estimated Constants in Law of Motion 
real(8) :: phi(nstage,nfac)                 !Parameters for nonlinear transition 
real(8) :: sdphi(nstage,nfac)               !Standard Error of Parameters for nonlinear transition 
real(8) :: rho(nstage,nfac)                 !Parameters for nonlinear transition 
real(8) :: sdrho(nstage,nfac)               !Standard Error of Parameters for nonlinear 
real(8) :: psi(npsi)                        !Policy Function for Investments                     
real(8) :: sdpsi(npsi)                      !Standard Error for Policy Function for Investments                     

!Parameters for the Mixture Distributions of the Factors
real(8) :: W0(nemf)                         !Initial weights for the factor mixture elements
real(8) :: sdW0(nemf)                       !Standard Error of Initial mean of the factors
real(8) :: a0(nemf,nfac)                    !Initial mean of the factors
real(8) :: sda0(nemf,nfac)                  !Standard Error of Initial mean of the factors
real(8) :: P0(nemf,nfac,nfac)               !Initial variance of the factors
real(8) :: sdP0(nemf,nfac,nfac)             !Standard Error Initial variance of the factors

!Parameters for the Mixture Distributions of the Error Term
real(8) :: H(ntime,nmea,nmea)               !Variance of uniquenesses in dynamic measurement equation
real(8) :: sdH(ntime,nmea,nmea)             !Standard Error of Variance of uniquenesses in measurement equations
real(8) :: HANCH(nanch)                     !Variance of uniquenesses in anchoring equations
real(8) :: sdHANCH(nanch)                   !Standard Error of Variance of uniquenesses in anchoring equations

end module globvar
