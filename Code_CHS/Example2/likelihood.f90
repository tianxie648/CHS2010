MODULE LIKELIHOOD
  USE GLOBVAR
  USE PROBABILITY, ONLY : LN_DIRICHPDF,LN_GAMPDF,LN_NORMPDF
  USE LIKE_AUX
  USE MAPPINGS
  IMPLICIT NONE

CONTAINS 
!!! This is the function called by DFPMIN when minimizing (func in the interface)
  REAL(8) FUNCTION LOGLIKELIHOOD(theta,ntheta)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: ntheta
    REAL(8), INTENT(IN) :: theta(ntheta)
    REAL(8), PARAMETER :: small=1.0d-250
    REAL(8) :: elapsed_time,prob(nind),p_dirich(5),aux,temp
    INTEGER :: j,time_in,time_out,time_rate,i,t,c,m

    call getpar(ntheta,theta)  			

    CALL SYSTEM_CLOCK(count_rate=time_rate) ! So we can time the evaluations
    CALL SYSTEM_CLOCK(count=time_in) 
    !$OMP PARALLEL DO
    DO i = 1, nind
		prob(i) = density(i)
!		write(*,*)i,prob(i)
		IF (prob(i)<small) prob(i)=small
    END DO
    !$OMP END PARALLEL DO
    LOGLIKELIHOOD = -SUM(DLOG(PROB))

!    pause

  END FUNCTION LOGLIKELIHOOD

END MODULE LIKELIHOOD
