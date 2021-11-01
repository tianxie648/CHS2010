MODULE PARETO_MOD
  USE GLOBVAR
  USE PARETO_AUX
  USE MAPPINGS
  IMPLICIT NONE

CONTAINS 
!!! This is the function called by DFPMIN when minimizing (func in the interface)
  REAL(8) FUNCTION PARETO(theta,ntheta)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: ntheta
    REAL(8), INTENT(IN) :: theta(ntheta)
    REAL(8), PARAMETER :: small=1.0d-250
    REAL(8) :: elapsed_time,crim(nind),aux,temp
    INTEGER :: j,time_in,time_out,time_rate,i,t,c,m

    call getpar(ntheta,theta)  			

    CALL SYSTEM_CLOCK(count_rate=time_rate) ! So we can time the evaluations
    CALL SYSTEM_CLOCK(count=time_in) 
    !$OMP PARALLEL DO
    DO i = 1, nind
        crim(i) = person(i)
!		write(*,*)i,crim(i)
    END DO
    !$OMP END PARALLEL DO
!    pause
    
    PARETO = SUM(crim)

  END FUNCTION PARETO

END MODULE PARETO_MOD
