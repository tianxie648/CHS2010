MODULE NONPARAMETRIC
! Salvador Navarro-Lozano
! January 21, 2004
IMPLICIT NONE

!!!! In all cases the gaussian kernel is true and epanechnikov is false. You want more, add your own

CONTAINS

FUNCTION KERNDENS(x,dom,h,kern)
IMPLICIT NONE
REAL(8), INTENT(IN) :: x(:),dom(:),h
LOGICAL, INTENT(IN) :: kern
REAL(8) :: KERNDENS(SIZE(dom)),k(SIZE(x))
INTEGER :: j
DO j = 1, SIZE(dom)
	IF (kern) THEN
		k = gausskern(dom(j),x,h)
	ELSE
		k = epankern(dom(j),x,h)
	END IF
	KERNDENS(j) = SUM(k)
END DO
KERNDENS = KERNDENS / (h*DBLE(SIZE(X)))
END FUNCTION KERNDENS
	 
FUNCTION gausskern(dom,x,h)
IMPLICIT NONE
REAL(8), INTENT(IN) :: dom,x(:),h
REAL(8), PARAMETER :: root2pi = 2.506628274631000502415765284811D0
REAL(8) :: gausskern(SIZE(x))
gausskern = (dom-x)/h
gausskern = DEXP(-0.5d0*gausskern*gausskern)/root2pi
END FUNCTION gausskern

FUNCTION epankern(dom,x,h)
IMPLICIT NONE
REAL(8), INTENT(IN) :: dom,x(:),h
REAL(8) :: epankern(SIZE(x))
epankern = (dom-x)/h
WHERE (DABS(epankern)>1.0d0) epankern=1.0d0
epankern = 0.75d0*(1-epankern*epankern)
END FUNCTION epankern

END MODULE NONPARAMETRIC
