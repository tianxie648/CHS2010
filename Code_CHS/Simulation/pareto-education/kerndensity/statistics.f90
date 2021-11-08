MODULE STATISTICS
! SALVADOR NAVARRO-LOZANO
! JANUARY 26, 2004
IMPLICIT NONE
PRIVATE :: quicksort,inssort ! Helper function, same as quicksort in matrix module

INTERFACE MEAN
    MODULE PROCEDURE MEAN_V, MEAN_M
END INTERFACE
	
INTERFACE VARIANCE
    MODULE PROCEDURE VARIANCE_V, VARIANCE_M
END INTERFACE

INTERFACE STDEV
    MODULE PROCEDURE STDEV_V, STDEV_M
END INTERFACE

CONTAINS

REAL(8) FUNCTION MEAN_V(V)
IMPLICIT NONE
REAL(8), INTENT(IN) :: V(:)
MEAN_V = SUM(V) / DBLE(SIZE(V))
END FUNCTION MEAN_V

FUNCTION MEAN_M(M)
IMPLICIT NONE
REAL(8), INTENT(IN) :: M(:,:)
REAL(8) :: MEAN_M(SIZE(M,2)),n
INTEGER :: j
n=DBLE(SIZE(M,1))
DO j = 1, SIZE(M,2)
	MEAN_M(j) = SUM(M(:,j)) / n
END DO
END FUNCTION MEAN_M

REAL(8) FUNCTION VARIANCE_V(V)
IMPLICIT NONE
REAL(8), INTENT(IN) :: V(:)
REAL(8) :: m
m = MEAN(V)
VARIANCE_V = (SUM((V-m)*(V-m))) / (DBLE(SIZE(V))-1.0d0)
END FUNCTION VARIANCE_V

FUNCTION VARIANCE_M(M)
IMPLICIT NONE
REAL(8), INTENT(IN) :: M(:,:)
REAL(8) :: VARIANCE_M(SIZE(M,2))
INTEGER :: j
DO j = 1, SIZE(M,2)
	VARIANCE_M(j) = VARIANCE_V(M(:,j))
END DO
END FUNCTION VARIANCE_M

REAL(8) FUNCTION STDEV_V(V)
IMPLICIT NONE
REAL(8), INTENT(IN) :: V(:)
STDEV_V = DSQRT(VARIANCE_V(V))
END FUNCTION STDEV_V

FUNCTION STDEV_M(M)
IMPLICIT NONE
REAL(8), INTENT(IN) :: M(:,:)
REAL(8) :: STDEV_M(SIZE(M,2))
STDEV_M = DSQRT(VARIANCE_M(M))
END FUNCTION STDEV_M

REAL(8) FUNCTION COVAR(V1,V2)
IMPLICIT NONE
REAL(8), INTENT(IN) :: V1(:),V2(:)
REAL(8) :: m1,m2,a1(SIZE(V1))
m1 = MEAN(V1)
m2 = MEAN(V2)
a1 = (V1 - m1)*(V2-m2)
COVAR = SUM(a1) / (SIZE(V1)-1.0d0)
END FUNCTION COVAR

REAL(8) FUNCTION CORREL(V1,V2)
IMPLICIT NONE
REAL(8), INTENT(IN) :: V1(:),V2(:)
REAL(8) :: m1,m2,a1
a1 = COVAR(V1,V2)
m1 = STDEV(V1)
m2 = STDEV(V2)
CORREL = a1/(m1*m2)
END FUNCTION CORREL

FUNCTION VARCOVAR(A) RESULT(B)
IMPLICIT NONE 
REAL(8), INTENT(IN) :: A(:,:)
REAL(8) :: B(SIZE(A,2),SIZE(A,2))
INTEGER :: i,j
DO i = 1, SIZE(A,2)
	DO j = 1, SIZE(A,2) 
		B(i,j) = COVAR(a(:,i),a(:,j))
	END DO
END DO
END FUNCTION VARCOVAR

FUNCTION HIST(vector,limit)
! H=HIST(vector,limit) returns the histogram of vector
! where H(1) returns the number of elements of vector <= limit(1)
! where H(i) returns the number of elements of vector between limit(i-1) and limit(i) (closed) for i<=SIZE(limitin)
! and H(SIZE(limit)+1) returns the number of elements of vector>limit(SIZE(limit))
! !!!!!!!!!!!Notice: H is an integer!!!!!!!!!!!!!!!!!!!!!!!
IMPLICIT NONE
REAL(8), INTENT(IN) :: vector(:),limit(:)
INTEGER :: HIST(SIZE(limit)+1)
INTEGER :: i,n
n=SIZE(limit)
HIST=0
HIST(1)=COUNT(vector<=limit(1))
DO i = 2, n
	HIST(i)=COUNT((vector>limit(i-1)).AND.(vector<=limit(i)))
END DO
HIST(n+1)=COUNT(vector>limit(n))
END FUNCTION HIST

FUNCTION CENTILE(x,p)
! calculate the percentiles you ask for in p of data vector x
! p then is an real(8) vector between 1 and 100 (so you can ask for the 12.5 centile if you want)
! centile, pin must be of the same size. Uses the same method as Matlab 6.1
IMPLICIT NONE
REAL(8), INTENT(IN) :: x(:),p(:)
REAL(8) :: centile(SIZE(p)),pp(SIZE(p))
REAL(8) :: xx(SIZE(x)+2),q(SIZE(x)+2),step,h(SIZE(x)+1),dxx(SIZE(x)+1)
REAL(8) :: s(SIZE(p)),del(SIZE(x)+1)
INTEGER :: nx,nxx,np,i,ind(MAX(SIZE(x),SIZE(p))),k(SIZE(p))
np=SIZE(p)
nx=SIZE(x)
nxx=nx+2
step = 100.0d0/DBLE(nx)
q(1) = 0.0d0
q(nxx) = 100.0d0
q(2) = 0.5d0*step
DO i = 3, nx+1
	q(i) = q(i-1) + step
END DO
CALL quicksort(x,xx(2:nx+1),ind(1:nx))
xx(1)=xx(2)
xx(nxx)=xx(nxx-1)
DO i = 1, nx+1
	h(i) = q(i+1) - q(i)
	dxx(i) = xx(i+1) - xx(i)
END DO
DO i=1, SIZE(ind)
  	ind(i)=i
END DO
!ind=(/1:SIZE(ind):1/)
CALL quicksort(p,pp,ind(1:np))
k = 0
DO i = 2, nxx
	WHERE ((pp>=q(i-1)).AND.(pp<q(i))) k=i-1
END DO
WHERE (pp==q(nxx)) k=nxx-1
s = pp - q(k)
del = dxx/h
centile(ind(1:np)) = xx(k) + s*del(k)
END FUNCTION CENTILE

!!!!!!!!!!!!!!!!!!!!!! HELPER FUNCTIONS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE QUICKSORT(Xin,xout,ind)
!  Sorts xout into ascENDing order - Quicksort
REAL (8), DIMENSION (:), INTENT (in) :: Xin
INTEGER, INTENT(INOUT) :: ind(:)
REAL (8), DIMENSION (SIZE(ind)), INTENT (Out) :: xout
xout=XIN
Call SUBSOR (xout, 1, Size (xout),ind)
Call INSSORT (xout,ind)
CONTAINS
RECURSIVE SUBROUTINE SUBSOR (xout, IDEB1, IFIN1,ind)
!  Sorts xout from IDEB1 to IFIN1
REAL(8), DIMENSION (:), INTENT (InOut) :: xout
INTEGER, INTENT (In) :: IDEB1, IFIN1
	  INTEGER, INTENT(INOUT) :: ind(:)
INTEGER, PARAMETER :: NINS = 16 ! Max for insertion sort
INTEGER :: ICRS, IDEB, IDCR, IFIN, IMIL,IWRK,IPIV
REAL(8) :: XPIV, XWRK
IDEB = IDEB1
IFIN = IFIN1
!  IF we DOn't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
IF ((IFIN - IDEB) > NINS) THEN
	IMIL = (IDEB+IFIN) / 2
	!  One chooses a pivot, median of 1st, last, and middle values
	IF (xout(IMIL) < xout(IDEB)) THEN
		XWRK = xout (IDEB)
		IWRK = IND (IDEB)
		xout (IDEB) = xout (IMIL)
		IND (IDEB) = IND (IMIL)
		xout (IMIL) = XWRK
		IND (IMIL) = IWRK
	END IF
	IF (xout(IMIL) > xout(IFIN)) Then
		XWRK = xout (IFIN)
		IWRK = IND(IFIN)
		xout (IFIN) = xout (IMIL)
		IND (IFIN) = IND (IMIL)
		xout (IMIL) = XWRK
		IND (IMIL) = IWRK
		IF (xout(IMIL) < xout(IDEB)) Then
			XWRK = xout (IDEB)
			IWRK = IND (IDEB)
			xout (IDEB) = xout (IMIL)
			IND (IDEB) = IND (IMIL)
			xout (IMIL) = XWRK
			IND (IMIL) = IWRK
		END IF
	END IF
	XPIV = xout (IMIL)
	IPIV = IND (IMIL)
	!  One exchanges values to put those > pivot in the END and
	!  those <= pivot at the beginning
	ICRS = IDEB
	IDCR = IFIN
	ECH2: DO
		DO
			ICRS = ICRS + 1
			IF (ICRS >= IDCR) Then
			!  the first  >  pivot is IDCR
			!  the last   <= pivot is ICRS-1
			!  Note: IF one arrives here on the first iteration, then
			!  the pivot is the maximum of the set, the last value is equal
			!  to it, and one can reduce by one the size of the set to process,
			!  as IF xout (IFIN) > XPIV
				Exit ECH2
			END IF
			IF (xout(ICRS) > XPIV) Exit
		END DO
		DO
			IF (xout(IDCR) <= XPIV) Exit
			IDCR = IDCR - 1
			IF (ICRS >= IDCR) Then
			!  The last value < pivot is always ICRS-1
				Exit ECH2
			END IF
		END DO
		XWRK = xout (IDCR)
		IWRK = IND (IDCR)
		xout (IDCR) = xout (ICRS)
		IND (IDCR) = IND (ICRS)
		xout (ICRS) = XWRK
		IND (ICRS) = IWRK
	END DO ECH2
!  One now sorts each of the two sub-intervals
	Call SUBSOR (xout, IDEB1, ICRS-1,IND)
	Call SUBSOR (xout, IDCR, IFIN1,IND)
END IF
RETURN
END SUBROUTINE SUBSOR
END SUBROUTINE QUICKSORT

SUBROUTINE INSSORT (xout,IND)
!  Sorts xout into increasing order (Insertion sort)
REAL(8), DIMENSION (:), INTENT (InOut) :: xout
INTEGER, INTENT(INOUT) :: ind(:)
INTEGER :: ICRS, IDCR,IWRK
REAL(8) :: XWRK
DO ICRS = 2, Size (xout)
	XWRK = xout (ICRS)
	IWRK = IND (ICRS)
	IF (XWRK >= xout(ICRS-1)) Cycle
	xout (ICRS) = xout (ICRS-1)
	IND (ICRS) = IND (ICRS-1)
	DO IDCR = ICRS - 2, 1, - 1
		IF (XWRK >= xout(IDCR)) Exit
		xout (IDCR+1) = xout (IDCR)
		IND (IDCR+1) = IND (IDCR)
	END DO
	xout (IDCR+1) = XWRK
	IND (IDCR+1) = IWRK
END DO
RETURN
END SUBROUTINE INSSORT
END MODULE STATISTICS
