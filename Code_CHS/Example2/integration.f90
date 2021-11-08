MODULE INTEGRATION
! Routines from Numerical Recipes the art of scientific computing
IMPLICIT NONE
PRIVATE ARTH,TRAPZD,POLINT,IMINLOC,termination2,adaptlobstp
INTEGER :: termination2  

CONTAINS

REAL(8) FUNCTION qsimp(func,a,b,eps,i)
! Returns the integral of the function func from a to b. The parameter EPS should be set to
! the desired fractional accuracy and JMAX so that 2 to the power JMAX-1 is the maximum
! allowed number of steps. Integration is performed by Simpson’s rule.
IMPLICIT NONE
INTEGER, INTENT(IN) :: i
REAL(8), INTENT(IN) :: a,b,eps
INTERFACE
	REAL(8) FUNCTION func(x,i)
	INTEGER, INTENT(IN) :: i
	REAL(8), INTENT(IN) :: x
	END FUNCTION func
END INTERFACE
INTEGER, PARAMETER :: JMAX=50
INTEGER :: j
REAL(8) :: os,ost,st
ost=0.0d0
os= 0.0d0
DO j=1,JMAX
	CALL trapzd(func,a,b,st,j,i)
	qsimp=(4.0d0*st-ost)/3.0d0
	IF (j > 5) THEN
		IF (ABS(qsimp-os) < EPS*ABS(os) .or. (qsimp == 0.0d0 .and. os == 0.0d0)) RETURN
	END IF
	os=qsimp
	ost=st
END DO
PAUSE '***FATAL ERROR: too many steps (QSIMP)***'
STOP
END FUNCTION qsimp

REAL(8) FUNCTION qromb(func,a,b,eps,i)
! Returns the integral of the function func from a to b. Integration is performed by Romberg’s
! method of order 2K, where, e.g., K=2 is Simpson’s rule.
! Parameters: EPS is the fractional accuracy desired, as determined by the extrapolation error
! estimate; JMAX limits the total number of steps; K is the number of points used in the
! extrapolation.
IMPLICIT NONE
REAL(8), INTENT(IN) :: a,b,eps
INTERFACE
	REAL(8) FUNCTION func(x,i)
	INTEGER, INTENT(IN) :: i
	REAL(8), INTENT(IN) :: x
	END FUNCTION func
END INTERFACE
INTEGER, PARAMETER :: JMAX=40,JMAXP=JMAX+1,K=15,KM=K-1
REAL(8), DIMENSION(JMAXP) :: h,s
INTEGER, INTENT(IN) :: i
REAL(8) :: dqromb
INTEGER :: j
h(1)=1.0d0
DO j=1,JMAX
	CALL trapzd(func,a,b,s(j),j,i)
	IF (j >= K) THEN
		CALL polint(h(j-KM:j),s(j-KM:j),0.0d0,qromb,dqromb)
		IF (DABS(dqromb) <= EPS*DABS(qromb)) RETURN
	END IF
	s(j+1)=s(j)
	h(j+1)=0.25d0*h(j)
END DO
PAUSE '***FATAL ERROR: too many steps (QROMB)***'
STOP
END FUNCTION qromb

SUBROUTINE trapzd(func,a,b,s,n,i)
! This routine computes the nth stage of refinement of an extended trapezoidal rule. func is
! input as the name of the function to be integrated between limits a and b, also input. When
! called with n=1, the routine returns as s the crudest estimate of the integral. Subsequent
! calls with n=2,3,... (in that sequential order) will improve the accuracy of s by adding 2n-2
! additional interior points. s should not be modified between sequential calls.
IMPLICIT NONE
INTEGER, INTENT(IN) :: i
REAL(8), INTENT(IN) :: a,b
REAL(8), INTENT(INOUT) :: s
INTEGER, INTENT(IN) :: n
INTERFACE
	REAL(8) FUNCTION func(x,i)
	INTEGER, INTENT(IN) :: i
	REAL(8), INTENT(IN) :: x
	END FUNCTION func
END INTERFACE
REAL(8) :: del,fsum
REAL(8), ALLOCATABLE :: ar(:)
INTEGER :: it,j
IF (n==1) THEN
	fsum=func(a,i)
	fsum=fsum+func(b,i)
	s=0.5D0*(b-a)*fsum
ELSE
	it=2**(n-2)
	del=(b-a)/it
	ALLOCATE(ar(it))
	ar=arth(a+0.5D0*del,del,it)
	fsum=func(ar(1),i)
	DO j = 2, it
		fsum=fsum+func(ar(j),i)
	END DO
	DEALLOCATE(ar)
	s=0.5d0*(s+del*fsum)
END IF
END SUBROUTINE trapzd

FUNCTION qgaus(func,a,b)
REAL(8), INTENT(IN) :: a,b
REAL(8) :: qgaus
INTERFACE
	FUNCTION func(x)
	REAL(8), INTENT(IN) :: x
	REAL(8) :: func
	END FUNCTION func
END INTERFACE
REAL(8) :: xm,xr,ss
REAL(8), DIMENSION(13) :: dx, w = (/0.1183214152792631, 0.1166604434852970, &
    0.1133618165463193, 0.1084718405285772, 0.1020591610944256, 0.0942138003559141, &
    0.0850458943134849, 0.0746841497656597, 0.0632740463295753, 0.0509758252971480, & 
    0.0379623832943635, 0.0244178510926323, 0.0105513726173431/),&
	x = (/0.0592300934293131, 0.1768588203568901, 0.2920048394859567, 0.4030517551234863, &
	     0.5084407148245057, 0.6066922930176182, 0.6964272604199576, 0.7763859488206790,  &
	     0.8454459427884982, 0.9026378619843073, 0.9471590666617146, 0.9783854459564709,  &
	     0.9958857011456171/)
INTEGER :: j
xm=0.5D0*(b+a)
xr=0.5D0*(b-a)
ss=0.0D0
dx=xr*x
DO j = 1, 13
	ss=ss+w(j)*(func(xm+dx(j))+func(xm-dx(j)))
END DO
ss=xr*ss
qgaus=ss
END FUNCTION qgaus

REAL(8) FUNCTION qgaus16(func,a,b,i)
INTEGER, INTENT(IN) :: i
REAL(8), INTENT(IN) :: a,b
INTERFACE
    REAL(8) FUNCTION func(x,i)
    INTEGER, INTENT(IN) :: i
    REAL(8), INTENT(IN) :: x
    END FUNCTION func
END INTERFACE
REAL(8) :: xm,xr,ss
REAL(8), DIMENSION(8) :: dx,w = (/0.18945061045507,0.18260341504492,0.16915651939500, &
    0.14959598881658,0.12462897125554,0.09515851168249,0.06225352393865,0.02715245941175/),&
	x = (/0.09501250983764,0.28160355077926,0.45801677765723,0.61787624440264, &
	0.75540440835500,0.86563120238783,0.94457502307323,0.98940093499165/)
INTEGER :: j
xm=0.5D0*(b+a)
xr=0.5D0*(b-a)
ss=0.0D0
dx=xr*x
DO j = 1, 8
	ss=ss+w(j)*(func(xm+dx(j),i)+func(xm-dx(j),i))
END DO
ss=xr*ss
qgaus16=ss
END FUNCTION qgaus16

REAL(8) FUNCTION qgaus8(func,a,b,i)
INTEGER, INTENT(IN) :: i
REAL(8), INTENT(IN) :: a,b
INTERFACE
    REAL(8) FUNCTION func(x,i)
    INTEGER, INTENT(IN) :: i
    REAL(8), INTENT(IN) :: x
    END FUNCTION func
END INTERFACE
REAL(8) :: xm,xr,ss
REAL(8), DIMENSION(4) :: dx,w = (/0.36268378337836,0.31370664587789,0.22238103445337, &
    0.10122853629038/),&
	x = (/0.18343464249565,0.52553240991633,0.79666647741363,0.96028985649754/)
INTEGER :: j
xm=0.5D0*(b+a)
xr=0.5D0*(b-a)
ss=0.0D0
dx=xr*x
DO j = 1, 4
	ss=ss+w(j)*(func(xm+dx(j),i)+func(xm-dx(j),i))
END DO
ss=xr*ss
qgaus8=ss
END FUNCTION qgaus8

RECURSIVE FUNCTION ADAPTIVE_QUAD_16_8(func,lo,hi,eps,i) RESULT (res)
IMPLICIT NONE
REAL(8), INTENT(IN) :: lo,hi,eps
INTEGER, INTENT(IN) :: i
INTERFACE
    REAL(8) FUNCTION func(x,i)
    INTEGER, INTENT(IN) :: i
    REAL(8), INTENT(IN) :: x
    END FUNCTION func
END INTERFACE
REAL(8) :: res
REAL(8) :: g8,g16,middle_point
REAL(8) :: left_area,right_area
REAL(8) :: diff
middle_point=0.5d0*(hi+lo)
g8 = qgaus8(func,lo,hi,i)
g16 = qgaus16(func,lo,hi,i)
diff = DABS(g16-g8)
IF ((diff < EPS*DABS(g8)).OR.((g16==0.0).AND.g8==0.0).OR.(DABS(lo-middle_point)<=0.01d0)) THEN
    res = g16
ELSE
    left_area = ADAPTIVE_QUAD_16_8(func,lo,middle_point,EPS,i)
    right_area = ADAPTIVE_QUAD_16_8(func,middle_point,hi,EPS,i)
    res = left_area + right_area
END IF
END FUNCTION ADAPTIVE_QUAD_16_8

REAL(8) FUNCTION qgaus20(func,a,b,i)
INTEGER, INTENT(IN) :: i
REAL(8), INTENT(IN) :: a,b
INTERFACE
    REAL(8) FUNCTION func(x,i)
    INTEGER, INTENT(IN) :: i
    REAL(8), INTENT(IN) :: x
    END FUNCTION func
END INTERFACE
REAL(8) :: xm,xr,ss
REAL(8), DIMENSION(10) :: dx,w = (/ 0.15275338713072598d0,0.14917298647260382d0, &
 0.14209610931838190d0,0.13168863844917658d0,0.11819453196151775d0,0.10193011981723323d0, &
 0.08327674157670475d0,0.06267204833410904d0,0.04060142980038705d0,0.01761400713915058d0/),&
 x = (/ 0.07652652113349734d0,0.22778585114164510d0,0.37370608871541955d0, &
 0.51086700195082713d0,0.63605368072651502d0,0.74633190646015080d0, &
 0.83911697182221889d0,0.91223442825132595d0,0.96397192727791381d0,0.99312859918509488d0/)
INTEGER :: j
xm=0.5D0*(b+a)
xr=0.5D0*(b-a)
ss=0.0D0
dx=xr*x
DO j = 1, 10
	ss=ss+w(j)*(func(xm+dx(j),i)+func(xm-dx(j),i))
END DO
ss=xr*ss
qgaus20=ss
END FUNCTION qgaus20

REAL(8) FUNCTION qgaus10(func,a,b,i)
INTEGER, INTENT(IN) :: i
REAL(8), INTENT(IN) :: a,b
INTERFACE
    REAL(8) FUNCTION func(x,i)
    INTEGER, INTENT(IN) :: i
    REAL(8), INTENT(IN) :: x
    END FUNCTION func
END INTERFACE
REAL(8) :: xm,xr,ss
REAL(8), DIMENSION(5) :: dx,w = (/ 0.29552422471475293d0,0.26926671930999174d0, &
 0.21908636251598215d0,0.14945134915058050d0,0.06667134430868371d0/), &
 x = (/ 0.14887433898163122d0,0.43339539412924716d0, &
 0.67940956829902444d0,0.86506336668898454d0,0.97390652851717174d0/)
INTEGER :: j
xm=0.5D0*(b+a)
xr=0.5D0*(b-a)
ss=0.0D0
dx=xr*x
DO j = 1, 5
	ss=ss+w(j)*(func(xm+dx(j),i)+func(xm-dx(j),i))
END DO
ss=xr*ss
qgaus10=ss
END FUNCTION qgaus10

RECURSIVE FUNCTION ADAPTIVE_QUAD_20_10(func,lo,hi,eps,i) RESULT (res)
IMPLICIT NONE
REAL(8), INTENT(IN) :: lo,hi,eps
INTEGER, INTENT(IN) :: i
INTERFACE
    REAL(8) FUNCTION func(x,i)
    INTEGER, INTENT(IN) :: i
    REAL(8), INTENT(IN) :: x
    END FUNCTION func
END INTERFACE
REAL(8) :: res
REAL(8) :: g10,g20,middle_point
REAL(8) :: left_area,right_area
REAL(8) :: diff
middle_point=0.5d0*(hi+lo)
g10 = qgaus10(func,lo,hi,i)
g20 = qgaus20(func,lo,hi,i)
diff = DABS(g20-g10)
IF ((diff < EPS*DABS(g10)).OR.((g20==0.0).AND.g10==0.0).OR.(DABS(lo-middle_point)<=0.01d0)) THEN
    res = g20
ELSE
    left_area = ADAPTIVE_QUAD_20_10(func,lo,middle_point,EPS,i)
    right_area = ADAPTIVE_QUAD_20_10(func,middle_point,hi,EPS,i)
    res = left_area + right_area
END IF
END FUNCTION ADAPTIVE_QUAD_20_10

REAL(8) FUNCTION adaptlob(func,a,b,tolin,i)
IMPLICIT NONE
REAL(8), INTENT(IN) :: a,b,tolin
INTEGER, INTENT(IN) :: i
INTERFACE
    REAL(8) FUNCTION func(x,i)
    INTEGER, INTENT(IN) :: i
    REAL(8), INTENT(IN) :: x
    END FUNCTION func
END INTERFACE
INTEGER :: j
REAL(8) :: m,h,x(13),y(13),fa,fb,i2,i1,is,s,erri1,erri2,R,tol
REAL(8), PARAMETER :: alpha=0.81649658092772603273242802490196d0, & 
	beta=0.44721359549995793928183473374626d0, & 
	x1=0.942882415695480d0,x2=0.641853342345781d0,x3=0.236383199662150d0,EPS=2.220446049250313D-016
termination2=0
tol=tolin
IF (tolin<EPS) tol=EPS
m=0.5d0*(a+b)
h=0.5d0*(b-a)
x=(/a,m-x1*h,m-alpha*h,m-x2*h,m-beta*h,m-x3*h,m,m+x3*h,m+beta*h,m+x2*h,m+alpha*h,m+x1*h,b/)
DO j = 1, 13
	y(j)=func(x(j),i)
END DO
fa=y(1)
fb=y(13)
i2=(h/6.0d0)*(y(1)+y(13)+ (5.0d0*(y(5)+y(9))) )
i1=(h/1470.0d0)*( (77.0d0*(y(1)+y(13)))  + (432.0d0*(y(3)+y(11))) + (625.0d0*(y(5)+y(9))) + &
    			  (672.0d0*y(7)) )
is=h*( (0.0158271919734802d0*(y(1)+y(13))) + (0.0942738402188500d0*(y(2)+y(12))) + &
	   (0.1550719873365850d0*(y(3)+y(11))) + (0.1888215739601820d0*(y(4)+y(10))) + &
	   (0.1997734052268590d0*(y(5)+ y(9))) + (0.2249264653333400d0*(y(6)+ y(8))) + &
	   (0.2426110719014080d0*y(7)) )
IF (is==0.0d0) THEN
	s=1.0d0
ELSE
	s=SIGN(1.0d0,is)
END IF
erri1=DABS(i1-is)
erri2=DABS(i2-is)
R=erri1/erri2
IF ((R>0.0d0).AND.(R<1.0d0)) tol=tol/R
is=s*DABS(is)*tol/EPS
IF (is==0.0d0) is=b-a
adaptlob=adaptlobstp(func,a,b,fa,fb,is,i)
END FUNCTION adaptlob

RECURSIVE REAL(8) FUNCTION adaptlobstp(func,a,b,fa,fb,is,i)
IMPLICIT NONE
REAL(8), INTENT(IN) :: a,b,fa,fb,is
INTEGER, INTENT(IN) :: i
INTERFACE
    REAL(8) FUNCTION func(x,i)
    INTEGER, INTENT(IN) :: i
    REAL(8), INTENT(IN) :: x
    END FUNCTION func
END INTERFACE
INTEGER :: j
REAL(8) :: m,h,mll,ml,mr,mrr,x(5),y(5),fm,fmll,fml,fmr,fmrr,i2,i1
REAL(8), PARAMETER :: alpha=0.81649658092772603273242802490196d0, & !SQRT(2/3)
	beta=0.44721359549995793928183473374626d0 !1/SQRT(5)
m=0.5d0*(a+b)
h=0.5d0*(b-a)
mll=m-alpha*h
ml=m-beta*h
mr=m+beta*h
mrr=m+alpha*h
x=(/mll,ml,m,mr,mrr/)
DO j = 1, 5
	y(j)=func(x(j),i)
END DO
fmll=y(1)
fml=y(2)
fm=y(3)
fmr=y(4)
fmrr=y(5)
i2=(h/6.0d0)*(fa+fb+ (5.0d0*(fml+fmr)) )
i1=(h/1470.0d0)*( (77.0d0*(fa+fb))  + (432.0d0*(fmll+fmrr)) + (625.0d0*(fml+fmr)) + &
    			  (672.0d0*fm) )
IF ( (is+(i1-i2)==is).OR.(mll<=a).OR.(b<=mrr) ) THEN
	IF ( ((m<=a).OR.(b<=m)) .AND. (termination2==0) ) THEN
		PAUSE 'Interval contains no more machine number. Required tolerance may not be met'
		termination2=1
	END IF
	adaptlobstp=i1
ELSE
	adaptlobstp=adaptlobstp(func,a,mll,fa,fmll,is,i) + &
				adaptlobstp(func,mll,ml,fmll,fml,is,i) + &
				adaptlobstp(func,ml,m,fml,fm,is,i) + &
				adaptlobstp(func,m,mr,fm,fmr,is,i) + &
				adaptlobstp(func,mr,mrr,fmr,fmrr,is,i) + &
				adaptlobstp(func,mrr,b,fmrr,fb,is,i)
END IF
END FUNCTION adaptlobstp

SUBROUTINE gauleg(x1,x2,x,w)
!Given the lower and upper limits of integration x1 and x2, this routine returns arrays x and w
!of length N containing the abscissas and weights of the Gauss-Legendre N-point quadrature
!formula. The parameter EPS is the relative precision.
IMPLICIT NONE
REAL(8), INTENT(IN) :: x1,x2
REAL(8), DIMENSION(:), INTENT(OUT) :: x,w
REAL(8), PARAMETER :: EPS=2.220446049250313D-016
INTEGER :: its,j,m,n
INTEGER, PARAMETER :: MAXIT=20
REAL(8) :: xl,xm
REAL(8), DIMENSION((size(x)+1)/2) :: p1,p2,p3,pp,z,z1
LOGICAL, DIMENSION((size(x)+1)/2) :: unfinished
REAL(8), PARAMETER :: PI_D=3.141592653589793238462643383279502884197d0
n=size(x)
m=(n+1)/2
xm=0.5d0*(x2+x1)
xl=0.5d0*(x2-x1)
z=DCOS(PI_D*(arthi(1,1,m)-0.25d0)/(n+0.5d0))
unfinished=.true.
DO its=1,MAXIT
	WHERE (unfinished)
		p1=1.0d0
		p2=0.0d0
	END WHERE
	DO j=1,n
		WHERE (unfinished)
			p3=p2
			p2=p1
			p1=((2.0d0*j-1.0d0)*z*p2-(j-1.0d0)*p3)/j
		END WHERE
	END DO
	WHERE (unfinished)
		pp=n*(z*p1-p2)/(z*z-1.0d0)
		z1=z
		z=z1-p1/pp
		unfinished=(ABS(z-z1) > EPS)
	END WHERE
	IF (.not. ANY(unfinished)) EXIT
END DO
IF (its == MAXIT+1) THEN
    PAUSE '***FATAL ERROR: too many iterations (GAULEG)***'
    STOP
END IF    
x(1:m)=xm-xl*z
x(n:n-m+1:-1)=xm+xl*z
w(1:m)=2.0d0*xl/((1.0d0-z**2)*pp**2)
w(n:n-m+1:-1)=w(1:m)
END SUBROUTINE gauleg

SUBROUTINE gaulag(x,w,alf)
! Given alf, the parameter á of the Laguerre polynomials, this routine returns arrays x and w
! of length N containing the abscissas and weights of the N-point Gauss-Laguerre quadrature
! formula. The abscissas are returned in ascending order. The parameter EPS is the relative
! precision.
IMPLICIT NONE
REAL(8), INTENT(IN) :: alf
REAL(8), DIMENSION(:), INTENT(OUT) :: x,w
REAL(8), PARAMETER :: EPS=2.220446049250313D-016
INTEGER :: its,j,n
INTEGER, PARAMETER :: MAXIT=20
REAL(8) :: anu
REAL(8), PARAMETER :: C1=9.084064d-01,C2=5.214976d-02,&
		C3=2.579930d-03,C4=3.986126d-03
REAL(8), DIMENSION(size(x)) :: rhs,r2,r3,theta
REAL(8), DIMENSION(size(x)) :: p1,p2,p3,pp,z,z1
LOGICAL, DIMENSION(size(x)) :: unfinished
REAL(8), PARAMETER :: PI=3.141592653589793238462643383279502884197d0
n=size(x)
anu=4.0d0*n+2.0d0*alf+2.0d0
rhs=arthi(4*n-1,-4,n)*PI/anu
r3=rhs**(1.0d0/3.0d0)
r2=r3**2
theta=r3*(C1+r2*(C2+r2*(C3+r2*C4)))
z=anu*COS(theta)**2
unfinished=.true.
DO its=1,MAXIT
	WHERE (unfinished)
		p1=1.0d0
		p2=0.0d0
	END WHERE
	DO j=1,n
		WHERE (unfinished)
			p3=p2
			p2=p1
			p1=((2.0d0*j-1.0d0+alf-z)*p2-(j-1.0d0+alf)*p3)/j
		END WHERE
	END DO
	WHERE (unfinished)
		pp=(n*p1-(n+alf)*p2)/z
		z1=z
		z=z1-p1/pp
		unfinished=(ABS(z-z1) > EPS*z)
	END WHERE
	IF (.not. ANY(unfinished)) EXIT
END DO
IF (its == MAXIT+1) THEN
    PAUSE '***FATAL ERROR: too many iterations (GAULAG)***'
    STOP
END IF
x=z
w=-DEXP(gammln(alf+n)-gammln(REAL(n,8)))/(pp*n*p2)
END SUBROUTINE gaulag

SUBROUTINE gauher(x,w)
!This routine returns arrays x and w of length N containing the abscissas and weights of
!the N-point Gauss-Hermite quadrature formula. The abscissas are returned in descending
!order. Note that internal computations are done in double precision.
!Parameters: EPS is the relative precision.
IMPLICIT NONE
REAL(8), DIMENSION(:), INTENT(OUT) :: x,w
REAL(8), PARAMETER :: EPS=2.220446049250313D-016,PIM4=0.7511255444649425d0
INTEGER :: its,j,m,n
INTEGER, PARAMETER :: MAXIT=20
REAL(8) :: anu
REAL(8), PARAMETER :: C1=9.084064d-01,C2=5.214976d-02,&
		C3=2.579930d-03,C4=3.986126d-03
REAL(8), DIMENSION((size(x)+1)/2) :: rhs,r2,r3,theta
REAL(8), DIMENSION((size(x)+1)/2) :: p1,p2,p3,pp,z,z1
LOGICAL, DIMENSION((size(x)+1)/2) :: unfinished
REAL(8), PARAMETER :: PI=3.141592653589793238462643383279502884197d0
n=size(x)
m=(n+1)/2
anu=2.0d0*n+1.0d0
rhs=arthi(3,4,m)*PI/anu
r3=rhs**(1.0d0/3.0d0)
r2=r3**2
theta=r3*(C1+r2*(C2+r2*(C3+r2*C4)))
z=DSQRT(anu)*COS(theta)
unfinished=.true.
DO its=1,MAXIT
	WHERE (unfinished)
		p1=PIM4
		p2=0.0d0
	END WHERE
	DO j=1,n
		WHERE (unfinished)
			p3=p2
			p2=p1
			p1=z*DSQRT(2.0d0/j)*p2-DSQRT(real(j-1,8)/real(j,8))*p3
		END WHERE
	END DO
	WHERE (unfinished)
		pp=DSQRT(2.0d0*n)*p2
		z1=z
		z=z1-p1/pp
		unfinished=(ABS(z-z1) > EPS)
	END WHERE
	IF (.not. ANY(unfinished)) EXIT
END DO
IF (its == MAXIT+1) THEN
    PAUSE '***FATAL ERROR: too many iterations (GAUHER)***'
    STOP
END IF
x(1:m)=z
x(n:n-m+1:-1)=-z
w(1:m)=2.0d0/pp**2
w(n:n-m+1:-1)=w(1:m)
END SUBROUTINE gauher

SUBROUTINE gaujac(x,w,alf,bet)
!Given alf and bet, the parameters á and â of the Jacobi polynomials, this routine returns
!arrays x and w of length N containing the abscissas and weights of the N-point Gauss-
!Jacobi quadrature formula. The abscissas are returned in descending order. The parameter
!EPS is the relative precision.
IMPLICIT NONE
REAL(8), INTENT(IN) :: alf,bet
REAL(8), DIMENSION(:), INTENT(OUT) :: x,w
REAL(8), PARAMETER :: EPS=2.220446049250313D-016
INTEGER :: its,j,n
INTEGER, PARAMETER :: MAXIT=20
REAL(8) :: alfbet,a,c,temp
REAL(8), DIMENSION(size(x)) :: b,p1,p2,p3,pp,z,z1
LOGICAL, DIMENSION(size(x)) :: unfinished
REAL(8), PARAMETER :: PI=3.141592653589793238462643383279502884197d0
n=size(x)
alfbet=alf+bet
z=COS(PI*(arthi(1,1,n)-0.25d0+0.5d0*alf)/(n+0.5d0*(alfbet+1.0d0)))
unfinished=.true.
DO its=1,MAXIT
	temp=2.0d0+alfbet
	WHERE (unfinished)
		p1=(alf-bet+temp*z)/2.0d0
		p2=1.0d0
	END WHERE
	DO j=2,n
		a=2*j*(j+alfbet)*temp
		temp=temp+2.0d0
		c=2.0d0*(j-1.0d0+alf)*(j-1.0d0+bet)*temp
		WHERE (unfinished)
			p3=p2
			p2=p1
			b=(temp-1.0d0)*(alf*alf-bet*bet+temp*&
				(temp-2.0d0)*z)
			p1=(b*p2-c*p3)/a
		END WHERE
	END DO
	WHERE (unfinished)
		pp=(n*(alf-bet-temp*z)*p1+2.0d0*(n+alf)*&
			(n+bet)*p2)/(temp*(1.0d0-z*z))
		z1=z
		z=z1-p1/pp
		unfinished=(ABS(z-z1) > EPS)
	END WHERE
IF (.not. ANY(unfinished)) EXIT
END DO
IF (its == MAXIT+1) THEN
    PAUSE '***FATAL ERROR: too many iterations (GAUJAC)***'
    STOP
END IF
x=z
w=DEXP(gammln(alf+n)+gammln(bet+n)-gammln(n+1.0d0)-&
	gammln(n+alf+bet+1.0d0))*temp*2.0d0**alfbet/(pp*p2)
END SUBROUTINE gaujac


!!!!!!!!!!!!HELPER FUNCTIONS!!!!!!!!!!!!!!!!!!!!!!!!!!!!
FUNCTION arth(first,increment,n)
! Array function returning an arithmetic progression.
REAL(8), INTENT(IN) :: first,increment
INTEGER, INTENT(IN) :: n
REAL(8), DIMENSION(n) :: arth
INTEGER :: k,k2
REAL(8) :: temp
if (n > 0) arth(1)=first
if (n <= 16) then
do k=2,n
arth(k)=arth(k-1)+increment
end do
else
do k=2,8
arth(k)=arth(k-1)+increment
end do
temp=increment*8
k=8
do
if (k >= n) exit
k2=k+k
arth(k+1:min(k2,n))=temp+arth(1:min(k,n-k))
temp=temp+temp
k=k2
end do
end if
END FUNCTION arth

FUNCTION arthi(first,increment,n)
INTEGER, INTENT(IN) :: first,increment,n
INTEGER, DIMENSION(n) :: arthi
INTEGER :: k,k2,temp
if (n > 0) arthi(1)=first
if (n <= 16) then
	do k=2,n
		arthi(k)=arthi(k-1)+increment
	end do
else
	do k=2,8
		arthi(k)=arthi(k-1)+increment
	end do
	temp=increment*8
	k=8
	do
		if (k >= n) exit
		k2=k+k
		arthi(k+1:min(k2,n))=temp+arthi(1:min(k,n-k))
		temp=temp+temp
		k=k2
	end do
end if
END FUNCTION arthi

SUBROUTINE polint(xa,ya,x,y,dy)
IMPLICIT NONE
REAL(8), DIMENSION(:), INTENT(IN) :: xa,ya
REAL(8), INTENT(IN) :: x
REAL(8), INTENT(OUT) :: y,dy
INTEGER :: m,n,ns
REAL, DIMENSION(size(xa)) :: c,d,den,ho
c=ya
d=ya
ho=xa-x
ns=iminloc(abs(x-xa))
y=ya(ns)
ns=ns-1
n=SIZE(xa)
do m=1,n-1
	den(1:n-m)=ho(1:n-m)-ho(1+m:n)
	if (any(den(1:n-m) == 0.0)) THEN
	    PAUSE '***FATAL ERROR polint: calculation failure***'
	    STOP
	END IF    
	den(1:n-m)=(c(2:n-m+1)-d(1:n-m))/den(1:n-m)
	d(1:n-m)=ho(1+m:n)*den(1:n-m)
	c(1:n-m)=ho(1:n-m)*den(1:n-m)
	if (2*ns < n-m) then
		dy=c(ns+1)
	else
		dy=d(ns)
		ns=ns-1
	end if
	y=y+dy
end do
END SUBROUTINE polint

FUNCTION iminloc(arr)
REAL(8), DIMENSION(:), INTENT(IN) :: arr
INTEGER, DIMENSION(1) :: imin
INTEGER :: iminloc
imin=minloc(arr(:))
iminloc=imin(1)
END FUNCTION iminloc

REAL(8) FUNCTION gammln(xx)
IMPLICIT NONE
REAL(8), INTENT(IN) :: xx
REAL(8) :: tmp,x
REAL(8) :: stp = 2.5066282746310005d0
REAL(8), DIMENSION(6) :: coef = (/76.18009172947146d0,-86.50532032941677d0, &
    24.01409824083091d0,-1.231739572450155d0,0.1208650973866179d-2,-0.5395239384953d-5/)
IF (xx<0.0d0) THEN
    PAUSE '***FATAL ERROR: argument has to be postive (gammln_s)***'
    STOP
END IF    
x=xx
tmp=x+5.5d0
tmp=(x+0.5d0)*DLOG(tmp)-tmp
gammln=tmp+DLOG(stp*(1.000000000190015d0+&
		SUM(coef(:)/arth(x+1.0d0,1.0d0,SIZE(coef))))/x)
END FUNCTION gammln

END MODULE INTEGRATION
