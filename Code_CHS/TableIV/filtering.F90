module filtering
    use probability
    use matrix
    use globvar
    use utilities
    implicit none

contains

subroutine get_filter(i,afilter)
    implicit none
    integer, intent(in) :: i
    real(8), intent(out) :: afilter(ntime,nfac)
    integer :: t,j,m,n,k,s,k1,k2
    integer :: tstart
    integer, parameter :: nsigma = 2*nfac + 1
    real(8), parameter :: kappa = 2.0d0
    real(8) :: a(ntime,nemf,nmea+1,nfac)									!Mean of factor
    real(8) :: P(ntime,nemf,nmea+1,nfac,nfac)					            !Variance of factor
    real(8) :: W(ntime,nemf,nmea+1)
    real(8) :: KF(ntime,nemf,nmea,nfac)
    real(8) :: v(ntime,nemf,nmea)
    real(8) :: aux,denominator,temp
    real(8) :: SUMW(ntime,nmea)
    real(8) :: WY(ntime,nmea,nemf)
    real(8) :: MY(ntime,nmea,nemf)
    real(8) :: FY(ntime,nmea,nemf)
    real(8) :: prob(ntime,nmea,nemf)
    real(8) :: aup(nfac)                                !Updated Mean of the Factor (For Unscent Transform)
    real(8) :: bup(nfac,nfac)
    real(8) :: MYk, FYk,xhat(nsigma)
    real(8) :: asigma(nsigma,nfac)
    real(8) :: wsigma(nsigma)
    real(8) :: diffvec(nsigma,nfac)
    real(8) :: diffout(nsigma,nfac,nfac)
    integer :: nobs(ntime)
    integer :: indeq(ntime,nmea)                                
    real(8) :: muanch(nanch,nemf)
    real(8) :: varanch(nanch,nemf)    
    real(8) :: wanch(nanch+1,nemf)                         
    real(8) :: pranch(nanch,nemf)
    real(8) :: sumanch(nanch)
    real(8) :: KFANCH(nemf,nanch,nfac)
    real(8) :: vANCH(nemf,nanch)
    real(8) :: aANCH(nemf,nanch+1,nfac)		
    real(8) :: PANCH(nemf,nanch+1,nfac,nfac)						
    real(8) :: YSTAR

	!Recording how many and which equations are not missing
    do t = 1, ntime
        nobs(t) = 0
        do j = 1, nequation(t)
            k = eqindex(t,j)
            if (dY(i,t,k) == 1) then
                nobs(t) = nobs(t) + 1
                indeq(t,nobs(t)) = k
            end if
        end do
    end do

    !Identify the first equation for which observation is not missing
    do t = 1, ntime
        if ( nobs(t) > 0 ) then
            tstart = t
            exit
        end if
    end do

    a(1,:,1,:) = a0
    P(1,:,1,:,:) = P0
    W(1,:,1) = W0
    sumw = 0.0d0
    if (tstart > 1) then
	    do t = 1, tstart-1
		    s = stage(t)
		    do m = 1, nemf
                !-------------------------------------------------------------------
                !Prediction according to the Unscented Kalman Filter
                aup = a(t,m,1,:)
                bup = P(t,m,1,:,:)
                call UT(nfac,nsigma,aup,kappa,bup,asigma,wsigma)
                a(t+1,m,1,:) = 0.0d0
                do k1 = 1, nfac
                    do k2 = 1, nsigma
                        a(t+1,m,1,k1) = a(t+1,m,1,k1) + wsigma(k2)*transition(s,k1,nfac,asigma(k2,:))
                    end do
                end do
                P(t+1,m,1,:,:) = Q(s,:,:)
                diffvec = 0.0d0
                diffout = 0.0d0
                do j = 1, nsigma
                    do k1 = 1, nfac
                        aux = transition(s,k1,nfac,asigma(j,:))
                        diffvec(j,k1) = aux - a(t+1,m,1,k1)
                    end do
                    do k1 = 1, nfac
                        do k2 = 1, nfac
                            diffout(j,k1,k2) = diffvec(j,k1)*diffvec(j,k2)
                        end do
                    end do
                    P(t+1,m,1,:,:) = P(t+1,m,1,:,:) + wsigma(j)*diffout(j,:,:)
                end do
                !-------------------------------------------------------------------
                W(t+1,m,1) = W(t,m,1)
            end do
	    end do
    end if

    do t = tstart, ntime-1
        s = stage(t)
        if (nobs(t) == 0) then
            do m = 1, nemf
                !-------------------------------------------------------------------
                !Prediction according to the Unscented Kalman Filter
                aup = a(t,m,1,:)
                bup = P(t,m,1,:,:)
                call UT(nfac,nsigma,aup,kappa,bup,asigma,wsigma)
                a(t+1,m,1,:) = 0.0d0
                do k1 = 1, nfac
                    do k2 = 1, nsigma
                        a(t+1,m,1,k1) = a(t+1,m,1,k1) + wsigma(k2)*transition(s,k1,nfac,asigma(k2,:))
                    end do
                end do
                P(t+1,m,1,:,:) = Q(s,:,:)
                diffvec = 0.0d0
                diffout = 0.0d0
                do j = 1, nsigma
                    do k1 = 1, nfac
                        aux = transition(s,k1,nfac,asigma(j,:))
                        diffvec(j,k1) = aux - a(t+1,m,1,k1)
                    end do
                do k1 = 1, nfac
                    do k2 = 1, nfac
                        diffout(j,k1,k2) = diffvec(j,k1)*diffvec(j,k2)
                    end do
                end do
                P(t+1,m,1,:,:) = P(t+1,m,1,:,:) + wsigma(j)*diffout(j,:,:)
                end do
                !-------------------------------------------------------------------
                W(t+1,m,1) = W(t,m,1)
            end do
	    end if
        if (nobs(t) > 0) then
            do j = 1, nobs(t)
                temp = 0.0d0
                if (nonlinear(t,indeq(t,j))==0) then
                    do m = 1, nemf
                        MY(t,j,m) = dot_product(beta(t,indeq(t,j),:),X(i,t,:)) + dot_product(Z(t,indeq(t,j),:),a(t,m,j,:))
                        FY(t,j,m) = dot_product(Z(t,indeq(t,j),:),matmul(P(t,m,j,:,:),Z(t,indeq(t,j),:))) + H(t,indeq(t,j),indeq(t,j))
                        WY(t,j,m) = W(t,m,j)
                        prob(t,j,m) = NORMPDF(Y(i,t,indeq(t,j)),MY(t,j,m),FY(t,j,m))
                        temp = temp + WY(t,j,m)*prob(t,j,m)
                        sumw(t,j) = sumw(t,j) + W(t,m,j)*prob(t,j,m)
                        KF(t,m,j,:) = matmul(P(t,m,j,:,:),Z(t,indeq(t,j),:))
			            v(t,m,j) = Y(i,t,indeq(t,j)) - MY(t,j,m)
			            a(t,m,j+1,:)   = a(t,m,j,:) + KF(t,m,j,:)*v(t,m,j)/FY(t,j,m)
			            P(t,m,j+1,:,:) = P(t,m,j,:,:) - outerprod(KF(t,m,j,:),KF(t,m,j,:))/FY(t,j,m)
                    end do
                else if (nonlinear(t,indeq(t,j))==1) then
                    do m = 1, nemf
                        call UT(nfac,nsigma,a(t,m,j,:),kappa,P(t,m,j,:,:),asigma,wsigma)
                        if (typefunct(t,indeq(t,j))==2) then
                            MY(t,j,m) = 0.0d0
                            FY(t,j,m) = H(t,indeq(t,j),indeq(t,j))
                            do k1 = 1, nsigma
                                xhat(k1) = dot_product(beta(t,indeq(t,j),:),X(i,t,:)) + dot_product(Z(t,indeq(t,j),:),asigma(k1,:))                     
                                MY(t,j,m) = MY(t,j,m) + wsigma(k1)*fnl(xhat(k1))
                            end do
                            do k1 = 1, nsigma
                                FY(t,j,m) = FY(t,j,m) + wsigma(k1)*(fnl(xhat(k1))-MY(t,j,m))*(fnl(xhat(k1))-MY(t,j,m))       
                            end do
                            WY(t,j,m) = W(t,m,j)
                            prob(t,j,m) = NORMPDF(Y(i,t,indeq(t,j)),MY(t,j,m),FY(t,j,m))
                            temp = temp + WY(t,j,m)*prob(t,j,m)
                            sumw(t,j) = sumw(t,j) + W(t,m,j)*prob(t,j,m)
                            KF(t,m,j,:) = 0.0d0
                            !Update mean vector and variance-covariance matrix
                            KF(t,m,j,:) = 0.0d0
                            do k1 = 1, nsigma
                                xhat(k1) = dot_product(beta(t,indeq(t,j),:),X(i,t,:)) + dot_product(Z(t,indeq(t,j),:),asigma(k1,:))
                                KF(t,m,j,:) = KF(t,m,j,:) + wsigma(k1)*(fnl(xhat(k1))-MY(t,j,m))*(asigma(k1,:)-a(t,m,j,:))  
                            end do
                            v(t,m,j) = Y(i,t,indeq(t,j)) - MY(t,j,m)
                            a(t,m,j+1,:) = a(t,m,j,:) + KF(t,m,j,:)*v(t,m,j)/FY(t,j,m) 
                            P(t,m,j+1,:,:) = P(t,m,j,:,:) - outerprod(KF(t,m,j,:),KF(t,m,j,:))/FY(t,j,m)
                       end if                            
                    end do
                end if
                !Compute Likelihood
                !Update Weights
                do m = 1, nemf
                    W(t,m,j+1) = W(t,m,j)*prob(t,j,m)/sumw(t,j)
                end do
		    end do

            !Predict the Means and Covariances
            do m = 1, nemf
                W(t+1,m,1) = W(t,m,nobs(t)+1)
                aup = a(t,m,nobs(t)+1,:) 
                bup = P(t,m,nobs(t)+1,:,:) 
                !-------------------------------------------------------------------
                !Prediction according to the Unscented Kalman Filter
                call UT(nfac,nsigma,aup,kappa,bup,asigma,wsigma)
                a(t+1,m,1,:) = 0.0d0
                do k1 = 1, nfac
                    do k2 = 1, nsigma
                        a(t+1,m,1,k1) = a(t+1,m,1,k1) + wsigma(k2)*transition(s,k1,nfac,asigma(k2,:))
                    end do
                end do
                P(t+1,m,1,:,:) = Q(s,:,:)
                diffvec = 0.0d0
                diffout = 0.0d0
                do j = 1, nsigma
                    do k1 = 1, nfac
                        aux = transition(s,k1,nfac,asigma(j,:))
                        diffvec(j,k1) = aux - a(t+1,m,1,k1)
                    end do
                    do k1 = 1, nfac
                        do k2 = 1, nfac
                            diffout(j,k1,k2) = diffvec(j,k1)*diffvec(j,k2)
                        end do
                    end do
                    P(t+1,m,1,:,:) = P(t+1,m,1,:,:) + wsigma(j)*diffout(j,:,:)
                end do
                !------------------------------------------------------------------- 
            end do
        end if    
	end do
    
	t = ntime
	if (nobs(t) > 0) then
	    s = stage(t)
        do j = 1, nobs(t)
            temp = 0.0d0
            if (nonlinear(t,indeq(t,j))==0) then
                do m = 1, nemf
                    MY(t,j,m) = dot_product(beta(t,indeq(t,j),:),X(i,t,:)) + dot_product(Z(t,indeq(t,j),:),a(t,m,j,:))
                    FY(t,j,m) = dot_product(Z(t,indeq(t,j),:),matmul(P(t,m,j,:,:),Z(t,indeq(t,j),:))) + H(t,indeq(t,j),indeq(t,j))
                    WY(t,j,m) = W(t,m,j)
                    prob(t,j,m) = NORMPDF(Y(i,t,indeq(t,j)),MY(t,j,m),FY(t,j,m))
                    temp = temp + WY(t,j,m)*prob(t,j,m)
                    sumw(t,j) = sumw(t,j) + W(t,m,j)*prob(t,j,m)
                    KF(t,m,j,:) = matmul(P(t,m,j,:,:),Z(t,indeq(t,j),:))
		            v(t,m,j) = Y(i,t,indeq(t,j)) - MY(t,j,m)
		            a(t,m,j+1,:)   = a(t,m,j,:) + KF(t,m,j,:)*v(t,m,j)/FY(t,j,m)
		            P(t,m,j+1,:,:) = P(t,m,j,:,:) - outerprod(KF(t,m,j,:),KF(t,m,j,:))/FY(t,j,m)
                end do
            else if (nonlinear(t,indeq(t,j))==1) then
                do m = 1, nemf
                    call UT(nfac,nsigma,a(t,m,j,:),kappa,P(t,m,j,:,:),asigma,wsigma)
                    if (typefunct(t,indeq(t,j))==2) then
                        MY(t,j,m) = 0.0d0
                        FY(t,j,m) = H(t,indeq(t,j),indeq(t,j))
                        do k1 = 1, nsigma
                            xhat(k1) = dot_product(beta(t,indeq(t,j),:),X(i,t,:)) + dot_product(Z(t,indeq(t,j),:),asigma(k1,:))                     
                            MY(t,j,m) = MY(t,j,m) + wsigma(k1)*fnl(xhat(k1))
                        end do
                        do k1 = 1, nsigma
                            FY(t,j,m) = FY(t,j,m) + wsigma(k1)*(fnl(xhat(k1))-MY(t,j,m))*(fnl(xhat(k1))-MY(t,j,m))       
                        end do
                        WY(t,j,m) = W(t,m,j)
                        prob(t,j,m) = NORMPDF(Y(i,t,indeq(t,j)),MY(t,j,m),FY(t,j,m))
                        temp = temp + WY(t,j,m)*prob(t,j,m)
                        sumw(t,j) = sumw(t,j) + W(t,m,j)*prob(t,j,m)
                        KF(t,m,j,:) = 0.0d0
                        !Update mean vector and variance-covariance matrix
                        KF(t,m,j,:) = 0.0d0
                        do k1 = 1, nsigma
                            xhat(k1) = dot_product(beta(t,indeq(t,j),:),X(i,t,:)) + dot_product(Z(t,indeq(t,j),:),asigma(k1,:))
                            KF(t,m,j,:) = KF(t,m,j,:) + wsigma(k1)*(fnl(xhat(k1))-MY(t,j,m))*(asigma(k1,:)-a(t,m,j,:))  
                        end do
                        v(t,m,j) = Y(i,t,indeq(t,j)) - MY(t,j,m)
                        a(t,m,j+1,:) = a(t,m,j,:) + KF(t,m,j,:)*v(t,m,j)/FY(t,j,m) 
                        P(t,m,j+1,:,:) = P(t,m,j,:,:) - outerprod(KF(t,m,j,:),KF(t,m,j,:))/FY(t,j,m)
                   end if                            
                end do
            end if
            !Compute Likelihood
            !Update Weights
            do m = 1, nemf
                W(t,m,j+1) = W(t,m,j)*prob(t,j,m)/sumw(t,j)
            end do
	    end do    
    end if      		

    t = ntime
    do j = 1, nanch
        if (dYANCH(i,j) == 0) then
            aANCH(:,j+1,:)   = aANCH(:,j,:)
            PANCH(:,j+1,:,:) = PANCH(:,j,:,:)
            WANCH(j+1,:)     = WANCH(j,:)
        else if (dYANCH(i,j) == 1) then
            temp = 0.0d0
            if (PROBANCH(j) == 0) then
                do m = 1, nemf
                    muanch(j,m)  = dot_product(delta(j,:),X(i,t,:)) + dot_product(alpha(j,:),aanch(m,j,:))
                    varanch(j,m) = dot_product(alpha(j,:),matmul(PANCH(m,j,:,:),alpha(j,:))) + HANCH(j)
                    wanch(j,m) = WANCH(1,m)
                    pranch(j,m) = NORMPDF(YANCH(i,j),muanch(j,m),varanch(j,m))
                    temp = temp + wanch(j,m)*pranch(j,m)
                    sumanch(j) = sumanch(j) + wanch(j,m)*pranch(j,m)
                    !Update in the linear anchoring equation
                    KFANCH(m,j,:) = matmul(PANCH(m,j,:,:),alpha(j,:))
		            vanch(m,j) = YANCH(i,j) - muanch(j,m)
		            aanch(m,j+1,:)  = aanch(m,j,:) + KFANCH(m,j,:)*vanch(m,j)/varanch(j,m)
		            PANCH(m,j+1,:,:) = PANCH(m,j,:,:) - outerprod(KFANCH(m,j,:),KFANCH(m,j,:))/varanch(j,m)
                end do
            else if (PROBANCH(j) == 1) then
                do m = 1, nemf
                    muanch(j,m)  = dot_product(delta(j,:),X(i,t,:)) + dot_product(alpha(j,:),aanch(m,j,:))
                    varanch(j,m) = dot_product(alpha(j,:),matmul(PANCH(m,j,:,:),alpha(j,:))) + HANCH(j)
                    wanch(j,m) = WANCH(1,m)
                    aux = muanch(j,m)/dsqrt(varanch(j,m))
                    if (YANCH(i,j) == 1.0d0) then
                        pranch(j,m) = NORMCDF(aux,0.0d0,1.0d0)
                    else if (YANCH(i,j) == 0.0d0) then
                        pranch(j,m) = 1.0d0-NORMCDF(aux,0.0d0,1.0d0)
                    end if
                    temp = temp + wanch(j,m)*pranch(j,m)
                    sumanch(j) = sumanch(j) + wanch(j,m)*pranch(j,m)
                    !Update in the nonlinear anchoring equation
                    muanch(j,m) = NORMCDF(aux,0.0d0,1.0d0)
                    call UT(nfac,nsigma,aanch(m,j,:),kappa,PANCH(m,j,:,:),asigma,wsigma)
                    KFANCH(m,j,:) = 0.0d0
                    do k1 = 1, nsigma
                        xhat(k1) = dot_product(delta(j,:),X(i,t,:)) + dot_product(alpha(j,:),asigma(k1,:))
                        YSTAR = NORMCDF(xhat(k1)/dsqrt(varanch(j,m)),0.0d0,1.0d0)
                        KF(t,m,j,:) = KF(t,m,j,:) + wsigma(k1)*(YSTAR-muanch(j,m))*(asigma(k1,:)-aanch(m,j,:))  
                    end do
		            vanch(m,j) = YANCH(i,j) - muanch(j,m)
		            aanch(m,j+1,:)  = aanch(m,j,:) + KFANCH(m,j,:)*vanch(m,j)/varanch(j,m)
		            PANCH(m,j+1,:,:) = PANCH(m,j,:,:) - outerprod(KFANCH(m,j,:),KFANCH(m,j,:))/varanch(j,m)
                 end do
            end if
            do m = 1, nemf
                WANCH(j+1,m) = WANCH(j,m)*pranch(j,m)/sumanch(j)
            end do
        end if
    end do    

    do t = 1, ntime
        afilter(t,:) = 0.0d0
        do m = 1, nemf
            afilter(t,:) = afilter(t,:) + W(t,m,nobs(t)+1)*a(t,m,nobs(t)+1,:)
        end do
    end do
	
end subroutine get_filter


end module filtering
