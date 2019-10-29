civar <-
function (job=1,n=10,alpha=0.05,eps=1e-08,maxiter=10,nExp=1e+06) 
{
dump("civar", "c:\\StatBook\\civar.r")
if(job==1) # shotest CI for normal variance
{
    n0=4;n1=15;n=n0:n1
    par(mfrow = c(1,2),mar=c(4,4,1,2))
	widEQT=1/qchisq(alpha/2,df=n-1)-1/qchisq(1-alpha/2,df=n-1)
    widSHORT=rep(NA,n1-n0+1)
    dfm = matrix(ncol=2,nrow=2)
    for (n in n0:n1)
	{
        Lk = qchisq(alpha/2, df = n - 1); Uk = qchisq(1 - alpha/2, df = n - 1)
        for (k in 1:maxiter) 
		{
            e2 = pchisq(Uk, df = n - 1) - pchisq(Lk, df = n - 1) - 1 + alpha
            e1 = (n + 1) * log(Uk/Lk) - (Uk - Lk)
            dfm[1, 1] = (n + 1)/Uk - 1;  dfm[1, 2] = -(n + 1)/Lk + 1
            dfm[2, 1] = dchisq(Uk, df = n - 1);  dfm[2, 2] = -dchisq(Lk, df = n - 1)
            UL = c(Uk, Lk) - solve(dfm) %*% c(e1, e2)
			print(c(k,UL,e1,e2))
            if (abs(UL[1] - Uk) + abs(UL[2] - Lk) < eps) break
			Uk=UL[1];Lk=UL[2]
        }
        widSHORT[n-n0+1]=1/Lk-1/Uk
    }
    matplot(n0:n1, cbind(widEQT,widSHORT),type = "l",col=1,lty=1,main="",ylab="",xlab = "")
	points(n0:n1,widEQT,cex=1.25);	points(n0:n1,widSHORT,cex=1.25,pch=16)
	legend("topright", c("Equal-tail CI", "Shortest CI"), lty=1,pch=c(1, 16),cex=1.5, bg="gray90")
    mtext(side=1,"Sample size, n",cex=1.5,line=2.5)
	mtext(side=2,"Relative width of CI for the normal variance",cex=1.5,line=2.5)        
    
		
    matplot(n0:n1,(widEQT-widSHORT)/widEQT*100, type = "l",col=1,lwd=3,main = "", xlab = "", ylab = "")
	mtext(side=1,"Sample size, n",cex=1.5,line=2.5)
    mtext(side=2,"% CI width reduction", cex = 1.5,line=2.5)
    
}
if(job==2) # shotest CI for normal SD
{
    n0=4;n1=15;n=n0:n1
    par(mfrow = c(1,2),mar=c(4,4,1,1))
	widEQT=1/sqrt(qchisq(alpha/2,df=n-1))-1/sqrt(qchisq(1-alpha/2,df=n-1))
    widSHORT=rep(NA,n1-n0+1)
    dfm = matrix(ncol=2,nrow=2)
    for (n in n0:n1)
	{
        Lk = qchisq(alpha/2, df = n - 1); Uk = qchisq(1 - alpha/2, df = n - 1)
        for (k in 1:maxiter) 
		{
            e2 = pchisq(Uk, df = n - 1) - pchisq(Lk, df = n - 1) - 1 + alpha
            e1 = (n) * log(Uk/Lk) - (Uk - Lk)
            dfm[1, 1] = (n)/Uk - 1;  dfm[1, 2] = -(n)/Lk + 1
            dfm[2, 1] = dchisq(Uk, df = n - 1);  dfm[2, 2] = -dchisq(Lk, df = n - 1)
            UL = c(Uk, Lk) - solve(dfm) %*% c(e1, e2)
			print(c(k,UL,e1,e2))
            if (abs(UL[1] - Uk) + abs(UL[2] - Lk) < eps) break
			Uk=UL[1];Lk=UL[2]
        }
        widSHORT[n-n0+1]=1/sqrt(Lk)-1/sqrt(Uk)
    }
    matplot(n0:n1, cbind(widEQT,widSHORT),type = "l",col=1,lty=1,main="",ylab="",xlab = "")
	points(n0:n1,widEQT,cex=1.25);	points(n0:n1,widSHORT,cex=1.25,pch=16)
	legend("topright", c("Equal-tail CI", "Shortest CI"), lty=1,pch=c(1, 16),cex=1.5, bg="gray90")
    mtext(side=1,"Sample size, n",cex=1.5,line=2.5)
	mtext(side=2,"CI width for the normal SD",cex=1.5,line=2.5)        
    
		
    matplot(n0:n1,(widEQT-widSHORT)/widEQT*100, type = "l",col=1,lwd=3,main = "", xlab = "", ylab = "")
	mtext(side=1,"Sample size, n",cex=1.5,line=2.5)
    mtext(side=2,"% CI width reduction", cex = 1.5,line=2.5)
    
}    
}
