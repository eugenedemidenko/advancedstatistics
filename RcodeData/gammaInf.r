gammaInf <-
function(job=1,nSim=50000)
{
dump("gammaInf","c:\\StatBook\\gammaInf.r")
if(job==1)
{
	par(mfrow=c(1,2),mar=c(4,4,3,1))
	alpha=seq(from=.2,to=2,by=.2);n.alpha=length(alpha)
	lambda=c(.5,1.5);n.lambda=length(lambda)
	ns=c(10,20);nns=length(ns)
	ab=as.data.frame(matrix(ncol=2,nrow=nns))
	names(ab)=c("Intercept","Slope")
	row.names(ab)=c("n=10","n=20")
	bias.a=matrix(nrow=n.alpha,ncol=nns)	
	for(il in 1:n.lambda)
	{
		for(i in 1:nns)
		{
			for(j in 1:n.alpha)
			{
				X=matrix(rgamma(nSim*ns[i],shape=alpha[j],rate=lambda[il]),ncol=ns[i])
				Xb=apply(X,1,mean)
				s2=apply(X,1,var)
				aMM=Xb^2/s2
				bias.a[j,i]=mean(aMM)-alpha[j]	
			}
			ab[i,]=coef(lm(bias.a[,i]~alpha))	
		}
		matplot(alpha,bias.a,type="b",lwd=3,lty=1:nns,col=1,xlim=c(0,2),ylim=c(0,.7),xlab="",ylab="Bias")	
		for(i in 1:nns) lines(alpha,ab[i,1]+ab[i,2]*alpha,col=1+i)
		mtext(side=3,paste("l =",lambda[il]),font=5,cex=2,line=.25)
		mtext(side=1,"a",font=5,cex=2,line=2.5)
		legend(0,.7,paste(1:nns,": n = ",ns,sep=""),lty=1:nns,lwd=3,cex=1.5)
		print(paste("lambda =",lambda[il]))
		print(ab)
		
	}
}
if(job==2)
{
	par(mfrow=c(1,2),mar=c(4,4,3,1))
	ab=matrix(c(.13,.29,.085,.126),ncol=2,nrow=2,byrow=T)
	alpha=seq(from=.2,to=2,by=.2);n.alpha=length(alpha)
	lambda=1;n.lambda=length(lambda)
	ns=c(10,20);nns=length(ns)
	cv.CR=matrix(NA,nrow=n.alpha,ncol=2)	
	for(i in 1:nns)
	{
		for(j in 1:n.alpha)
		{
			X=matrix(rgamma(nSim*ns[i],shape=alpha[j],rate=lambda),ncol=ns[i])
			Xb=apply(X,1,mean)
			s2=apply(X,1,var)
			aMM=Xb^2/s2
			abMM=(aMM-ab[i,1])/(1+ab[i,2])
			cv.CR[j,1]=sd(abMM)/alpha[j]
		}
		cv.CR[,2]=1/sqrt(ns[i]*alpha*(alpha*trigamma(alpha)-1))			
		matplot(alpha,cv.CR,type="l",lwd=3,lty=1:2,col=1,xlim=c(0,2),xlab="",ylab="Coefficient of variation")	
		mtext(paste("n =",ns[i]),cex=2,line=1)
		mtext(side=1,"a",font=5,cex=2,line=2.5)		
	}
	
}	
if(job==3)
{
	alpha=seq(from=1,to=2,by=.01)
	n=10
	par(mfrow=c(1,2))
	plot(alpha,lgamma(alpha),type="l")
	plot(alpha,psigamma(alpha,der=1),type="l")
	return()
	cv.CR=1/sqrt(n*alpha*(alpha*psigamma(alpha,der=1)-1))
	cv.CR=1/sqrt(n*alpha*(alpha*trigamma(alpha)-1))
	plot(alpha,psigamma(alpha),type="l")
	plot(alpha,psigamma(alpha,der=2),type="l")
	#plot(alpha,cv.CR,type="l")
}
}
