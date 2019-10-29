olsnormT <-
function(job=1,alpha=-1,beta=1,sigma=.5,muX=2,sigmaX=1,p=.3,nSim=100000)
{
dump("olsnormT","c:\\StatBook\\olsnormT.r")
par(mfrow=c(1,2),mar=c(3.5,3.5,3,1))
for(n in c(10,30))
{
	if(job==1)
	{
		X=matrix(rnorm(n*nSim),ncol=n)
		ti=paste("X normally distributed, n=",n,sep="")	
	}
	if(job==2)
	{
		X=matrix(sample(x=c(1,0),size=nSim*n,rep=T,prob=c(p,1-p)),ncol=n)
		ti=paste("X Bernoulli distributed, n = ",n,", p = ",p,sep="")		
	}
	eps=matrix(rnorm(n*nSim,sd=sigma),ncol=n)
	Y=alpha+beta*X+eps
	r2x=rowSums(X^2);rx=rowMeans(X)
	rxy=rowSums(X*Y);ry=rowMeans(Y)
	den=r2x-rx^2*n
	num=rxy-rx*ry*n
	beta.OLS=num/den;alpha.OLS=ry-beta.OLS*rx
	sigma2.hat=rowSums((Y-alpha.OLS-beta.OLS*X)^2)/(n-2)
	tstat=(beta.OLS-beta)/sqrt(sigma2.hat/den)
	tstat=tstat[order(tstat)]
	nb=length(tstat)	
	plot(qt((1:nb-.5)/nb,df=n-2),tstat,pch=16,xlim=c(-8,8),ylim=c(-8,8),xlab="",ylab="")
	title(ti)
	mtext(side=1,"Theoretical t-quantiles",cex=1.5,line=2.5)
	mtext(side=2,"Empirical quantiles",cex=1.5,line=2.25)
	segments(-8,-8,8,8,col=2)
}	
}
