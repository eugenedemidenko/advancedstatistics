dvalREG <-
function(job=1,n=5,delta=1,alpha=1,beta=1,x=1:n,xp=4,sigma=2,nSim=50000)
{
dump("dvalREG","c:\\StatBook\\dvalREG.r")
if(job==1)
{
	X=matrix(rnorm(n*nSim,mean=0,sd=sigma),ncol=n)
	aX=rowMeans(X);s2X=apply(X,1,var)*(n-1)
	Y=matrix(rnorm(n*nSim,mean=delta,sd=sigma),ncol=n)
	aY=rowMeans(Y);s2Y=apply(Y,1,var)*(n-1)
	sig.hat=(s2X+s2Y)/(2*n-2)
	dfiN=pnorm((aX-aY)/sqrt(2)/sig.hat)
	dfiT=pt((aX-aY)/sqrt(2)/sig.hat,df=2*n-2)
	dfi=pnorm(-delta/sqrt(2)/sigma)
	MSE1=mean((dfiN-dfi)^2)
	MSE2=mean((dfiT-dfi)^2)
	print(c(MSE1,MSE2))
}
if(job==2)
{
	mx=mean(x);SUMx=var(x)*(n-1)
	Y1=Y2=dv1=dv2=rep(0,nSim)
	n1=rep(1,n)
	for(isim in 1:nSim)
	{
		Y=alpha+beta*x+rnorm(n,sd=sigma)
		b.est=cov(x,Y)*(n-1)/SUMx
		a.est=mean(Y)-b.est*mx
		res=Y-a.est-b.est*x
		s2.est=sum(res^2)/(n-2)
		Y2[isim]=a.est+b.est*xp+rnorm(1,sd=sigma)
		Y1[isim]=a.est+b.est*(xp+1)+rnorm(1,sd=sigma)
		dv1[isim]=dnorm(-b.est/sqrt(2*s2.est+s2.est/SUMx))
		dv2[isim]=pt(-b.est/sqrt(2*s2.est+s2.est/SUMx),df=n-2)	
	}
	dvT=pnorm(-beta/sigma/sqrt(2+1/SUMx))
	print(c(mean(Y1<Y2),dvT,mean(dv1),mean(dv2)))
}
if(job==3)
{
	beta2=-.2
	x2=x^2
	lfs=tsd=nsd=rep(0,nSim)
	par(mfrow=c(2,2))
	for(isim in 1:nSim)
	{
		Y=alpha+beta*x+beta2*x2+rnorm(n,sd=sigma)
		if(isim<4) plot(x,Y)
		so=summary(lm(Y~x+x2))
		ta=so$coefficients;s.hat=so$sigma
		b.hat=ta[2,1];seb.hat=ta[2,2]
		lfs[isim]=b.hat+sum(rnorm(n=2,sd=sigma))
		tsd[isim]=pt(-b.hat/sqrt(2*s.hat^2+seb.hat^2),df=n-3)
		nsd[isim]=pnorm(-b.hat/sqrt(2*s.hat^2+seb.hat^2))		
	}
	print(c(mean(lfs<0),mean(tsd),mean(nsd)))


}

}
