cdfMED <-
function(n=5,mu=1,nSim=10000,lambda=.1)
{
dump("cdfMED","c:\\StatBook\\cdfMED.r")
par(mfrow=c(1,1),mar=c(4,4,3,1))
med=rep(NA,nSim)
for(i in 1:nSim)
{
	x=rexp(n,rate=lambda)
	sn=sign(runif(n,min=-1,max=1))
	x=x*sn+mu
	med[i]=median(x)
}
med=med[order(med)]
Fv=(1:nSim)/nSim
plot(med,Fv,type="l",xlab=paste(nSim,"medians"),ylab="cdf, probability",xlim=c(-20,20))
title(paste("Cdfs of the median of Laplace distribution\nlambda=",lambda,", mu=",mu,", n=",n,sep=""))
x1=seq(from=med[1],to=mu,length=100)
x2=seq(from=mu,to=med[nSim],length=100)
Fx=c(.5*exp(-lambda*(mu-x1)),1-.5*exp(-lambda*(x2-mu)))
r=ceiling(n/2)
Fmed=pbeta(Fx,r,n-r+1)
lines(c(x1,x2),Fmed,lty=2)
legend(-20,1,c("Empirical cdf","Theoretical cdf, beta"),lty=1:2)
}
