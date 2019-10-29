ci.binpr <-
function(n=30,lambda=.95,nSim=1000000)
{
dump("ci.binpr","c:\\StatBook\\ci.binpr.r")
par(mfrow=c(1,1),cex.main=2,cex.lab=1.5)
alpha=1-lambda
Z1a=qnorm(1-alpha/2)
p.true=seq(from=0.1,to=.9,by=.01);N=length(p.true)
covpr.W=covpr.EW=rep(NA,nrow=N)
for(i in 1:N)
{
	p.hat=rbinom(n=nSim,size=n,prob=p.true[i])/n
	covpr.W[i]=mean(abs(p.hat-p.true[i])<Z1a*sqrt(p.hat[i]*(1-p.hat[i])/n))	
	D=Z1a-4*p.hat^2*n+4*p.hat*n
	p.low=p.hat-0.5*Z1a/(Z1a^2+n)*(2*p.hat*Z1a-Z1a+sqrt(D))
	p.up=p.hat-0.5*Z1a/(Z1a^2+n)*(2*p.hat*Z1a-Z1a-sqrt(D))
	covpr.EW[i]=mean(p.true[i]<p.up & p.true[i]>p.low)
}
matplot(p.true,cbind(covpr.W,covpr.EW),type="l",lty=1,lwd=2,col=1:2,xlab="True probability",ylab="Coverage probability",main=paste("n = ",n,", nSim = ",nSim,sep=""))			
segments(-1,lambda,1,lambda)
legend("bottomleft",c("Wald","Estimated Wald"),col=1:2,lwd=3,lty=1,cex=1.5,bg="gray96")

}
