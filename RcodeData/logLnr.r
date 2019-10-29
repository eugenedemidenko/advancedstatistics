logLnr <-
function(p=0.5,mu=4,sigma=1)
{
dump("logLnr","c:\\StatBook\\logLnr.r")

ptNR=function(mu,sigma,p)
{
	l=mu+sigma*qnorm((1-p)/2);L=mu+sigma*qnorm((1+p)/2)
	M=matrix(NA,ncol=2,nrow=2)
	for(i in 1:10)
	{
		E1=(l-mu)^2/2/sigma^2+l-(L-mu)^2/2/sigma^2-L
		E2=pnorm((L-mu)/sigma)-pnorm((l-mu)/sigma)-p
		M[1,1]=(l-mu)/sigma^2+1;M[1,2]=-(L-mu)/sigma^2-1
		M[2,1]=-dnorm((l-mu)/sigma)/sigma;M[2,2]=dnorm((L-mu)/sigma)/sigma
		delta=solve(M)%*%c(E1,E2)
		lL=c(l,L)-delta
		if(max(abs(delta))<0.0001) break	
		l=lL[1];L=lL[2]
	}
	return(exp(c(l,L)))
}

par(mfrow=c(1,2),mar=c(4.5,4.5,3,1))
mus=c(1,2);sigma=.5
ps=seq(from=.5,to=.95,by=.01)
nps=length(ps)
tT=matrix(ncol=2,nrow=nps)
for(imu in 1:2)
{
	for(i in 1:nps)
	  tT[i,]=ptNR(mu=mus[imu],sigma=sigma,p=ps[i])
	matplot(ps,tT,type="l",lty=1,col=1,lwd=2,xlab="p",ylab="Tight limits")
	em=bquote(mu == .(mus[imu]))
	mtext(side=3,em,cex=2,line=.5)
}

}
