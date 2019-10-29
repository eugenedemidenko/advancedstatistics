robloc <-
function(cc=.5,theta=1,sigma=1,sigmaN=10,delta=.1,nSim=100000,eps=.001,maxit=100)
{
dump("robloc","c:\\StatBook\\robloc.r")
HubEstF=function(Y,start.est,maxit,eps)
{
	hub.est=start.est
	n=ncol(Y)
	un=rep(1,n)
	for(it in 1:maxit)
	{
		tun=hub.est%*%t(un)
		ind=abs(Y-tun)<cc	
		w=ind+(1-ind)/abs(Y-tun)
		hub.est.new=((w*Y)%*%un)/(w%*%un)
		if(mean(abs(hub.est.new-hub.est))<eps) break
		hub.est=hub.est.new
		print(it)
	}
	return(hub.est.new)
}
par(mfrow=c(1,2),mar=c(4,4,3,1))
for(n in c(50,200))
{
	n1=round(n*(1-delta))
	n2=n-n1
	Y1=matrix(rnorm(n1*nSim,mean=theta,sd=sigma),nrow=nSim,ncol=n1)
	Y2=matrix(rnorm(n2*nSim,mean=theta,sd=sigmaN),nrow=nSim,,ncol=n2)
	Y=cbind(Y1,Y2)
	mea=rowMeans(Y)
	med=apply(Y,MARGIN=1,FUN=median)
	meaD=density(mea)
	medD=density(med)
	hub=HubEstF(Y,start.est=med,maxit,eps)
	hubD=density(hub)
	XX=cbind(meaD$x,medD$x,hubD$x);YY=cbind(meaD$y,medD$y,hubD$y)
	matplot(XX,YY,col=1,type="l",lwd=2,xlab="",ylab="")
	mtext(side=1,"Estimate of the center",line=2.75,cex=1.5)
	mtext(side=2,"Density",line=2.75,cex=1.5)
	title(paste("n1=",n1,", n2=",n2,", n=",n,sep=""))
	legend(min(XX),max(YY),c("Mean","Median",paste("Huber (c=",cc,")",sep="")),lty=1:3,lwd=2,bg=gray(.9))
	legend(.5*min(XX)+.6*max(XX),max(YY),c(paste("SD mean=",round(sd(mea),2)),paste("SD median=",round(sd(med),2)),paste("SD Huber=",round(sd(hub),2))),bg=gray(.9))
}

}
