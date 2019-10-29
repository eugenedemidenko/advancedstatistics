corn0 <-
function(ro=.7,alpha=.05,nSim=100000)
{
	dump("corn0","c:\\StatBook\\corn0.r")
	par(mfrow=c(1,2),mar=c(3.5,3.5,2,1),cex.main=1.5)
	ro.alt=seq(from=0,to=.99,length=50);nro.alt=length(ro.alt)
	
	powZ=powt=rep(NA,nro.alt)
	Z1a=qnorm(1-alpha/2)
	for(n in c(10,30))
	{
		t1a=qt(1-alpha/2,n-2)
		Zro.dif=.5*(log(1+ro.alt)-log(1-ro.alt))-.5*(log(1+ro)-log(1-ro))
		powZT=pnorm(Zro.dif*sqrt(n-3)+Z1a,lower.tail=F)+pnorm(Zro.dif*sqrt(n-3)-Z1a)		
		for(ir in 1:nro.alt)
		{
			X=matrix(rnorm(nSim*n),ncol=n)
			X2=rowSums(X^2);xm=rowMeans(X)
			Z=matrix(rnorm(nSim*n),ncol=n)
			Y=ro.alt[ir]*X+Z*sqrt(1-ro.alt[ir]^2)
			Y2=rowSums(Y^2);XY=rowSums(X*Y)
			ym=rowMeans(Y)
			r=(XY-n*xm*ym)/sqrt((X2-n*xm^2)*(Y2-n*ym^2))		
			Zr=.5*(log(1+r)-log(1-r))-.5*(log(1+ro)-log(1-ro))
			powZ[ir]=mean(abs(Zr)*sqrt(n-3)>Z1a)
		}
		matplot(ro.alt,cbind(powZ,powZT),col=1,lwd=2,ylim=c(0,1),type="l",xlab="",ylab="")
		title(paste("n =",n))
		mtext(side=1,"Alternative correlation coefficient",line=2.5,cex=1.3)
		mtext(side=2,"Power",line=2.25,cex=1.3)
		segments(-1,alpha,1,alpha,col="gray80")
		segments(ro,-1,ro,1,col="gray80")
		legend(0,.94,c("Simulations","FZ approximation"),col=1,lty=1:2,lwd=2,cex=1.25,bg="gray90")
	}
}
