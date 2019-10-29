cor0 <-
function(alpha=.05,nSim=100000)
{
	dump("cor0","c:\\StatBook\\cor0.r")
	par(mfrow=c(1,2),mar=c(3.5,3.5,2,1),cex.main=1.5)
	ro=seq(from=-.7,to=.7,length=50);nro=length(ro)
	Zro=.5*(log(1+ro)-log(1-ro))
	powZ=powt=rep(NA,nro)
	Z1a=qnorm(1-alpha/2)
	for(n in c(10,30))
	{
		t1a=qt(1-alpha/2,n-2)
		powZT=pnorm(Zro*sqrt(n-3)+Z1a,lower.tail=F)+pnorm(Zro*sqrt(n-3)-Z1a)
		nuro=ro*sqrt(n-2)/sqrt(1-ro^2)
		powtT=pt(t1a,df=n-2,ncp=nuro,lower.tail=F)+pt(-t1a,df=n-2,ncp=nuro)
		for(ir in 1:nro)
		{
			X=matrix(rnorm(nSim*n),ncol=n)
			X2=rowSums(X^2);xm=rowMeans(X)
			Z=matrix(rnorm(nSim*n),ncol=n)
			Y=ro[ir]*X+Z*sqrt(1-ro[ir]^2)
			Y2=rowSums(Y^2);XY=rowSums(X*Y)
			ym=rowMeans(Y)
			r=(XY-n*xm*ym)/sqrt((X2-n*xm^2)*(Y2-n*ym^2))		
			Zr=.5*(log(1+r)-log(1-r))
			powZ[ir]=mean(abs(Zr)*sqrt(n-3)>Z1a)
		}
		matplot(ro,cbind(powZ,powZT),col=1,lwd=2,ylim=c(0,1),type="l",xlab="",ylab="")
		title(paste("n =",n))
		mtext(side=1,"Alternative correlation coefficient",line=2.5,cex=1.3)
		mtext(side=2,"Power",line=2.25,cex=1.3)
		segments(-1,alpha,1,alpha,col="gray80")
		segments(0,-1,0,1,col="gray80")
		legend(-.35,1,c("t-test/simulation","FZ-test"),col=1,lty=1:2,lwd=2,cex=1.25,bg="gray90")
	}
}
