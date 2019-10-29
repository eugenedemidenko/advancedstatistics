meanC <-
function(nSim=100000)
{
dump("meanC","c:\\StatBook\\meanC.r")
par(mfrow=c(1,2),mar=c(4,4,3,1))
for(n in c(2,10))
{
	X=matrix(runif(nSim*n),ncol=n,nrow=nSim)
	X=tan(pi*(X-.5))
	Xav=rowMeans(X)
	Xav=Xav[order(Xav)]
	Fv=(1:nSim)/nSim
	plot(Xav,Fv,type="s",xlim=c(-10,10),xlab="",ylab="Probability")
	mtext(side=1,paste(nSim,"simulated values/reduced range"),line=2.5,cex=1.25)
	title(paste("cdf of the average of Cauchy distribution, n =",n))
	xx=seq(from=-10,to=10,length=100)
	lines(xx,atan(xx)/pi+.5,col=2)
	legend(-10,1,c("Empirical cdf","Theoretical cdf"),col=c(1,2),lty=1,cex=1.5)
}

}
