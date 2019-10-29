multCLT <-
function(nSim=10000,N=50)
{
dump("multCLT","c:\\StatBook\\multCLT.r")
par(mfrow=c(1,2),mar=c(4.5,4.5,3,1),cex.lab=1.5)
cdf=cdfIND=matrix(0,ncol=N,nrow=N)
for(n in c(5,30))
{
	X=matrix(runif(n*nSim),nrow=nSim)
	mu.true=.5;var.true=(n-1)/12/n
	xmean=rowMeans(X)
	x2sum=rowSums(X^2)
	xs2=(x2sum-xmean^2*n)/n
	U2=sqrt(n)*cbind((xmean-mu.true)*sqrt(12),(xs2-var.true)*12)
	x1=x2=seq(from=-.5,to=1.5,length=N)
	for(i1 in 1:N)
	for(i2 in 1:N)
	{
		cdf[i1,i2]=mean(U2[,1]<=x1[i1] & U2[,2]<=x2[i2])
		cdfIND[i1,i2]=pnorm(x1[i1])*pnorm(x2[i2])
	}
	contour(x1,x2,cdf,levels=c(.3,.6),xlab="1st component, mean",ylab="2nd component, variance")
	title(paste("n =",n))
	contour(x1,x2,cdfIND,levels=c(.3,.6),ad=T,col=2,lty=2)
	legend(0,1.5,c("Empirical cdf","Theoretical normal cdf"),lty=1:2,col=1:2,cex=1.25,bg="gray95")
}

}
