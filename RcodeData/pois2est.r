pois2est <-
function(Nsim=100000)
{
dump("pois2est","c:\\StatBook\\pois2est.r")
par(mfrow=c(1,2),mar=c(4,4,3,1))
lambda=seq(from=.1,to=1.5,by=.1)
LL=length(lambda)
MSE.MM=MSE.VM=rep(0,LL)
for(n in c(3,5))
{
	for(i in 1:LL)
	{
		X=matrix(rpois(Nsim*n,lambda[i]),ncol=n)
		mm.mean=rowMeans(X)
		mm.var=(rowMeans(X^2)-mm.mean^2)*n/(n-1)
		MSE.MM[i]=mean((mm.mean-lambda[i])^2)
		MSE.VM[i]=mean((mm.var-lambda[i])^2)
	}
	RelMSE.MM=sqrt(MSE.MM)/lambda
	RelMSE.VM=sqrt(MSE.VM)/lambda
	matplot(lambda,cbind(RelMSE.MM,RelMSE.VM),type="l",col=1,main=paste("Sample size, n =",n),xlab="",ylab="Relative RMSE",lwd=2)
	mtext(side=1,expression(paste(plain(True),"  ", lambda)),cex=1.5,line=3)
	if(n==5) legend(.5,1.5,c("Mean MM","Variance VM"),lty=1:2,lwd=2,cex=1.25,bg=gray(.96))
}
}
