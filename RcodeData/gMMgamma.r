gMMgamma <-
function(nSim=5000)
{
dump("gMMgamma","c:\\StatBook\\gMMgamma.r")
gMM.gamma=function(aMM,lMM,LM,maxIt=100,eps=.001)
{
	H=matrix(ncol=2,nrow=2)
	for(it in 1:maxIt)
	{
		g=aMM*log(lMM)-(aMM+k)*log(lMM+theta)+lgamma(aMM+k)-lgamma(aMM)-LM
		H[,1]=log(lMM)-log(lMM+theta)+digamma(aMM+k)-digamma(aMM)
		H[,2]=aMM/lMM-(aMM+k)/(lMM+theta)
		H[1,1]=H[1,1]+1;H[2,2]=H[2,2]+1
		delta=solve(H)%*%g
		if(max(abs(delta))<eps) break
		aMM=aMM-delta[1];lMM=lMM-delta[2]
		if(it==maxIt) {aMM=lMM=NA}
	}
	return(c(aMM,lMM))
}

alpha=2;lambda=.6
k=c(1,2);theta=c(.1,.5)
n=seq(from=10,to=50,by=5);LN=length(n)
mest=matrix(ncol=4,nrow=LN)
LM=rep(0,2)
simal=matrix(nrow=nSim,ncol=4)
for(inn in 1:LN)
{
	for(iexp in 1:nSim)
	{
		X=rgamma(n=n[inn],shape=alpha,rate=lambda)
		lMM=mean(X)/var(X);aMM=mean(X)^2/var(X)
		simal[iexp,1:2]=c(aMM,lMM)
		for(i in 1:2)
			LM[i]=log(mean(X^k[i]*exp(-theta[i]*X)))
		simal[iexp,3:4]=gMM.gamma(aMM,lMM,LM)
	}
	mest[inn,]=colMeans(simal,na.rm=T)
}
par(mfrow=c(1,2))
what=c("Shape","Rate")
for(ig in 1:2)
{
	matplot(n,cbind(mest[,ig],mest[,ig+2]),col=1,lwd=3,type="b",main="",xlab="",ylab="")		
	mtext(side=1,"Sample size, n",cex=1.5,line=2.75)
	mtext(side=2,"Mean value",cex=1.5,line=2.75)
	mtext(side=3,what[ig],cex=2,line=.5)
	if(ig==1) segments(-1,alpha,1000,alpha) else segments(-1,lambda,1000,lambda)
	legend("topright",c("1=MM","2=GMM"),cex=1.5,bg=gray(.9))
}
}
