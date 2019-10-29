nonpN <-
function(SD=1,n=30,dft=3,alpha=.05,nSim=100000)
{
	dump("nonpN","c:\\StatBook\\nonpN.r")	
	mua=seq(from=-1,to=1,length=100);N=length(mua)
	powN=powB=rep(NA,N)
	Z1a=qt(1-alpha/2,df=n-1)
	B1a=qbinom(1-alpha/2,size=n,prob=.5)
	B2a=qbinom(alpha/2,size=n,prob=.5)
	par(mfrow=c(1,2),mar=c(3,3,2,1))
	namd=c("N",paste("T(df=",dft,")",sep=""))
	namdT=c("Normal distribution",paste("T-distribution with",dft,"df"))
	for(ig in 1:2)
	{			
		for(i in 1:N)
		{	
			if(ig==1) X=matrix(rnorm(n*nSim,mean=mua[i],sd=SD),ncol=n)
			if(ig==2) X=matrix(mua[i]+SD*rt(n*nSim,df=dft)/sqrt(n/(n-2)),ncol=n)
			avx=rowMeans(X);avx2=rowMeans(X^2)
			sdx=sqrt((avx2-avx^2)/(n-1)*n)
			powN[i]=mean(abs(avx)/sdx>Z1a/sqrt(n))	
			Z=matrix(0,ncol=n,nrow=nSim)
			Z[X>0]=1
			m=rowSums(Z)
			powB[i]=mean(m>B1a | m<B2a)	
		}
		matplot(mua,cbind(powN,powB),type="l",col=1,lwd=3,ylim=c(0,1),xlab="",ylab="")
		title(namdT[ig])
		mtext(side=1,"Alternative mu",cex=1.3,line=2)
		mtext(side=2,"Power",cex=1.3,line=2)
		legend("bottomright",c(namd[ig],"Binomial"),cex=1.25,lty=1:2,lwd=3,bg="gray90")
		segments(-10,alpha,100,alpha,col="gray90")	
		text(-1,.2,paste("n =",n),font=4,cex=1.25,adj=0)
	}

}
