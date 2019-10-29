expgr <-
function(job=0,A.true=10,lambda.true=0.1,x=1:10,sigma=.2,nSim=10000,N=200,alpha=.05) #vectorized NLS simulations
{
dump("expgr","c:\\StatBook\\expgr.r")
print(date())
n=length(x)
Y.true=A.true*(1-exp(-lambda.true*x))

if(job==0) #display 4 simulations
{
	par(mfrow=c(2,2),mar=c(3,3,3,1))
	for(i in 1:4)
	{
		y=Y.true*exp(rnorm(n=n,sd=sigma))
		plot(x,y,type="o")
		Ly=log(y)
		lines(x,Y.true,lwd=3)
		alpha0=1.1*max(y);ly=log(1-y/alpha0)
		lambda0=-coef(lm(ly~x-1))
		out=try(nls(Ly~alpha+log(1-exp(-lambda*x)),start=list(alpha=alpha0,lambda=lambda0)))
		if(attr(out,"class")!="try-error")
		{	
			print(summary(out))
			b=coef(out)
			xx=seq(from=1,to=n,length=100)
			lines(xx,exp(b[1])*(1-exp(-b[2]*xx)),lwd=3,lty=2)
			title(paste("Asymptote estimate =",round(exp(b[1]),1)))
		}
	}
}
if(job==1)
{
	b=matrix(nrow=nSim,ncol=2)
	JJ=matrix(ncol=2,nrow=2)
	ebx=exp(-lambda.true*x)
	JJ[1,1]=n;JJ[1,2]=JJ[2,1]=sum(x*ebx/(1-ebx));JJ[2,2]=sum((x*ebx/(1-ebx))^2)
	iJJ=solve(JJ)
	for(isim in 1:nSim)
	{		
		y=Y.true*exp(rnorm(n=n,sd=sigma))
		Ly=log(y)
		alpha0=1.1*max(y);ly=log(1-y/alpha0)
		lambda0=-coef(lm(ly~x-1))
		out=try(nls(Ly~alpha+log(1-exp(-lambda*x)),start=list(alpha=alpha0,lambda=lambda0)))
		if(attr(out,"class")!="try-error") b[isim,]=coef(out)		
	}
	mis=is.na(b[,1]) & is.na(b[,2])
	nan=sum(mis)
	b=b[!mis,]
	par(mfrow=c(1,1),cex.lab=1.5,cex.main=2)
	plot(b,pch=16,cex=.3,xlim=c(1,4),ylim=c(0,.4),xlab="log(A) NLS estimate",ylab="lambda NLS estimate")
	rug(b[,1],side=3,col=3);rug(b[,2],side=4,col=3)	
	d1=density(b[,1]);lines(d1$x,.4+.04*(0.45-0)-.1*d1$y,col=4,lwd=2)
	b1=seq(from=1,to=5,length=N);dn1=dnorm(b1,mean=log(A.true),sd=sigma*sqrt(iJJ[1,1]))
	lines(b1,.4+.04*(0.4-0)-.1*dn1,col=4,lwd=2,lty=2)
	
	d2=density(b[,2]);lines(4+.04*(4-1)-.1*d2$y,d2$x,col=4,lwd=2)
	b2=seq(from=0,to=.4,length=N);dn2=dnorm(b2,mean=lambda.true,sd=sigma*sqrt(iJJ[2,2]))
	lines(4+.04*(4-1)-.1*dn2,b2,col=4,lwd=2,lty=2)	
	
    text(3,.3,paste("s =",sigma),font=5,cex=3)
	points(log(A.true),lambda.true,col=2);segments(log(A.true),-1,log(A.true),6,col=2);segments(-1,lambda.true,6,lambda.true,col=2)
	title(paste(nSim," simulations (",nan," failed), n = ",n,sep="")) 
	#Normal confidence region
	b11=rep(b1-log(A.true),rep=N);b22=rep(b2-lambda.true,each=N)
	iOM=JJ/sigma^2
	d2=matrix(b11^2*iOM[1,1]+2*b11*b22*iOM[1,2]+b22^2*iOM[2,2],ncol=N)
	contour(b1,b2,d2,add=T,levels=qchisq(1-alpha,df=2),lwd=3,drawlabels=F,lty=2)	
	#Near exact confidence region
	d3=matrix(ncol=N,nrow=N)
	jf=rep(1,2)
	for(i in 1:N)
	for(j in 1:N)
	{
		dfi=b1[i]+log(1-exp(-b2[j]*x))-log(A.true)-log(1-exp(-lambda.true*x))
		jf[1]=sum(dfi);jf[2]=sum(dfi*exp(-lambda.true*x)/(1-exp(-lambda.true*x))*x)
		d3[i,j]=t(jf)%*%solve(JJ)%*%jf/sigma^2
	}	
	#contour(b1,b2,d3,add=T,levels=qchisq(1-alpha,df=2),col=2,lwd=2,drawlabels=F,lty=1)	
	contour(b1,b2,d3,add=T,levels=qchisq(1-alpha,df=2),lwd=3,drawlabels=F)	
	legend(2.5,.27,c("kernel density","normal density","score 95% CR","normal 95% CR"),col=c(4,4,1,1),lty=c(1,2,1,2),lwd=2,bg="gray93",cex=2)		
}

}
