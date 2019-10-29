binprop1 <-
function(job=1,n=50,alpha=0.05,p0=0.2,N=50,nSim=100000)
{
dump("binprop1","c:\\StatBook\\binprop1.r")

if(job==1) #Testing the significance level of the Wald test for binomial proportion
{	
	par(mfrow=c(1,2),mar=c(3.5,3.5,3,1),cex.main=1.5)
	cl=c(1,"gray60","gray80")
	cl=2:4
	Z1a=qnorm(1-alpha/2)
	pnull=seq(from=0.1,to=.9,by=.01);N=length(pnull)
	sigNW=sigEW=sigICDF=rep(NA,nrow=N)
	j=0
	for(n in c(10,50))
	{
		j=j+1
		for(i in 1:N)
		{
			p.hat=rbinom(n=nSim,size=n,prob=pnull[i])/n
			sigNW[i]=mean(abs(p.hat-pnull[i])>Z1a*sqrt(pnull[i]*(1-pnull[i])/n))
			sigEW[i]=mean(abs(p.hat-pnull[i])>Z1a*sqrt(p.hat*(1-p.hat)/n))
			FB.eq=pbeta(pnull[i],n*p.hat+.5,n*(1-p.hat)+.5)	
			sigICDF[i]=mean(FB.eq<alpha/2 | FB.eq>1-alpha/2)
		}
		matplot(pnull,cbind(sigNW,sigEW,sigICDF),type="l",col=cl,lty=1,lwd=2,xlab="",ylab="",main=paste("n =",n))			
		segments(-1,alpha,1,alpha)
		mtext(side=1,"The null probability, p0",cex=1.25,line=2.25)
		mtext(side=2,"Empirical significance level",cex=1.25,line=2.5)
		if(j==1) legend(.3,max(cbind(sigNW,sigEW,sigICDF)),c("Null Wald","Estimated Wald","Clopper-Pearson"),col=cl,lty=1,lwd=3,cex=1.5,bg="gray90")
	}		
}
if(job==2) #Power functions for testing the binomial proportion
{	
	par(mfrow=c(1,2),mar=c(3.5,3.5,2,1),cex.main=1.5)
	Z1a=qnorm(1-alpha/2)
	p.alt=seq(from=p0/4,to=2*p0,length=N)
	powNW=powCP=powCP.emp=rep(NA,N)
	j=0
	for(n in c(30,100))
	{
		j=j+1
		q1=qbeta(.5*alpha,n*p0+.5,n*(1-p0)+.5)	
		q2=qbeta(.5*alpha,n*p0+.5,n*(1-p0)+.5,lower.tail=F)	
		for(i in 1:N)
		{	
			p.hat=rbinom(n=nSim,size=n,prob=p.alt[i])/n
			powNW[i]=mean(abs(p.hat-p0)>Z1a*sqrt(p0*(1-p0)/n))							
			powCP[i]=pbeta(q1,n*p.alt[i]+.5,n*(1-p.alt[i])+.5)+pbeta(q2,n*p.alt[i]+.5,n*(1-p.alt[i])+.5,lower.tail=F)	
			powCP.emp[i]=mean(p.hat<q1 | p.hat>q2)
		}
		allp=cbind(powCP,powCP.emp,powNW)	
		matplot(p.alt,allp,type="l",lty=1:3,col=1,lwd=2,xlab="",ylab="")	
		segments(-1,alpha,1,alpha)
		segments(p0,-1,p0,.3,col="gray70",lwd=2)
		title(paste("n =",n))
		mtext(side=1,"The alternative probability, p",cex=1.25,line=2.3)
		mtext(side=2,"Power",cex=1.25,line=2.5)
		if(j==1) legend(.1,max(allp),c("Theoretical Clopper-Pearson","Simulation-derived Clopper-Pearson","Simulation-derived null Wald"),col=1,lwd=2,lty=1:3,cex=1,bg="gray90")
	}		
}
if(job==3)
{
	n=50
	Z1a=qnorm(1-alpha/2)
	m=rbinom(n=1,size=n,prob=p0);p.hat=m/n
	sd0=sqrt(p0*(1-p0)/n)
	Z=(p.hat-p0)/sd0
	print(c(alpha,Z1a,m,Z))
	q1=n*p0-Z1a*sd0*n;q2=n*p0+Z1a*sd0*n
	print(c(q1,q2))
	q12=q1q2icdf(p0=p0,alpha=alpha,n=n)
	print(q12)

}
}
