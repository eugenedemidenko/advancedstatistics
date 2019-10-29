binprop2 <-
function(n1=50,n2=40,alpha=0.05,N=100,nSim=10000)
{
dump("binprop2","c:\\StatBook\\binprop2.r")
par(mfrow=c(1,2),mar=c(3.5,3.5,3,1),cex.main=1.5)
cl=c(1,"gray70")
Z1a=qnorm(1-alpha/2)
pnull=seq(from=0.01,to=.99,length=N)
sigNE=sigEQ=rep(NA,nrow=N)
for(i in 1:N)
{	
	p1.hat=rbinom(n=2*nSim,size=n1,prob=pnull[i])/n1
	p2.hat=rbinom(n=2*nSim,size=n2,prob=pnull[i])/n2
	SE1=sqrt(p1.hat*(1-p1.hat)/n1+p2.hat*(1-p2.hat)/n2)
	sigNE[i]=mean(abs(p1.hat-p2.hat)/SE1>Z1a)
	p.hat=(p1.hat*n1+p2.hat*n2)/(n1+n2)
	SE2=sqrt(p.hat*(1-p.hat)*(1/n1+1/n2))
	sigEQ[i]=mean(abs(p1.hat-p2.hat)/SE2>Z1a)	
}	
matplot(pnull,cbind(sigNE,sigEQ),type="l",col=cl,lty=1,lwd=2,xlab="",ylab="",main="Type I error")
segments(-1,alpha,1,alpha)
mtext(side=1,"The null probability, p0",cex=1.25,line=2.25)
mtext(side=2,"Empirical significance level",cex=1.25,line=2.5)
legend("bottomleft",c("Nonequal-variance test","Equal-variance test"),col=cl,lwd=2,lty=1,cex=1.5,bg="gray94")

p1=p2=seq(from=.2,to=.8,length=N)
powNE=powEQ=matrix(ncol=N,nrow=N)
for(i in 1:N)
for(j in 1:N)
{	
	p1.hat=rbinom(n=nSim,size=n1,prob=p1[i])/n1
	p2.hat=rbinom(n=nSim,size=n2,prob=p2[j])/n2
	SE1=sqrt(p1.hat*(1-p1.hat)/n1+p2.hat*(1-p2.hat)/n2)
	powNE[i,j]=mean(abs(p1.hat-p2.hat)/SE1>Z1a)
	p.hat=(p1.hat*n1+p2.hat*n2)/(n1+n2)
	SE2=sqrt(p.hat*(1-p.hat)*(1/n1+1/n2))
	powEQ[i,j]=mean(abs(p1.hat-p2.hat)/SE2>Z1a)	
}
contour(p1,p2,powNE,levels=c(.5,.6,.7,.8,.9),col=cl[1],labcex=1)
contour(p1,p2,powEQ,levels=c(.5,.6,.7,.8,.9),col=cl[2],add=T,labcex=1)	
title("Power functions")
mtext(side=1,"p1",cex=1.25,line=2.3)
mtext(side=2,"p2",cex=1.25,line=2.5)
legend(.3,.5,c("Nonequal-variance test","Equal-variance test"),col=cl,cex=1.5,lwd=2,lty=1,bg="gray90")

}
