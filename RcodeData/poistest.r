poistest <-
function(n=2,lambda0=2,alpha=0.05,N=30,nSim=100000)
{
dump("poistest","c:\\StatBook\\poistest.r")
#one-sided test
par(mfrow=c(1,2),mar=c(4,4,3,1),cex.lab=1.25)

lambda=seq(from=lambda0,to=lambda0+5,length=N)
powNW=powEW=powICDF=rep(NA,N)
for(i in 1:N)
{
	Y=rpois(n=nSim,lambda=n*lambda[i])
	pvNW=pnorm(-(Y-n*lambda0)/sqrt(n*lambda0))
	powNW[i]=mean(pvNW<alpha)
	pvEW=pnorm(-(Y-n*lambda0)/sqrt(Y))
	powEW[i]=mean(pvEW<alpha)
	pvICDF=pgamma(n*lambda0,Y+1,1)
	powICDF[i]=mean(pvICDF<alpha)
}
matplot(lambda,cbind(powNW,powEW,powICDF),type="l",col=1,lwd=2,ylim=c(0,1),xlab="Alternative lambda",ylab="Simulation-derived power",main="One-sided test")
segments(0,alpha,1000,alpha,lty=2)
legend(x="topleft",c("Null Wald","Estimated Wald","Inverse cdf"),lty=1:3,lwd=2,cex=1.25,bg="gray94")
print("One-sided test powers")
print(cbind(lambda,powNW,powEW,powICDF))

#two-sided test
q12=function(lambda0,n,alpha=0.05)
{
	reg12=function(q1,q2,lambda0,n)
	{
		lhs1=pgamma(n*lambda0+.5,q1+1,1)-pgamma(n*lambda0+.5,q2+1,1)
		lhs2=(q1-q2)*log(n*lambda0+.5)-(lgamma(q1+1)-lgamma(q2+1))
		c(lhs1,lhs2)
	}
	q10=qpois(alpha/2,lambda=lambda0*n)
	q20=qpois(alpha/2,lambda=lambda0*n,lower.tail=F)
	y=c(1-alpha,0)
	o=nls(y~reg12(q1,q2,lambda0=lambda0,n=n),start=c(q1=q10,q2=q20),algorithm="port")
	return(coef(o))		
}

q12A=function(lambda0,n,alpha=0.05)
{
	reg12=function(q1,q2,lambda0,n)
	{
		lhs1=pgamma(n*lambda0,q1+1,1)-pgamma(n*lambda0,q2+1,1)
		lhs2=(q1-q2)*log(n*lambda0)-(lgamma(q1+1)-lgamma(q2+1))
		c(lhs1,lhs2)
	}
	q10=qpois(alpha/2,lambda=lambda0*n)
	q20=qpois(alpha/2,lambda=lambda0*n,lower.tail=F)
	y=c(1-alpha,0)
	o=nls(y~reg12(q1,q2,lambda0=lambda0,n=n),start=c(q1=q10,q2=q20),algorithm="port")
	return(coef(o))		
}



lambda=seq(from=.1,to=lambda0+5,length=N)
powNW=powEW=powICDF=powICDFNE=rep(NA,N)
q1q2=q12A(lambda0,n,alpha=alpha)
#alpha1=pgamma(n*lambda0+.5,q1q2[1]+1,1,lower.tail=F)
#alpha2=pgamma(n*lambda0+.5,q1q2[2]+1,1)
alpha1=pgamma(n*lambda0,q1q2[1]+1,1,lower.tail=F)
alpha2=pgamma(n*lambda0,q1q2[2]+1,1)

for(i in 1:N)
{
	Y=rpois(n=nSim,lambda=n*lambda[i])
	pvNW=2*pnorm(-abs(Y-n*lambda0)/sqrt(n*lambda0))
	powNW[i]=mean(pvNW<alpha)
	pvEW=2*pnorm(-abs(Y-n*lambda0)/sqrt(Y))
	powEW[i]=mean(pvEW<alpha)
	p1=pgamma(n*lambda0,Y+1,1);p2=pgamma(n*lambda0,Y+1,1,lower.tail=F)
	powICDF[i]=mean(p1<alpha/2 | p2<alpha/2)		
	powICDFNE[i]=mean(p1<alpha2 | p2<alpha1)		
}

matplot(lambda,cbind(powNW,powEW,powICDF),type="l",col=1,lwd=2,ylim=c(0,1),xlab="Alternative lambda",ylab="Simulation-derived power",main="Two-sided test")
lines(lambda,powICDFNE,col=2)
segments(0,alpha,1000,alpha,lty=2)
segments(lambda0,0,lambda0,1,col="gray80")
segments(0,alpha,1000,alpha,lty=2)
legend(x="topleft",c("Null Wald","Estimated Wald","Inverse cdf"),lty=1:3,lwd=2,cex=1.25,bg="gray90")
print("Two-sided test powers")
print(cbind(lambda,powNW,powEW,powICDF,powICDFNE))

}
