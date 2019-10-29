poissamn <-
function(lambda0=1.2,mindet=.1,alpha=.05,pow=.9)
{
dump("poissamn","c:\\StatBook\\poissamn.r")
q12=function(lambda0,n,alpha=0.05)
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

Z1a=qnorm(1-alpha/2)
n=20:2000;nn=length(n)
s=sqrt(n)*mindet/sqrt(lambda0)
PWn=pnorm(-s-Z1a)+pnorm(s-Z1a)
par(mfrow=c(1,1),mar=c(3.5,3.5,2,1))
q1q2=matrix(ncol=2,nrow=nn)
for(i in 1:nn) q1q2[i,]=q12(lambda0=lambda0,n=n[i],alpha=alpha)
pICDF=pgamma((lambda0+mindet)*n,q1q2[,1]+1,1,lower.tail=F)+pgamma((lambda0+mindet)*n,q1q2[,2]+1,1)
matplot(n,cbind(PWn,pICDF),lwd=3,col=1,type="l",xlab="",ylab="")
mtext(side=1,"Sample size, n",cex=1.25,line=2.5)
mtext(side=2,"Theoretical power function",cex=1.25,line=2.5)
legend("bottomright",c("null Wald","Inverse cdf"),lty=1:2,lwd=2,col=1,cex=1.5,bg="gray90")
a=abs(PWn-pow);nW=n[a==min(a)]
lines(x=c(-200,nW,nW),y=c(pow,pow,0))
a=abs(pICDF-pow);nICDF=n[a==min(a)]
lines(x=c(-200,nICDF,nICDF),y=c(pow,pow,0),lty=2)
title(paste("nWALD = ",nW,", nICDF = ",nICDF,sep=""))
cat("The required sample size to detect",mindet,"difference in the birthrate is\n", nW,"for Wald test and", nICDF,"for inverse cdf test")

}
