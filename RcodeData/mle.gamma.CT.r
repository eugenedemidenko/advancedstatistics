mle.gamma.CT <-
function(povsal=11000)
{
dump("mle.gamma.CT","c:\\StatBook\\mle.gamma.CT.r")
sc=scan("c:\\StatBook\\Connecticut.txt",what="")
sc=as.numeric(sc);X=sc[!is.na(sc)]
n=length(X)
lMM=mean(X)/var(X);aMM=mean(X)^2/var(X)
A0=sum(log(X));A1=sum(X)
der1=rep(0,2);HNR=matrix(0,ncol=2,nrow=2)
a1=aMM;l1=lMM	
eps=.0000001;maxit=100
for(it in 1:maxit)
{
	dera1=n*log(l1)-n*digamma(a1)+A0
	derl1=n*a1/l1-A1
	deri1=c(dera1,derl1)
	HNR[1,1]=trigamma(a1);HNR[1,2]=HNR[2,1]=-1/l1
	HNR[2,2]=a1/l1^2
	HNR=n*HNR
	a12=c(a1,l1)+solve(HNR)%*%deri1
	a1=a12[1];l1=a12[2]
	loglik=n*a1*log(l1)-n*log(gamma(a1))+(a1-1)*A0-l1*A1
	if(sum(abs(deri1))<eps) break		
}
print(paste("alphaML=",signif(a1,4),", lambdaML=",signif(l1,4)))	
X=X[order(X)]
print(paste("Minimum salary =",X[1]))
par(mfrow=c(1,1),mar=c(4,4,1,1))
plot(X,(1:n)/n,xlim=c(0,250000),type="s",xlab="",ylab="",lwd=3)
mtext(side=1,"Salary, $",line=2.5,cex=1.5)
mtext(side=2,"Probability, cdf",line=2.5,cex=1.5)
legend(100000,.4,c("Empirical cdf","gamma cdf"),col=1:2,lwd=c(3,1),bg=gray(.9),cex=1.5)
rug(X)
X=seq(from=0,to=250000,length=200)
lines(X,pgamma(X,shape=a1,rate=l1),col=2)
pvpr=pgamma(povsal,shape=a1,rate=l1)*100
segments(povsal,-1,povsal,.3)
text(povsal,.31,paste("Povery salary=$",povsal,sep=""),srt=90,adj=0,cex=1.25)
paste("% people in poverty =",round(pvpr,2))
text(50000,.07,paste(round(pvpr,2),"% of population are in poverty",sep=""),adj=0,font=2)
}
