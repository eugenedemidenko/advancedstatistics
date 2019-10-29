mle.gamma.OPT <-
function(ss=3,alpha=1,lambda=2,n=20,maxit=20)
{
dump("mle.gamma.OPT","c:\\StatBook\\mle.gamma.OPT.r")
set.seed(ss)
X=rgamma(n,shape=alpha,rate=lambda)
lMM=mean(X)/var(X);aMM=mean(X)^2/var(X)
A0=sum(log(X));A1=sum(X)
der1=der2=rep(0,2);HNR=HEFS=matrix(0,ncol=2,nrow=2)
a1=a2=aMM;l1=l2=lMM	
all1=all2=matrix(nrow=maxit,ncol=4)
for(it in 1:maxit)
{
# NR and FS
	dera1=n*log(l1)-n*digamma(a1)+A0
	derl1=n*a1/l1-A1
	deri1=c(dera1,derl1)
	HNR[1,1]=trigamma(a1);HNR[1,2]=HNR[2,1]=-1/l1
	HNR[2,2]=a1/l1^2
	HNR=n*HNR
	a12=all1[it,1:2]=c(a1,l1)+solve(HNR)%*%deri1
	a1=a12[1];l1=a12[2]
	all1[it,3]=n*a1*log(l1)-n*log(gamma(a1))+(a1-1)*A0-l1*A1
	all1[it,4]=sum(abs(deri1))
# EFS	
	dera2=log(l2)-digamma(a2)+log(X)
	derl2=a2/l2-X
	deri2=c(sum(dera2),sum(derl2))
	der2=cbind(dera2,derl2)
	HEFS=t(der2)%*%der2		
	a12=all2[it,1:2]=c(a2,l2)+solve(HEFS)%*%deri2
	a2=a12[1];l2=a12[2]
	all2[it,3]=n*a2*log(l2)-n*log(gamma(a2))+(a2-1)*A0-l2*A1
	all2[it,4]=sum(abs(deri2))
}
par(mfrow=c(1,3),mar=c(4,4,4,1))
tit=c("alpha","lambda","log-likelihood")
for(j in 1:3)
matplot(1:maxit,cbind(all1[,j],all2[,j]),type="b",col=1,lty=1,xlab="iteration",ylab="",main=tit[j])	
}
