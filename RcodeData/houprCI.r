houprCI <-
function(alpha=.5,mu.true=log(400),sd.true=.3,nSim=10000)
{
dump("houprCI","c:\\StatBook\\houprCI.r")
X=c(348,297,734,298,503,465)
n=length(X)
t1a=qt(1-alpha/2,df=n-1)
LX=log(X)
mx=mean(LX);sig=sd(LX)
CI.l=exp(mx-t1a*sig/sqrt(n))
CI.u=exp(mx+t1a*sig/sqrt(n))

CR.l=exp(mx-t1a*sig)
CR.u=exp(mx+t1a*sig)

cat("The ", alpha*100,"% CI for the median: (",round(CI.l),",",round(CI.u),")\n",sep="")
cat("The ", alpha*100,"% confidence range for the individual house price: (",round(CR.l),",",round(CR.u),")\n",sep="")

#simulations

LX=matrix(rnorm(nSim*n,mean=mu.true,sd=sd.true),ncol=n)
X=exp(LX)
mx=rowMeans(LX);sig=apply(LX,1,sd)
CI.l=exp(mx-t1a*sig/sqrt(n))
CI.u=exp(mx+t1a*sig/sqrt(n))
cpCI=mean(CI.l<exp(mu.true) & CI.u>exp(mu.true))

CR.l=exp(mx-t1a*sig)%*%t(rep(1,6))
CR.u=exp(mx+t1a*sig)%*%t(rep(1,6))
CC=X>CR.l & X<CR.u
cpCR=mean(CC)
cat("CI simulation derived coverage probability for the median =",cpCI)
cat("\nConfidence range simulation derived coverage probability for the individual house =",cpCR)
}
