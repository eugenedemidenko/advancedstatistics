meanmed <-
function(n=10,lambda=1,nSim=100000)
{
dump("meanmed","c:\\StatBook\\meanmed.r")
s=sample(x=c(-1,1),replace=T,size=n*nSim,prob=c(.5,.5))
Y=matrix(s*rexp(n=n*nSim,rate=lambda),ncol=n)
med=apply(X=Y,MARGIN=1,FUN=median)
mea=apply(X=Y,MARGIN=1,FUN=mean)
print(paste("Variance of median=",var(med)))
print(paste("Variance of mean=",var(mea)))
print(paste("Ratio=",var(mea)/var(med)))
}
