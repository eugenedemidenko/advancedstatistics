sampP <-
function(lambda=3,nSim=10000)
{
dump("sampP","c:\\StatBook\\sampP.r")
n.max=lambda+5*sqrt(lambda)
x=0:n.max
p=dpois(x,lambda=lambda)
XN=sample(x=x,size=nSim,replace=T,prob=p)
plot(x,p,type="h",lwd=3,xlab="x",ylab="Probability")
title(paste("Random sample n=",nSim," from Poisson distribution lambda=",lambda))
p.emp=rep(0,n.max+1)
for(i in 0:n.max)
 p.emp[i+1]=mean(XN==x[i+1])
points(x,p.emp,pch=16,col=2)
}
