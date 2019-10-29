thastest <-
function(n=30,p0=.3,nSim=100000)
{
dump("thastest","c:\\StatBook\\thastest.r")
par(mfrow=c(1,2),mar=c(4,4.5,3,1),cex.lab=1.5)
m=rbinom(n=nSim,size=n,prob=p0)
p.hat=m/n
WA0=n*(p.hat-p0)^2/p0/(1-p0);WA0=WA0[order(WA0)]
WAE=n*(p.hat-p0)^2/p.hat/(1-p.hat);WAE=WAE[order(WAE)]
lp.hat=m*log(p.hat)+(n-m)*log(1-p.hat)
l0=m*log(p0)+(n-m)*log(1-p0)
LR=2*(lp.hat-l0);LR=LR[order(LR)]
SC0=(m/p0-(n-m)/(1-p0))^2*p0*(1-p0)/n;SC0=SC0[order(SC0)]
SCE=(m/p0-(n-m)/(1-p0))^2*p.hat*(1-p.hat)/n;SCE=SCE[order(SCE)]
#print(mean(m/p0-(n-m)/(1-p0)))
matplot(cbind(WA0,WAE,LR,SC0,SCE),(1:nSim-.5)/nSim,lwd=2,type="l",lty=1,xlim=c(0,5),xlab="Test statistic value from simulations",ylab="cdf")
title(paste("Chi-square df=1 approximation of binomial proportion\np0=",p0,", n = ",n,", nSim = ",nSim,sep=""))
x=seq(from=0,to=10,length=200)
lines(x,pchisq(x,df=1),lty=2)
legend("bottomright",c("Wald null","Wald estimated","LR","Score null","Score estimated","chisq(df = 1)"),col=c(1:5,1),lty=c(1,1,1,1,1,2),lwd=2,bg="gray95",cex=1.5)


WA0=(p.hat-p0)/sqrt(p0*(1-p0)/n);WA0=WA0[order(WA0)]
WAE=(p.hat-p0)/sqrt(p.hat*(1-p.hat)/n);WAE=WAE[order(WAE)]
lp.hat=m*log(p.hat)+(n-m)*log(1-p.hat)
l0=m*log(p0)+(n-m)*log(1-p0)
LR=sign(p.hat-p0)*sqrt(2*(lp.hat-l0));LR=LR[order(LR)]
SC0=(m/p0-(n-m)/(1-p0))*sqrt(p0*(1-p0)/n);SC0=SC0[order(SC0)]
SCE=(m/p0-(n-m)/(1-p0))*sqrt(p.hat*(1-p.hat)/n);SCE=SCE[order(SCE)]
#print(mean(m/p0-(n-m)/(1-p0)))
matplot(cbind(WA0,WAE,LR,SC0,SCE),(1:nSim-.5)/nSim,lwd=2,type="l",lty=1,xlim=c(-4,4),xlab="Test statistic value from simulations",ylab="cdf")
title(paste("N(0,1) approximation of binomial proportion\np0=",p0,", n= ",n,", nSim = ",nSim,sep=""))
x=seq(from=-4,to=4,length=200)
lines(x,pnorm(x),lty=2)
legend("topleft",c("Wald null","Wald estimated","LR","Score null","Score estimated","N(0,1) cdf"),col=c(1:5,1),lty=c(1,1,1,1,1,2),lwd=2,bg="gray95",cex=1.5)




}
