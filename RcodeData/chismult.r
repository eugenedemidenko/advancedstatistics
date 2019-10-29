chismult <-
function(p=c(.1,.2,.5,.2),n=30,nY=50,nSim=100000)
{
dump("chismult","c:\\StatBook\\chismult.r")
par(mfrow=c(1,2),cex.lab=1.5,cex.main=1.5)
p=p/sum(p)
m=length(p)
# One-sample test
X=rmultinom(n=nSim,size=n,prob=p)
p.hat=X/n
chP=n*colSums((p.hat-p)^2/p) # Pearson chi-square test
chP=chP[order(chP)]
plot(chP,(1:nSim-.5)/nSim,lwd=2,xlim=c(0,2*m),main=paste("One-sample test, n =",n),xlab="Test statistic",ylab="cdf",type="l",col=2)	
pp=(1/p)*p.hat
chS=2*n*colSums(p.hat*log(pp)) # LR test
chS=chS[order(chS)]
lines(chS,(1:nSim-.5)/nSim,col=3,lwd=2)
legend("bottomright",c("Chi.cdf","Pearson/Wald","LR"),lwd=2,bg="gray90",col=1:3,lty=1,cex=1.5)
# Two-sample test
Y=rmultinom(n=nSim,size=nY,prob=p)
p.hat2=(X+Y)/(n+nY)
pY.hat=Y/nY
t1=colSums(p.hat*log(p.hat))
t2=colSums(pY.hat*log(pY.hat))
t3=colSums(p.hat2*log(p.hat2))
cht2=2*(n*t1+nY*t2-(n+nY)*t3)
cht2=cht2[order(cht2)]
plot(cht2,(1:nSim-.5)/nSim,xlim=c(0,2*m),lwd=2,main=paste("Two-sample test, nX=",n,", nY=",nY,sep=""),xlab="Test statistic",ylab="cdf",type="l",col=3)	
x=seq(from=0,to=3*(2*m-2),length=200)
lines(x,pchisq(x,df=m-1),lwd=2)

chP=colSums((p.hat-pY.hat)^2/(p.hat/n+pY.hat/nY)) # Wald test
chP=chP[order(chP)]
lines(chP,(1:nSim-.5)/nSim,col=2,lwd=2)
legend("bottomright",c("Chi.cdf","Wald","LR"),col=1:3,lty=1,cex=1.5,lwd=2,bg="gray90")

}
