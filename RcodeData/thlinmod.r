thlinmod <-
function(n=10,b0=-1,tau=.6,s2=2,sdx=1,sdu=.5,roxu=.7,nSim=100000)
{
dump("thlinmod","c:\\StatBook\\thlinmod.r")
par(mfrow=c(1,1),mar=c(4,4,3.5,1),cex.lab=1.5)
X=rnorm(n*nSim,sd=sdx);Z=rnorm(n*nSim)
h=roxu/sdx/sqrt(1-roxu^2);U=(Z+h*X)/sqrt(h^2*sdx^2+1)

Y=b0*X+tau*U+rnorm(n*nSim,sd=sqrt(s2))
X=matrix(X,ncol=n);U=matrix(U,ncol=n);Y=matrix(Y,ncol=n)
v2=sdx^2*(1-roxu^2)
c11=rowSums(X^2);c12=rowSums(X*U);c22=rowSums(U^2)
yx=rowSums(X*Y);yu=rowSums(U*Y)
disc=c11*c22-c12^2
b.hat=(yx*c22-yu*c12)/disc;tau.hat=(-yx*c12+yu*c11)/disc
res=Y-b.hat*X-tau.hat*U
Smin=rowSums(res^2)
s2.hat=Smin/n 
#s2.hat=Smin/(n-2) #unbiased sigma.hat^2
#Wald
WA=(b.hat-b0)/sqrt(s2.hat/v2/n)
WA=WA[order(WA)]
# LR
tau0=rowSums((Y-b0*X)*U)/rowSums(U^2)
res0=Y-b0*X-tau0*U
S0=rowSums(res0^2)
LR=sign(b.hat-b0)*sqrt(n*log(S0/Smin))
LR=LR[order(LR)]
#score
SC=rowSums(res0*X)/sqrt(v2*s2.hat*n)
SC=SC[order(SC)]
matplot(cbind(WA,LR,SC),(1:nSim-.5)/nSim,lwd=1,type="l",lty=1,xlim=c(-4,4),xlab="Test statistic value from simulations",ylab="cdf")
x=seq(from=-4,to=4,length=200)
lines(x,pnorm(x),lty=2)
legend("topleft",c("Wald","LR","Score","N(0,1)"),col=c(1:3,1),lty=c(1,1,1,2),lwd=2,bg="gray90",cex=1.5)
title(paste("N(0,1) approximation of linear regression slope beta with random X and U\nn=",n,", b0=",b0,", tau=",tau,", s2=",s2,", sdx=",sdx,", sdu=",sdu, ", roxu=",roxu,", nSim=",nSim,sep=""))

}
