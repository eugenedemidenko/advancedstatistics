dn3 <-
function(nOBS=10000,muX=1,sdX=2,muY=1,sdY=2,dr="c")
{
dump("dn3",paste(dr,":\\statbook\\dn3.r",sep=""))
par(mfrow=c(1,3),mar=c(4,4,3,1),cex.axis=1.5,cex.main=2)
x.range=c(muX-3*sdX,muX+3*sdX)
y.range=c(muY-3*sdY,muY+3*sdY)

for(ro in c(-.7,0,.7))
{
X=rnorm(nOBS,mean=muX,sd=sdX)
beta=ro*sdY/sdX
alpha=muX-beta*muY
s2YX=sdY^2*(1-ro^2)
Y=rnorm(nOBS,alpha+beta*X,sd=sqrt(s2YX))
plot(X,Y,xlim=x.range,ylim=y.range,main=paste("ro =",ro),xlab="",ylab="")
lines(x.range,alpha+beta*x.range,lwd=3,col=3)
lines(x.range,alpha+beta*x.range-sqrt(s2YX),col=3,lwd=3,lty=2)
lines(x.range,alpha+beta*x.range+sqrt(s2YX),col=3,lwd=3,lty=2)

}

}
