vartestSP2 <-
function()
{
dump("vartestSP2","c:\\StatBook\\vartestSP2.r")
GOOG=read.csv("c:\\StatBook\\GOOG.csv",stringsAsFactors=F);GOOG=GOOG[,6]
AMZN=read.csv("c:\\StatBook\\AMZN.csv",stringsAsFactors=F);AMZN=AMZN[,6]
n=length((AMZN))
par(mfrow=c(1,2),mar=c(3.5,3.75,2,1),cex.main=1.5)
matplot(1:n,cbind(GOOG,AMZN),type="l",lwd=2,col=2:3,lty=1,xlab="",ylab="",main="Stock price")
mtext(side=1,"Time, day",line=2.5,cex=1.5)
mtext(side=2,"Stock price",line=2.5,cex=1.5)
legend("topleft",c("GOOGL","AMZN"),col=2:3,lwd=3,lty=1,cex=1.25)
rGOOG=log(GOOG[2:n]/GOOG[1:(n-1)])
rAMZN=log(AMZN[2:n]/AMZN[1:(n-1)])
n1=n-1
matplot(1:n1,cbind(rGOOG,rAMZN),type="l",ylim=c(-.05,.05),col=2:3,xlab="",ylab="",lty=1,main="log return")
mtext(side=1,"Time, day",line=2.5,cex=1.5)
mtext(side=2,"log return",line=2.5,cex=1.5)
legend("topleft",c("GOOGL","AMZN"),col=2:3,lwd=3,lty=1,cex=1.25)
lines(lowess(rGOOG),lwd=3,col=2)
lines(lowess(rAMZN),lwd=3,col=3)
}
