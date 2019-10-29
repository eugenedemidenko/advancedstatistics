cltP <-
function(nExp=10000)
{
dump("cltP","c:\\StatBook\\cltP.r")
par(mfrow=c(1,1),mar=c(4.5,4.5,1,1),cex.lab=1.5)
X=runif(nExp);Y=rnorm(nExp)
Z=X+X^2+Y+Y^2
Z=Z[order(Z)]
Fv=(1:nExp)/nExp
plot(Z,Fv,type="s",xlab="Z",ylab="cdf",lwd=3,xlim=c(-1,10))
x=seq(from=-1,to=10,length=100)
lines(x,pnorm((x-11/6)/sqrt(571/180)),col=2,lty=2,lwd=3)
lines(x,pnorm(x,mean=5/6+1,sd=sqrt(61/180+3)),col=3,lty=3,lwd=3)
legend(4,.4,c("Empirical cdf","4-term normal cdf","2-term normal cdf"),lty=1:3,col=1:3,cex=1.5,lwd=3,bg=gray(.9))
}
