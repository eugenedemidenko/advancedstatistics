qqnill <-
function(n=100)
{
dump("qqnill","c:\\StatBook\\qqnill.r")
par(mfrow=c(1,2),cex.main=1.5,cex.lab=1.25)
x=rnorm(n,mean=1,sd=2)
x=x[order(x)]
ip=(1:n-0.5)/n
plot(qnorm(ip,mean=0,sd=1),x,xlim=c(-4,4),ylim=c(-4,4),xlab="Theoretical quantiles with mu=0 and SD=1",ylab="Ordered observations")
title("Q-Q plot with the 'wrong' mu and SD")
segments(-5,-5,5,5,lwd=3,col=2)
xx=c(-5,5);lines(xx,1+2*xx,col=3)
plot(qnorm(ip,mean=1,sd=2),x,xlim=c(-4,4),ylim=c(-4,4),xlab="Theoretical quantiles with mu = 1 and SD = 2",ylab="Ordered observations")
title("Q-Q plot with the 'right' mu and SD")
segments(-5,-5,5,5,lwd=3,col=2)
}
