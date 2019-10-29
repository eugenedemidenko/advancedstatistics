qqLOGband <-
function(lambda=.95,n=100)
{
dump("qqLOGband","c:\\StatBook\\qqLOGband.r")
par(mfrow=c(1,1),mar=c(4,4,1,1))
x=exp(rnorm(n))
x=x[order(x)]
x=(x-mean(x))/sd(x)
thq=qnorm(((1:n)-.5)/n)
plot(thq,x,xlab="Theoretical quantile",ylab="Empirical quantile")
q=seq(from=-8,to=10,length=1000)
pthq=pnorm(q)
Zl=qnorm((1+lambda)/2)
lb=qnorm(pthq-Zl*sqrt(pthq*(1-pthq)/n))
ub=qnorm(pthq+Zl*sqrt(pthq*(1-pthq)/n))
lines(q,lb,col=2)
lines(q,ub,col=2)

}
