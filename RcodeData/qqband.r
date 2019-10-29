qqband <-
function(lambda=.95)
{
dump("qqband","c:\\StatBook\\qqband.r")
par(mfrow=c(1,3))
n=c(10,100,1000)
for(i in 1:3)
{
	x=rnorm(n[i])
	x=x[order(x)]
	x=(x-mean(x))/sd(x)
	thq=qnorm(((1:n[i])-.5)/n[i])
	plot(thq,x,xlab="Theoretical quantile",ylab="Empirical quantile")
	title(paste("n =",n[i]))
	q=seq(from=-8,to=8,length=1000)
	pthq=pnorm(q)
	Zl=qnorm((1+lambda)/2)
	lb=qnorm(pthq-Zl*sqrt(pthq*(1-pthq)/n[i]))
	ub=qnorm(pthq+Zl*sqrt(pthq*(1-pthq)/n[i]))
	lines(q,lb,col=2)
	lines(q,ub,col=2)
}


}
