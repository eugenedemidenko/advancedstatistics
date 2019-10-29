expr <-
function(NSIM=1000,alpha=-1,beta=.2)
{
dump("expr","c:\\StatBook\\expr.r")
y=runif(NSIM)
x=-(alpha+log(1/y-1))/beta
x=x[order(x)]
plot(x,(1:NSIM)/NSIM,type="s",ylab="Probability")
title("Empirical (black) and theoretical (red) cdfs")
xx=seq(from=x[1],to=x[NSIM],length=100)
lines(xx,exp(alpha+beta*xx)/(1+exp(alpha+beta*xx)),col=2)

}
