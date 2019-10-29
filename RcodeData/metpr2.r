metpr2 <-
function(Nsim=100000)
{
dump("metpr2","c:\\StatBook\\metpr2.r")
par(mfrow=c(1,1),mar=c(4,4,1,1))
theta=1:50
pr=rep(NA,50)
for(i in 1:50)
{
	X=rexp(Nsim,rate=1/theta[i])
	Y=rexp(Nsim,rate=1/theta[i])
	pr[i]=mean(abs(X-Y)<15)
}
plot(theta,pr,xlab="Mean late arrival, theta",ylab="Meeting probability")
x=seq(from=1,to=50,length=100)
prFORM=1-exp(-15/x)
lines(x,prFORM)
legend(25,1,c("Simulations","Theoretical"),lty=c(NA,1),pch=c(1,NA),cex=1.25)
}
