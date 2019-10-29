rugf2 <-
function(p=0.4,mu1=1,mu2=3,n=1000)
{
dump("rugf2","c:\\StatBook\\rugf2.r")
par(mfrow=c(1,1),mar=c(4,4,1,1))
Y=runif(n)
qy=qnorm(Y)
x=p*(qy+mu1)+(1-p)*(qy+mu2)
for(it in 1:10)
{
	num=p*pnorm(x-mu1)+(1-p)*pnorm(x-mu2)-Y
	den=p*dnorm(x-mu1)+(1-p)*dnorm(x-mu2)
	delta=num/den
	if(max(abs(delta))<0.001) break
	x=x-delta
	print(c(it,max(abs(delta))))
}
x=x[order(x)]
Fv=(1:n)/n
plot(x,Fv,type="s",xlab="x",ylab="Probability")
lines(x,p*pnorm(x-mu1)+(1-p)*pnorm(x-mu2),col=2)
legend(min(x),1,c("Empirical cdf","Theoretical cdf"),lty=1,col=1:2,cex=1.5)
}
