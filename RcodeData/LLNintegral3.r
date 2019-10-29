LLNintegral3 <-
function(p=0.4,n=10)
{
	dump("LLNintegral3","c:\\StatBook\\LLNintegral3.r")
	par(mfrow=c(1,1),mar=c(4,4,3,.5))
	k=0:n
	cdfB=pbinom(q=k,size=n,prob=p)
	plot(k,cdfB,type="s",ylab="cdf, probability")
	title(paste("Binomial distribution with p=",p,", n=",n,sep=""))
	x=seq(from=0,to=n,length=100)
	Z=(x-n*p)/sqrt(n*p*(1-p))
	lines(x,pnorm(Z),lwd=3)
	legend(5,.3,c("Binomial cdf","Normal cdf"),lty=1,lwd=c(1,3),cex=1.5)
}
