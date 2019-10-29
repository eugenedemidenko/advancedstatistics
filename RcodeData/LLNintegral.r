LLNintegral <-
function(ss=4)
{
	dump("LLNintegral","c:\\StatBook\\LLNintegral.r")
	par(mfrow=c(1,1),mar=c(4,4,.2,.5))
	set.seed(ss)
	n=seq(from=100,to=10000,by=100)
	ln=length(n)
	int=rep(NA,ln)
	a=-10;b=5
	for(i in 1:ln)
	{		
		X=runif(n[i],min=a,max=b)
		int[i]=(b-a)*mean(exp(-0.123*X^6)*log(1+X^8))
	}
	plot(n,int,type="b",xlab="Number of simulated values, n",ylab="LLN integral")
	exact.int=integrate(function(x) exp(-0.123*x^6)*log(1+x^8),lower=-10,upper=5)$value
	segments(-1000,exact.int,10000,exact.int,lwd=3)
	text(4000,1.2,paste("Exact integral =",round(exact.int,5)),adj=0)
}
