LLNintegral2 <-
function(lambda=1,ss=4)
{
	dump("LLNintegral2","c:\\StatBook\\LLNintegral2.r")
	set.seed(ss)
	x=seq(from=1,to=20,length=100)
	par(mfrow=c(1,2),mar=c(4,4.5,.2,.5),cex.lab=1.5)
	Lfx=-log(abs(sin(x)/(1+x^6)))
	plot(x,Lfx,type="l",ylab="-log(abs(sin(x)/(1+x^6)))")
	segments(0,0,25,25,lwd=3)
	text(10,5,paste("l =",lambda),font=5,cex=1.5)
    n=seq(from=100,to=10000,by=100)
    ln=length(n)
    int=rep(NA,ln)
    for(i in 1:ln)
    {               
        X=-log(1-runif(n[i]))/lambda
        int[i]=mean(sin(X)/(1+X^6)*exp(lambda*X))/lambda
    }
    plot(n,int,type="b",xlab="Number of simulated values, n",ylab="LLN integral")
    exact.int=integrate(function(x) sin(x)/(1+x^6),lower=0,upper=Inf)$value
    segments(-1000,exact.int,10000,exact.int,lwd=3)
    text(4000,0.495,paste("Exact integral =",round(exact.int,5)),adj=0)
}
