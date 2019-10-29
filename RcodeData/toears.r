toears <-
function(job=1)
{
dump("toears","c:\\StatBook\\toears.r")
x=scan("c:\\StatBook\\toears.txt");n=length(x);x10=log10(x)
if(job==1)
{
	par(mfrow=c(1,2),mar=c(4,4,2,.1))
	hist(x,main="Original scale",freq=F,xlab="Arsenic in toenail, mg/g")
	segments(x,rep(-1,n),x,rep(.2,n))
	hist(x10,main="log10 scale",freq=F,xlab="log10 Arsenic in toenail, mg/g")	
	segments(x10,rep(-1,n),x10,rep(.05,n))
	lines(density(x10),lwd=3)
}
if(job==2)
{
	par(mfrow=c(1,2),mar=c(4,4,2,.1))
	x10=(x10-mean(x10))/sd(x10)
	qn=qnorm((1:n)/n)
	plot(qn,x10,xlim=c(-4,4),ylim=c(-4,4),xlab="Theoretical quantile, Z",ylab="Empirical quantile")
	title("q-q plot for normalized log10 toenail values")
	segments(-5,-5,5,5,lwd=2,col=2)
	segments(2,-5,2,10,lty=2)	
	aqn=abs(qn-2)
	x2=x10[aqn==min(aqn)]
	hist(x10,nclass=20,main="log10 scale",freq=F,xlab="log10 Arsenic in toenail, mg/g")	
	segments(x10,rep(-1,n),x10,rep(.05,n))
	lines(density(x10),lwd=3)
	segments(x2,-1,x2,.3,lty=2);text(x2,.31,round(10^x2))
}

}
