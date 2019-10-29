SCcancerQQ2 <-
function()
{
    dump("SCcancerQQ2","c:\\StatBook\\SCcancerQQ2.r")
    x=read.csv(file="c:\\StatBook\\DeathYears.csv",header=F)[,1]
	par(mfrow=c(1,1),mar=c(4.25,4.5,1,1),cex.axis=1.25,cex.lab=1.5)
    x=x[order(x)];n=length(x)
    y=(1:n-.5)/n
    lambda10=1/mean(x[x<1])
	lambda20=1/mean(x)
	o=nls(y~p*(1-exp(-lambda1*x))+(1-p)*(1-exp(-lambda2*x)),start=list(p=.7,lambda1=lambda10,lambda2=lambda20))
	print(summary(o))
	year=seq(from=0,to=20,length=100)
	a=coef(o)
	Ft=a[1]*(1-exp(-a[2]*year))+(1-a[1])*(1-exp(-a[3]*year))
	plot(x,y,type="s",xlab="Cancer survival, years",ylab="Chance to die")
	lines(year,Ft,lwd=3)
	legend(5,.22,c("Empirical cdf","Two-component exponential cdf"),lwd=c(1,3),bg=gray(.93),cex=1.25)	
 }
