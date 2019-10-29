SCcancerQQ <-
function(job=1,alpha=.05)
{
dump("SCcancerQQ","c:\\StatBook\\SCcancerQQ.r")
y=read.csv(file="c:\\StatBook\\DeathYears.csv",header=F)[,1]
y=y[order(y)];n=length(y)
Fv=(1:n-.5)/n
q=-log(1-Fv)
if(job==1)
{
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	plot(q,y,ylab="",xlab="")
	mtext(side=1,"Theoretical quantile",cex=1.5,line=2.75)
	mtext(side=2,"Data (time to death)",cex=1.5,line=2.5)
	segments(-1,2,6,2,lty=2)
	sl=sum(q*y)/sum(q^2)
	lines(q,q*sl,col=3,lwd=3)
	q1=q[q<1];y1=y[q<1]
	sl1=sum(q1*y1)/sum(q1^2)
	lines(q1,q1*sl1,col=2,lwd=3)
	
}
if(job==2)
{
	y=y/mean(y)
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	plot(q,y,ylab="",xlab="")
	mtext(side=1,"Theoretical quantile",cex=1.5,line=2.75)
	mtext(side=2,"Data (time to death)",cex=1.5,line=2.5)
	segments(-1,2,6,2,lty=2)
	sl=sum(q*y)/sum(q^2)
	lines(q,q*sl,col=3,lwd=3)
	q2=q[y<2];y2=y[y<2]
	Z1a=qnorm(1-alpha/2)
	z=seq(from=0,to=6,length=1000)
	ez=exp(-z)
	up=-log(ez+Z1a*sqrt(ez*(1-ez)/n))
	lines(z,up,col=2)
	down=-log(ez-Z1a*sqrt(ez*(1-ez)/n))
	lines(z,down,col=2)
}

}
