kidsdrink <-
function(job=1)
{
dump("kidsdrink","c:\\StatBook\\kidsdrink.r")
d=read.csv("c:\\StatBook\\kidsdrink.csv",header=T)
if(job==1)
{
	par(mfrow=c(1,1),mar=c(4.5,4.5,1,1),cex.lab=1.5)
	plot(d$age,d$alcm,type="n",xlab="Age",ylab="Time of watching, hours")
	for(a in 12:16)
	{
		da=d$alcm[d$age==a];n=length(da)
		points(rep(a,n),da)
		den=density(da,from=0)
		lines(a+2.25*den$y,den$x)	
	}
}
if(job==2)
{
	d=cbind(d,log(1/60^2+d$alcm))
	names(d)[ncol(d)]="logalcm"
	o=lm(logalcm~drink+age+boy+race+alcbr+pared+inc+grade,data=d)
	print(summary(o))
	par(mfrow=c(1,1),mar=c(4.5,4.5,1,1),cex.lab=1.5)
	alab=c(1,2,5,10,25,50);lalab=log(alab)
	plot(d$age,d$logalcm,xlim=c(12,17),ylim=range(lalab),type="n",axes=F,xlab="Age",ylab="Alcohol scene watching")
	axis(side=1,12:16)
	axis(side=2,at=lalab,labels=paste(alab,"h"),srt=90)
	for(a in 12:16)
	{
		da=d$logalcm[d$age==a];n=length(da)
		points(rep(a,n),da)
		den=density(da,from=0)
		lines(a+1.25*den$y,den$x)	
	}
	x=11:16
	a=coef(o)
	lines(x,a[1]+a[2]+a[3]*x+a[4]+a[5]+a[6]+a[7]+a[8]+a[9],col=2,lwd=3)
	lines(x,a[1]+a[3]*x+a[4]+a[6]+a[7]+a[8]+a[9],col=3,lwd=3,lty=2)
	legend(13.2,log(2),c("Drink and have an alcohol related item","Do not drink and do not have an alcohol related item"),col=2:3,lwd=3,lty=1:2,bg="gray97",cex=1.5)
}


}
