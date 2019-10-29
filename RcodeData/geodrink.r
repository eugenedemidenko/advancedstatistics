geodrink <-
function(job=1,alpha=.05)
{
dump("geodrink", "c:\\StatBook\\geodrink.r")
dat=read.csv("c:\\StatBook\\kidsdrinkDAT.csv")
print(names(dat))
o=glm(bingedr~frndalc+age+black+senseek+alcmovie+parentmont+cthhsz,family=binomial,data=dat)
a=coef(o)
X=cbind(dat$senseek,dat$alcmovie,dat$parentmont,dat$cthhsz)
mx=colMeans(X,na.rm=T)
n=nrow(X)
if(job==1)
{	
	print(summary(o))
	par(mfrow=c(1,1),mar=c(3.5,3.5,1,.1))
	plot(1,1,xlim=c(10,95),ylim=c(0,50),xlab="",ylab="",axes=F)
	axis(side=1,seq(from=10,to=90,by=10))
	axis(side=2,seq(from=0,to=50,by=10))
	mtext(side=1,"Quantile, %",cex=1.25,line=2.5)
	mtext(side=2,"Probability of binge drinking of a white 16-year-old, %",cex=1.25,line=2.25)
	legend(10,50,c("Sensation seeking (1)","Alcohol movie watching (2)","Parent's monitoring (3)","CT household size (4)"),lty=1,col=2:5,lwd=3,cex=1.25,bg="gray90")
	text(93,7,"My friends don't drink",font=3,cex=1.3,srt=90)
	text(93,35,"I have a friend who drinks",font=3,cex=1.3,srt=90)
	age=16;black=0
	for(i in 0:1)
	for(j in 1:4)
	{
		x=X[,j]
		x=x[order(x)]
		x10=x[n/10];x90=x[9*n/10]
		xplot=seq(from=x10,to=x90,length=100)
		lpred=a[1]+a[2]*i+a[3]*age+a[4]*black+a[4+j]*xplot
		a5=a[5:8]
		lpred=lpred+sum(a5[-j]*mx[-j])
		pr100=100*exp(lpred)/(1+exp(lpred))
		lines(seq(from=10,to=90,length=100),pr100,col=j+1,lwd=3)			
		text(9,pr100[1],j,cex=1.25)
	}
}
if(job==2)
{
	par(mfrow=c(1,1))
	N=100
	Z1a=qnorm(1-alpha/2)
	xalc=prm=lowp=upp=seq(from=0,to=1.3,length=100)
	for(i in 1:N)
	{
		x=c(1,1,14,0,mx[1],xalc[i],mx[3:4])
		varp=c(x)%*%vcov(o)%*%x
		prm[i]=sum(x*a)
		lowp[i]=prm[i]-Z1a*sqrt(varp)
		upp[i]=prm[i]+Z1a*sqrt(varp)		
	}
	prm=exp(prm)/(1+exp(prm))
	lowp=exp(lowp)/(1+exp(lowp))
	upp=exp(upp)/(1+exp(upp))
	plot(xalc,prm,type="l",lwd=3,xlab="alcmovie",ylab="Probability of binge drinking")
	lines(xalc,lowp)
	lines(xalc,upp)

	

}
}
