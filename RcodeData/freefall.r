freefall <-
function(job=1,n=2,N=200,g=9.8)
{
dump("freefall","c:\\StatBook\\freefall.r")
if(job==1)
{
	time=seq(from=0,to=2,length=N)
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	Cs=c(.25,20)
	St=matrix(ncol=n,nrow=N)
	for(i in 1:n)
	St[,i]=1/Cs[i]*log(.5*(exp(time*sqrt(g*Cs[i]))+exp(-time*sqrt(g*Cs[i]))))
	matplot(time,St,type="l",lty=1,col=1,ylim=c(0,20),xlim=c(0,2.5),lwd=3,axes=F,xlab="",ylab="")
	axis(side=1,seq(from=0,to=2,by=.25))
	axis(side=2,seq(from=0,to=20,by=1))
	lines(time,.5*g*time^2,lwd=3)
	text(2.01,.5*g*2^2,"C = 0",adj=0,cex=1.5)
	for(i in 1:n)
	text(2.01,1/Cs[i]*log(.5*(exp(time[N]*sqrt(g*Cs[i]))+exp(-time[N]*sqrt(g*Cs[i])))),paste("C =",Cs[i]),adj=0,cex=1.5)
	mtext(side=1,"Time, seconds",cex=1.5,line=2.75)
	mtext(side=2,"Distance covered, m",cex=1.5,line=2.75)
}
if(job==2)
{
	sigma=.2	
	C.true=.2
	time=seq(from=.1,to=2,by=.2);n=length(time)
	Si=1/C.true*log(.5*(exp(time*sqrt(g*C.true))+exp(-time*sqrt(g*C.true))))+rnorm(n,mean=0,sd=sigma)
	o=nls(Si~1/C.est*log(.5*(exp(time*sqrt(g*C.est))+exp(-time*sqrt(g*C.est)))),start=c(C.est=.1,g=9.8))
	plot(time,Si)
	tix=seq(from=.1,to=2,length=200)
	a=coef(o)
	s=1/a[1]*log(.5*(exp(tix*sqrt(a[2]*a[1]))+exp(-tix*sqrt(a[2]*a[1]))))
	lines(tix,s)	
	print(summary(o))
}
if(job==3)
{
	ff=function(time,C.est,g=9.7935) 1/C.est*log(.5*(exp(time*sqrt(g*C.est))+exp(-time*sqrt(g*C.est))))
	da=read.csv("c:\\StatBook\\FallingHat.csv")
	n=nrow(da)
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	plot(da$time,da$St,xlab="",ylab="")
	mtext(side=1,"Time, seconds",line=2.75,cex=1.25)
	mtext(side=2,"Distance covered by a free falling hat, meters",line=2.75,cex=1.25)
	
	o=nls(St~ff(time,C.est),data=da,start=c(C.est=.1))
	print(summary(o));RSS.C=summary(o)$sigma^2*(n-1)
	C.est=coef(o)
	ts=seq(from=0,to=.4,length=200)
	Newt=0.5*9.7935*ts^2;RSS.Newton=sum((da$St-0.5*9.7935*da$time^2)^2)
	lines(ts,ff(ts,C.est),lwd=2)
	lines(ts,Newt,lwd=2,lty=2)
	Stg=da$St-0.5*9.7935*da$time^2
	olm=lm(Stg~da$time-1)
	cc=coef(olm)
	print(summary(olm))
	lines(ts,0.5*9.7935*ts^2+cc*ts,lwd=2,lty=3)
	RSS.lin=summary(olm)$sigma^2*(n-1)
	ch1=paste("C-model (RSS=",round(RSS.C,4),")",sep="")
	ch2=paste("Newton (RSS=",round(RSS.Newton,4),")",sep="")
	ch3=paste("Linear (RSS=",round(RSS.lin,4),")",sep="")
	legend(0,.7,c(ch1,ch2,ch3),lwd=2,lty=1:3,bg="gray93",cex=1.5)
	
}
}
