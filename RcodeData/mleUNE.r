mleUNE <-
function(sr=4)
{
	dump("mleUNE","c:\\StatBook\\mleUNE.r")
	set.seed(sr)
	r=runif(n=200,min=1,max=25)
	r=c(r,100)
	n=length(r)
	par(mfrow=c(1,1),mar=c(4,1,1,1))
	plot(r,rep(1,n),type="h",ylim=c(0,1),xlim=c(0,100),axes=F,xlab="",ylab="")
	mtext(side=1,"Unemployment rate per area, %",line=2.75,cex=2)
	axis(side=1,seq(from=0,to=100,by=10))
	rug(r)
	arrows(90,.7,99.5,.7);text(93.5,.7,"outlier",adj=2,cex=1.25,font=2)
	thetaMM=2*mean(r)
	segments(thetaMM,-1,thetaMM,.5,lwd=3,col=2)
	text(thetaMM+.5,.5,paste("Method of moments estimate = ",round(thetaMM),"%",sep=""),adj=0,font=2,cex=1.5)
	r=.12
	ni=c(round(runif(n=200,min=3,max=100)),2)
	mi=round(ni*r);mi[201]=2
	cbind(ni,mi,mi/ni)
}
