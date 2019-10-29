oilspill <-
function(job=1,dist=3000,nSim=150)
{
dump("oilspill","c:\\StatBook\\oilspill.r")
if(job==1) #create the graph
{
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	plot(0:dist,0:dist,type="n",xlab="",ylab="")
	mtext(side=1,"X-location",line=2.6,cex=1.25)
	mtext(side=2,"Y-location",line=2.6,cex=1.25)
	polygon(x=c(0,0,dist,0),y=c(dist,dist,0,0),col="lightblue")
	n=24
	pr=0
	for(isim in 1:nSim)
	{
		xc=xnew=0;yc=ynew=0
		for(i in 1:n)
		{
			ist=sample(c(0,1),size=1,prob=c(2/5,3/5))
			if(ist) xnew=xc+rexp(1,rate=1/150)
			ist=sample(c(0,1),size=1,prob=c(1/2,1/2))
			if(ist) ynew=yc+rexp(1,rate=1/100)
			if(xnew<3000 & ynew<3000) segments(xc,yc,xnew,ynew,col=2) else break
			xc=xnew;yc=ynew
		}
		if(xnew+ynew<dist) pr=pr+1
		dist0=dist/(1+5/9)	
	}
	polygon(x=c(0,dist,dist,0),y=c(dist,dist,0,dist),col="gray90")
	lines(0:dist,dist-0:dist,lwd=3)
	lines((0:dist0)*5/9,col=3,lty=2,lwd=3)
	text(2000,2500,paste("Simulation probability =",round(pr/nSim,3)),cex=1.25,font=2)
	SDshore=sqrt(26400*n/2)
	tt=seq(from=-3*SDshore,to=3*SDshore,length=100)
	ptt=1000000*dnorm(tt,mean=0,sd=SDshore);mptt=max(ptt)
	lines((ptt-tt)/sqrt(2)+1929,(ptt+tt)/sqrt(2)+1071,col=3,lwd=3,lty=2)
	segments(1929,1071,mptt/sqrt(2)+1929,mptt/sqrt(2)+1071,col=3)
}
if(job==2) #vectorized simulations
{
	n=24
	U=matrix(sample(c(0,1),size=n*nSim,rep=T,prob=c(2/5,3/5)),ncol=n)
	X=matrix(rexp(n*nSim,rate=1/150),ncol=n)
	Dx=rowSums(U*X)
	V=matrix(sample(c(0,1),size=n*nSim,rep=T,prob=c(1/2,1/2)),ncol=n)
	Y=matrix(rexp(n*nSim,rate=1/100),ncol=n)
	Dy=rowSums(V*Y)
	prSIM=mean(Dx+Dy<3000)
	cat("Simulated probability not to reach the shore =",prSIM,"\n")
	prNORM=pnorm((3000-n*140)/sqrt(26400*n))
	cat("Normal approximation probability not to reach the shore =",prNORM,"\n")
}

}
