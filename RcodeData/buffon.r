buffon <-
function(job=1,L=1,D=2,nExp=1000)
{
dump("buffon","c:\\StatBook\\buffon.r")
if(job==1)
{
	par(mfrow=c(1,1),mar=c(0,0,2,0))
	yD=D*seq(from=0,to=1,by=1)
	plot(yD,yD,ylim=D*c(-.2,1.2),xlim=c(-1.2*L,1.2*L),type="n",xlab="",ylab="",axes=F)
	for(i in 1:length(yD))
		segments(-L,yD[i],L,yD[i],lwd=3)
	ninter=0
	for(ix in 1:nExp)
	{
		nc=D*runif(1);x=(runif(1)-.5)*L
		ang=pi*runif(1)
		y0=-L/2*sin(ang)+nc
		y1=L/2*sin(ang)+nc
		segments(x0=x-L/2*cos(ang),y0=y0,x1=x+L/2*cos(ang),y1=y1,col=2,lwd=1)	
		if(y0<0 | y0>D | y1<0 | y1>D) ninter=ninter+1
	}
	pr=ninter/nExp
	print(c(pr,2*L/pi/D))
	mtext(side=3,paste(nExp," Buffon's needle experiments\n",ninter," needle intersections, probability=",round(pr,2),"\ntheoretical probability=",round(2*L/pi/D,3),sep=""),line=-1)
	mtext(side=1,paste("D=",D,", L=",L,sep=""),line=-1.5)

}
if(job==2)
{
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	L=seq(from=.1,to=3,length=50)
	nr=length(L)
	pr=rep(NA,nr)
	for(i in 1:nr)
	{
		nc=runif(nExp)
		ang=pi*runif(nExp)
		y0=-L[i]/2*sin(ang)+nc
		y1=L[i]/2*sin(ang)+nc
		pr[i]=mean(y0<0 | y0>1 | y1<0 | y1>1)			
	}
	plot(L,pr,xlab="L/D",ylab="Probability")
	L1=seq(from=.1,to=1,length=50)
	pTH1=2/pi*L1
	L2=seq(from=1,to=3,length=50)
	pTH2=2/pi*acos(1/L2)+2/pi*L2*(1-sqrt(1-(1/L2^2)))
	lines(c(L1,L2),c(pTH1,pTH2))
	legend(1,.3,c("Simulations","Theoretical probability"),lty=c(NA,1),pch=c(1,NA),cex=1.25) 
}
}
