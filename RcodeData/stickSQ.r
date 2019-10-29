stickSQ <-
function(job=1,nSticks=1000,L=1)
{
dump("stickSQ","c:\\StatBook\\stickSQ.r")
if(job==1) #plot
{
	par(mfrow=c(1,1),mar=c(.1,.1,1,.1))
	plot(1,1,type="n",xlim=c(-.5,2.5),ylim=c(-.5,2.5),xlab="",ylab="",axes=F)
	lines(x=c(0,0,2,2,0),y=c(0,2,2,0,0),lwd=3)
	count=0
	for(iexp in 1:nSticks)
	{
		xycent=runif(n=2,min=0,max=2)
		theta=runif(1,min=0,max=pi)
		x0=xycent[1]-L*cos(theta)/2
		y0=xycent[2]-L*sin(theta)/2
		x1=xycent[1]+L*cos(theta)/2
		y1=xycent[2]+L*sin(theta)/2
		segments(x0,y0,x1,y1)
		if(x0>0 & x0<2 & y0>0 & y0<2 & x1>0 & x1<2 & y1>0 & y1<2) 
		{
			count=count+1
			segments(x0,y0,x1,y1,col=2)
		}
	}
	mtext(side=3,paste("Number of sticks=",nSticks,", stick length=",L,"\nProbability inside the 2x2 square=",count/nSticks,sep=""),line=-1,font=2)
}
if(job==2) #vectorized simulations
{
	xycent=matrix(runif(2*nSticks),ncol=2)
	theta=runif(nSticks)*pi
	x0=xycent[,1]-L*cos(theta)/2
	y0=xycent[,2]-L*sin(theta)/2
	x1=xycent[,1]+L*cos(theta)/2
	y1=xycent[,2]+L*sin(theta)/2
	prob=mean(x0>0&x0<2&y0>0&y0<2&x1>0&x1<2&y1>0&y1<2)
	return(prob)
}	
}
