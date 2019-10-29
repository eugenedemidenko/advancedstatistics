rws <-
function(job=1,n=10,probx=.5,proby=.5,Tx=n,Ty=n,stepMAX=5000,NSIM=1000)
{
# n=squares size; (Tx,Ty) the point of arrival; stepMAX=max number of steps
dump("rws","c:\\StatBook\\rws.r")
if(job==1)
{
    x=rep(NA,stepMAX);y=rep(NA,stepMAX)
    x[1]=1;y[1]=1
    for(i in 1:stepMAX)
    { 
		ic=as.character(i)
		if(i<10) ic=paste("000",i,sep="")
		if(i>=10 && i<100) ic=paste("00",i,sep="") 
		if(i>=100) ic=paste("0",i,sep="") 
		# The folder StatBook\\rwsplots\\ must exist
		jpeg(paste("c:\\StatBook\\rwsplots\\step_",ic,".jpg",sep=""),quality=100,height=1000,width=1000)
		par(mfrow=c(1,1),mar=c(0,0,2,0))
		plot(1:n,1:n,type="n",xlab="",ylab="",axes=F)		
		for(j in 1:n)
		{
			segments(1,j,n,j,col=gray(.9))
			segments(j,1,j,n,col=gray(.9))
		}
		if(i>2) segments(x[2:i],y[2:i],x[1:(i-1)],y[1:(i-1)],lwd=2)
		points(Tx,Ty,pch=16,col=3,cex=1.75)
		dx=sample(x=c(-1,1),size=1,prob=c(1-probx,probx))
		xnew=min(max(x[i]+dx,1),n)
		dy=sample(x=c(-1,1),size=1,prob=c(1-proby,proby))
		ynew=min(max(y[i]+dy,1),n)
		segments(x[i],y[i],xnew,ynew,lwd=3)
		x[i+1]=xnew;y[i+1]=ynew
		points(x[i+1],y[i+1],col=2,pch=16,cex=1.5) 
		mtext(side=3,paste("Step",i),cex=1.5,font=2,line=.5)
		if(x[i+1]==Tx && y[i+1]==Ty) 
		{
			text(n/2,n/2,"Games's over!",font=2,cex=2,col=2)
			dev.off()
			break
		}
		dev.off()		
     }
}
if(job==2) #simulations
{
	steps.req=rep(NA,NSIM)
    for(isim in 1:NSIM)
    {
		x=1;y=1
		for(i in 1:stepMAX)
		{ 
			dx=sample(x=c(-1,1),size=1,prob=c(1-probx,probx))
			x=min(max(x+dx,1),n)
			dy=sample(x=c(-1,1),size=1,prob=c(1-proby,proby))
			y=min(max(y+dy,1),n)
			if(x==Tx && y==Ty) 
			{
				steps.req[isim]=i
				break
			}
		} 
    }
    return(steps.req)
}

}
