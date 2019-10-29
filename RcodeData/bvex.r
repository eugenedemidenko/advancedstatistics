bvex <-
function(job=1,n1=30,n2=50,hx=.75,hy=.75,Nx=70,Ny=60,ss=5)
{
dump("bvex","c:\\StatBook\\bvex.r")
set.seed(ss)
x1=rnorm(n1,mean=-1,sd=2);y1=rnorm(n1,mean=1,sd=1)
yx1=cbind(y1,x1)
x2=rnorm(n2,mean=1,sd=1);y2=rnorm(n2,mean=-1,sd=3)
yx2=cbind(y2,x2)
yx=rbind(yx1,yx2)
n=nrow(yx)
x=seq(from=-6,to=5,length=Nx)
y=seq(from=-7,to=5,length=Ny)
fxy=bvn.density.my(x.data=yx[,2],y.data=yx[,1],x=x,y=y,hx=hx,hy=hy)
if(job==1)
{
	par(mfrow=c(1,1),mar=c(4,4,3,1))
	image(x,y,fxy,col=gray((0:255)/255),main=paste("Image kernel density and contours with",n1,"+",n2,"data points"))
	contour(x,y,fxy,add=T,col=3)
	points(yx[,2],yx[,1],col=2,pch=16)
	ro=round(cor(yx[,2],yx[,1]),2)
	text(4,-6,paste("ro=",ro,sep=""),col="white",cex=1.25)
	
	condmSE=Eyx(yx=yx,ro=ro,x=x,hx=hx,hy=hy)
	lines(condmSE[,1],condmSE[,2],col=4,lwd=3)
	lines(condmSE[,1],condmSE[,2]+condmSE[,3],col=4,lwd=3,lty=2)
	lines(condmSE[,1],condmSE[,2]-condmSE[,3],col=4,lwd=3,lty=2)
	legend(-5.8,-5,c("Regression","Regression +/-SE"),col=4,lty=1:2,lwd=3,bg="white")
}
if(job==2)
{
	par(mfrow=c(1,1),mar=rep(0,4))
	fxyOR=bvn.density.my(x.data=yx[,2],y.data=yx[,1],x=x,y=y,hx=hx,hy=hy)
	kd2=persp(x,y,fxyOR,theta=60,phi=70,r=100,box=F,col=gray(.98),shade=.7,ltheta = -15, lphi = 100)
}
if(job==3)
{
	for(i in 1:360)
	{
		ic=as.character(i)
		if(i<10) ic=paste("00",i,sep="")
		if(i>=10 & i<100) ic=paste("0",i,sep="")
		png(paste("c:\\StatBook\\bvex.movie\\bvex3_",ic,".png",sep=""),width = 1000, height = 1000)
		par(mfrow=c(1,1),mar=rep(0,4))
		fxyOR=bvn.density.my(x.data=yx[,2],y.data=yx[,1],x=x,y=y,hx=hx,hy=hy)
		kd2=persp(x,y,fxyOR,theta=i,phi=70,r=100,box=F,col=gray(.98),shade=.7,ltheta = -15, lphi = 100)
		dev.off()
	}
}

}
