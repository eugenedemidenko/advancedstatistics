nhcancer <-
function(job=1,hx=10000,hy=10000)
{
dump("nhcancer","c:\\StatBook\\nhcancer.r")
if(job==1)
{
	par(mfrow=c(1,2),mar=c(0,0,3,0))
	xytown=read.csv("c:\\StatBook\\NHtowns.csv",stringsAsFactors=F)
	ntowns=nrow(xytown)
	maxxy=1138
	x.town=xytown[,1:maxxy];xr=range(x.town[!is.na(x.town)])
	y.town=xytown[,(1+maxxy):(2*maxxy)];yr=range(y.town[!is.na(y.town)])
	plot(xr,yr,type="n",xlab="",ylab ="",axes=F,main="Lung cancer locations")
	for(i in 1:ntowns)
	{
		xi=x.town[i,];xi=xi[!is.na(xi)]
		yi=y.town[i,];yi=yi[!is.na(yi)]
		lines(xi,yi,col=2,lwd=2)
	}

	xyc=read.csv("c:\\StatBook\\xyNHcancer.csv",stringsAsFactors=F)
	points(xyc$x,xyc$y,pch=16,cex=.5)

	plot(xr,yr,type="n",xlab="",ylab ="",axes=F,main="Random residents' locations")
	for(i in 1:ntowns)
	{
		xi=x.town[i,];xi=xi[!is.na(xi)]
		yi=y.town[i,];yi=yi[!is.na(yi)]
		lines(xi,yi,col=2,lwd=2)
	}

	xyp=read.csv("c:\\StatBook\\xyNHpopulation.csv",stringsAsFactors=F)
	points(xyp$x,xyp$y,pch=16,cex=.1)
}
if(job==2)
{
	par(mfrow=c(1,2),mar=c(0,0,3,0))
	n.2dens=100
	xytown=read.csv("c:\\StatBook\\NHtowns.csv",stringsAsFactors=F)
	ntowns=nrow(xytown)
	maxxy=1138
	x.town=xytown[,1:maxxy];xr=range(x.town[!is.na(x.town)])
	y.town=xytown[,(1+maxxy):(2*maxxy)];yr=range(y.town[!is.na(y.town)])
	x=seq(from=xr[1]-.1*(xr[2]-xr[1]),to=xr[2]+.1*(xr[2]-xr[1]),length=n.2dens)
	y=seq(from=yr[1]-.1*(yr[2]-yr[1]),to=yr[2]+.1*(yr[2]-yr[1]),length=n.2dens)
	
	xy=as.matrix(read.csv("c:\\StatBook\\xyNHcancer.csv",stringsAsFactors=F),ncol=2)
	fxy=bvn.density.my(x.data=xy[,1],y.data=xy[,2],x=x,y=y,hx=hx,hy=hy)
	image(x,y,fxy,col=gray((0:255)/255),main="Lung cancer density")
	points(xy[,1],xy[,2],col=2,pch=16,cex=.5)		
	contour(x,y,fxy,add=T,col="yellow",lwd=2)
	for(i in 1:ntowns)
	{
		xi=x.town[i,];xi=xi[!is.na(xi)]
		yi=y.town[i,];yi=yi[!is.na(yi)]
		lines(xi,yi,col="white")
	}
		
	xy=read.csv("c:\\StatBook\\xyNHpopulation.csv",stringsAsFactors=F)
	fxy=bvn.density.my(x.data=xy[,1],y.data=xy[,2],x=x,y=y,hx=hx,hy=hy)
	image(x,y,fxy,col=gray((0:255)/255),main="Population density")
	points(xy[,1],xy[,2],col=2,pch=16,cex=.075)		
	contour(x,y,fxy,add=T,col="yellow",lwd=2)	
	for(i in 1:ntowns)
	{
		xi=x.town[i,];xi=xi[!is.na(xi)]
		yi=y.town[i,];yi=yi[!is.na(yi)]
		lines(xi,yi,col="white")
	}	
}
if(job==3)
{
	par(mfrow=c(1,2),mar=c(0,0,0,0))
	n.2dens=100
	xytown=read.csv("c:\\StatBook\\NHtowns.csv",stringsAsFactors=F)
	ntowns=nrow(xytown)
	maxxy=1138
	x.town=xytown[,1:maxxy];xr=range(x.town[!is.na(x.town)])
	y.town=xytown[,(1+maxxy):(2*maxxy)];yr=range(y.town[!is.na(y.town)])
	
	x=seq(from=xr[1]-.1*(xr[2]-xr[1]),to=xr[2]+.1*(xr[2]-xr[1]),length=n.2dens)
	y=seq(from=yr[1]-.1*(yr[2]-yr[1]),to=yr[2]+.1*(yr[2]-yr[1]),length=n.2dens)
	
	xy=as.matrix(read.csv("c:\\StatBook\\xyNHcancer.csv",stringsAsFactors=F),ncol=2)
	fxyCancer=bvn.density.my(x.data=xy[,1],y.data=xy[,2],x=x,y=y,hx=hx,hy=hy)
	xy=read.csv("c:\\StatBook\\xyNHpopulation.csv",stringsAsFactors=F)
	fxyPop=bvn.density.my(x.data=xy[,1],y.data=xy[,2],x=x,y=y,hx=hx,hy=hy)
	cancRate=fxyCancer/(fxyPop+.1)
	image(x,y,cancRate,col=gray((0:255)/255))
	xe=x%*%t(rep(1,length(y)));ye=rep(1,length(x))%*%t(y)
	x.max=unique(xe[cancRate==max(cancRate)])
	y.max=unique(ye[cancRate==max(cancRate)])
	contour(x,y,cancRate,add=T,col="yellow",lwd=2)	
	for(i in 1:ntowns)
	{
		xi=x.town[i,];xi=xi[!is.na(xi)]
		yi=y.town[i,];yi=yi[!is.na(yi)]
		lines(xi,yi,col="red")		
	}		
	rX=range(x.town,na.rm=T);rY=range(y.town,na.rm=T)
	points(x.max,y.max,pch=16)
	library(jpeg)
	nh.map=readJPEG("c:\\StatBook\\NHmaxcanc.jpg",native=T)
	plot(x,y,type="n",xlab="",ylab="",axes=T)
	rasterImage(nh.map,min(x),min(y),max(x),max(y))	
}
if(job==4)
{
	hx=3000;hy=3000
	n.2dens=100
	xytown=read.csv("c:\\StatBook\\NHtowns.csv",stringsAsFactors=F)
	ntowns=nrow(xytown)
	maxxy=1138
	x.town=xytown[,1:maxxy];xr=range(x.town[!is.na(x.town)])
	y.town=xytown[,(1+maxxy):(2*maxxy)];yr=range(y.town[!is.na(y.town)])
	
	x=seq(from=xr[1]-.1*(xr[2]-xr[1]),to=xr[2]+.1*(xr[2]-xr[1]),length=n.2dens)
	y=seq(from=yr[1]-.1*(yr[2]-yr[1]),to=yr[2]+.1*(yr[2]-yr[1]),length=n.2dens)
	
	xy=as.matrix(read.csv("c:\\StatBook\\xyNHcancer.csv",stringsAsFactors=F),ncol=2)
	fxyCancer=bvn.density.my(x.data=xy[,1],y.data=xy[,2],x=x,y=y,hx=hx,hy=hy)
	fxyCancer[fxyCancer<10^-12]=NA
	
	for(theta360 in 1:360)
	{
		ic=as.character(i)
		if(i<10) ic=paste("00",i,sep="")
		if(i<100 & i>9) ic=paste("0",i,sep="")
	#	jpeg(paste("c\\StatBook\\NHcancer360\\NH",ic,".jpg",sep=""),height=1000,width=600,quality=100)
		persp(x,y,fxyCancer,theta=theta360,phi=80,r=10000,box=F,d=100,xlab="",ylab="")
		contour(x,y,fxyCancer,add=T)
	#	dev.off()
	}

}
}
