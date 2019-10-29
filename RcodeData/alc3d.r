alc3d <-
function()
{
dump("alc3d","c:\\StatBook\\alc3d.r")
da=read.csv("c:\\StatBook\\alcoholUSA.csv")
nst=nrow(da)
for(theta in 1:360)
{
	thCH=as.character(theta)
	if(theta<10) thCH=paste("00",thCH,sep="")
	if(theta>=10 & theta<100) thCH=paste("0",thCH,sep="")
	jpeg(paste("c:\\StatBook\\alc3d\\alc3d",thCH,".jpg",sep=""),width=1000,height=1000,quality=100)
	par(mfrow=c(1,1),mar=c(2,2,2,2),cex.main=2,cex.lab=2)
	op=persp(x=range(da$Beer),y=range(da$Wine),z=matrix(ncol=2,nrow=2),zlim=range(da$Spirits),xlab="Beer",ylab="Wine",zlab="Spirits",theta=theta,phi=30,r=1000,ticktype="detailed",nticks=9,main=paste("Alcohol consumption per capita in US, theta=",theta,sep=""))	
	p3=trans3d(x=da$Beer, y=da$Wine, z=da$Spirits, pmat=op)
	lines(p3$x,p3$y,lwd=3)
	points(p3$x,p3$y,pch=16,cex=1.25,col=2)
	text(p3$x,p3$y+.00005,da$Year)
	p2=trans3d(x=da$Beer, y=da$Wine, z=rep(min(da$Spirits),nst),pmat=op)
	segments(p3$x,p3$y,p2$x,p2$y)
	lines(p2$x,p2$y,col=4)
	points(p2$x,p2$y,pch=16,cex=1,col=3)
	dev.off()
}

}
