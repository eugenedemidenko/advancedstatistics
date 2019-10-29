ranlRest <-
function(Nexp=500,r=.25)
{
dump("ranlRest","c:\\statbook\\ranlRest.r")
par(mfrow=c(1,1),mar=c(.1,.1,2,1))
pr<-0
plot(c(-1,1),c(-1,1),type="n",xlab="",ylab="",axes=F)
lines(x=c(-1,-1,1,1,-1),y=c(-1,1,1,-1,-1),lwd=3)
tt<-seq(from=0,to=2*pi,length=100)
lines(r*cos(tt),r*sin(tt),lwd=5)
x<-c(-1,1)
for(i in 1:Nexp)
{
	c1<--1+2*runif(1)
	c2<--1+2*runif(1)
	fi<-runif(1)*pi
	k<-tan(fi)
	y=c2+k*(x-c1)
	lines(x,y)
	dist2<-(c2-c1*k)^2/(1+k^2)
	if(dist2<r^2) pr<-pr+1
}
mtext(side=3,paste("Nexp=",Nexp,", r=",r,", prob=",pr/Nexp,sep=""),line=.5,cex=1.5,font=2)

}
