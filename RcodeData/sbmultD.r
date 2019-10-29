sbmultD <-
function(n=50,phi=80)
{
dump("sbmultD","c:\\StatBook\\sbmultD.r")
par(mfrow=c(1,2),mar=c(0,0,0,0))
x=seq(from=-4,to=4,length=n)
xn=rep(x,n);yn=rep(x,each=n)
fd=matrix(dnorm(xn+yn)*dnorm(yn),nrow=n,ncol=n)
for(theta in c(15,60))
{
	persp(x,x,fd,theta=theta,phi=phi,r=100,zlab="",xlab="x",ylab="y",box=F,axes=F)
	mtext(side=3,paste("theta=",theta,sep=""),cex=1.5,line=-4)
}

}
