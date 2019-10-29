histN <-
function(mu=0,SD=1)
{
	dump("histN","c:\\StatBook\\histN.r")
	par(mfrow=c(1,1),mar=c(4,4,2,.1))
	x=rnorm(1000,mean=mu,sd=SD)
	hist(x,probability=T,main=paste("mean =",mu, "and SD =",SD),col="gray93")
	xd=seq(from=-4,to=4,length=100)
	lines(xd,dnorm(xd,mean=mu,sd=SD),lwd=3)
	segments(x,rep(-1,1000),x,rep(0.02,1000))
}
