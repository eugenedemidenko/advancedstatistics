kernM <-
function(n=20,ss=4)
{
dump("kernM","c:\\StatBook\\kernM.r")
set.seed(ss)
par(mfrow=c(1,2),mar=c(4,4,3,1))
dens=c("gaussian", "epanechnikov","rectangular","triangular");ns=length(dens)
x=rnorm(n=n,mean=1,sd=2)
low=min(x)-1;up=max(x)+1
for(h in c(.25,.5))
{
	plot(x,x,type="n",xlim=c(low,up),ylim=c(0,.4),xlab="Data, x",ylab="Kernel density",main=paste("Bandwidth, h=",h,sep=""))
	rug(x,lwd=3)
	xx=seq(from=low,to=up,length=500)
	for(i in 1:ns) 
		lines(density(x,from=low,to=up,kernel=dens[i],bw=h),col=i,lwd=2)
	legend(3,.4,dens,lty=1,lwd=3,col=1:ns,bg="gray96",cex=1.25)
}
}
