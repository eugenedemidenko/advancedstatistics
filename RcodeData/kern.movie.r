kern.movie <-
function(n=20,ss=4)
{
dump("kern.movie","c:\\StatBook\\kern.movie.r")
set.seed(ss)
dens=c("gaussian", "epanechnikov","rectangular","triangular");ns=length(dens)
hs=seq(from=.1,to=.5,by=.001);nhs=length(hs)
x=rnorm(n=n,mean=1,sd=2)
low=min(x)-1;up=max(x)+1
xx=seq(from=low,to=up,length=500)
for(ih in 1:nhs)
{
	cih=as.character(ih)
	if(ih<10) cih=paste("000",cih,sep="")
	if(ih<100 & ih>9) cih=paste("00",cih,sep="")
	if(ih<1000 & ih>99) cih=paste("0",cih,sep="")
	jpeg(paste("c:\\StatBook\\kern.movie\\h",cih,".jpg",sep=""),width=1000,height=700)
	par(mfrow=c(1,1),mar=c(4,4,3,1))
	plot(x,x,type="n",xlim=c(low,up),ylim=c(0,.5),xlab="Data, x",ylab="Kernel density",main=paste("Bandwidth, h=",hs[ih],sep=""))
	rug(x,lwd=3)	
	for(i in 1:ns) 
		lines(density(x,from=low,to=up,kernel=dens[i],bw=hs[ih]),col=i,lwd=2)
	legend(3,.5,dens,lty=1,lwd=3,col=1:ns,bg="gray96",cex=1.5)
	dev.off()
}
}
