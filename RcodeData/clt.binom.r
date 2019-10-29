clt.binom <-
function(p=.1,N=200)
{
dump("clt.binom","c:\\StatBook\\clt.binom.r")

for(n in 2:N)
{	
	ib=as.character(n)
	if(n<10) ib=paste("000",n,sep="")
	if(n>=10 & n<100) ib=paste("00",n,sep="")
	if(n>=100 & n<1000) ib=paste("0",n,sep="")		
	png(paste("c:\\StatBook\\clt.binom\\binom",ib,"png",sep=""),width=1500,height=1000)
	par(mfrow=c(1,1),cex.main=2,cex.lab=1.5,cex.axis=1.5)
	x=0:n
	seb=sqrt(p*(1-p)*n)
	plot((x-p*n)/seb,dbinom(x,size=n,prob=p),cex=1.5,xlim=c(-3,3),lwd=10,type="h",xlab="Normalized binomial counts",ylab="Probability/density",main=paste("Illustration of CLT: normalized binomial distribution with n =",n," and prob=",p))
	lines((x-p*n)/seb,dbinom(x,size=n,prob=p),col=2,lwd=6)
	dev.off()
}



}
