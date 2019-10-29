robias <-
function(Nsim=500000)
{
dump("robias","c:\\StatBook\\robias.r")
par(mfrow=c(1,1),mar=c(4,4,1,1))
ros=seq(from=-.9,to=.9,by=.1)
Nros=length(ros)
ns=c(4,10,20);Lns=length(ns)
bias=matrix(nrow=Nros,ncol=Lns)
for(inn in 1:Lns)
{
	X=matrix(rnorm(Nsim*ns[inn]),ncol=ns[inn])
	X2=rowSums(X^2);xm=rowMeans(X)
	Z=matrix(rnorm(Nsim*ns[inn]),ncol=ns[inn])
	for(i in 1:Nros)
	{
		Y=ros[i]*X+Z*sqrt(1-ros[i]^2)
		Y2=rowSums(Y^2);XY=rowSums(X*Y)
		ym=rowMeans(Y)
		r=(XY-ns[inn]*xm*ym)/sqrt((X2-ns[inn]*xm^2)*(Y2-ns[inn]*ym^2))
		bias[i,inn]=mean(r)-ros[i]
	}	
}
matplot(ros,bias,xlim=c(-1,1),xlab="",ylab="",col=1,type="l",lwd=3)
mtext(side=1,"r",font=5,cex=2,line=2.5)
mtext(side=2,"Bias",cex=1.5,line=2.5)
legend(0.1,.06,paste("n =",ns),lty=1:3,lwd=3,cex=1.5,bg=gray(.92))
}
