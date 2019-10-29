qtrpow <-
function(pow=.8,alpha=.05,a=0.01)
{
dump("qtrpow","c:\\StatBook\\qtrpow.r")

xtx=matrix(ncol=3,nrow=3)
n1=4;n2=50
P=matrix(nrow=n2-n1+1,ncol=3)
sigmas=c(1,2,3);need=rep(NA,3)
for(n in n1:n2)
{
	qF=qf(1-alpha,df1=1,df2=n-3)
	xtx[1,1]=n;xtx[1,2]=xtx[2,1]=n*(n+1)/2;xtx[1,3]=xtx[3,1]=xtx[2,2]=n*(2*n^2+3*n+1)/6
	xtx[2,3]=xtx[3,2]=n^2*(n+1)^2/4;xtx[3,3]=n*(6*n^4+15*n^3+10*n^2-1)/30
	for(i in 1:3)
	{
		lambda=a^2/solve(xtx)[3,3]/sigmas[i]^2
		P[n-3,i]=1-pf(qF,df1=1,df2=n-3,ncp=lambda)
	}		
}
par(mfrow=c(1,1),mar=c(4,4,1,1))
matplot(n1:n2,P,ylim=c(0,1),type="l",col=1,lwd=4,xlab="",ylab="")
for(i in 1:3)
{
	need[i]=min((n1:n2)[P[,i]>=pow])
	lines(x=c(-1,need[i],need[i]),y=c(pow,pow,-1))
}
mtext(side=1,"n",cex=1.5,line=2.75)
mtext(side=2,"Power of detection, probability",cex=1.5,line=2.75)
legend(4,1,c("sigma = 1","sigma = 2","sigma = 3"),lty=1:3,lwd=3,cex=2,bg="gray95")
out=data.frame(cbind(sigmas,need))
out
}
