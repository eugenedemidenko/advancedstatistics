ttest2pow <-
function(n=4,m=4,nSim=100000,alpha=.05,nmu=51)
{
dump("ttest2pow","c:\\StatBook\\ttest2pow.r")
print("It takes some time...")
powEV=powWE=rep(0,nmu)
taEV=qt(1-alpha/2,df=n+m-2)
eff.size=seq(from=-1,to=1,length=nmu)
par(mfrow=c(1,2),mar=c(3.5,3.5,3,1))
for(isd in 1:2)
{
	if(isd==1) {sdX=1;sdY=1} 
	if(isd==2) {sdX=1;sdY=3} 
	sdALL=sqrt(sdX^2+sdY^2)	
	mus=eff.size*sdALL
	for(i in 1:nmu)
	{
		X=matrix(rnorm(m*nSim,mean=0,sd=sdX),ncol=m)
		avX=rowMeans(X);S2X=rowSums((X-avX)^2)
		Y=matrix(rnorm(n*nSim,mean=mus[i],sd=sdY),ncol=n)
		avY=rowMeans(Y);S2Y=rowSums((Y-avY)^2)
		sigma.hat=sqrt((S2X+S2Y)/(n+m-2))		
		Ti=(avX-avY)/sigma.hat/sqrt(1/n+1/m)		
		powEV[i]=mean(abs(Ti)>taEV)
		
		s2x=S2X/(m-1);s2y=S2Y/(n-1)
		sigma.wave=sqrt(s2x/m+s2y/n)
		nu=sigma.wave^4/(s2x^2/(m-1)/m^2+s2y^2/(n-1)/n^2)
		Ti=(avX-avY)/sigma.wave
		ta=qt(1-alpha/2,df=nu)
		powWE[i]=mean(abs(Ti)>ta)		
	}
	matplot(eff.size,cbind(powEV,powWE),type="l",col=1,lty=1:2,ylim=c(0,.4),lwd=2,xlab="",ylab="")
	if(isd==1) title(paste("Equal variances: sdX = sdY = ",sdX,sep=""))
	if(isd==2) title(paste("Unequal variances: sdX = ",sdX,", sdY = ",sdY,sep=""))
	segments(-100,alpha,100,alpha,lty=3)
	mtext(side=1,"Effect size",cex=1.5,line=2.5)
	mtext(side=2,"Power, probability",cex=1.5,line=2.25)	
	c1=paste("t-test equal variance, df=",n+m-2,sep="")
	c2=paste("Welsh test, mean(df)=",round(mean(nu),1),sep="")
	legend(-.75,.4,c(c1,c2),col=1,lty=1:2,lwd=3,bg="gray90",cex=1.25) 
	text(0,.3,paste("m = ",m,", n = ",n,sep=""),cex=1.5)	
}
}
