powsim <-
function(job=1,n=10,m=5,nSim=100000,mu0=1,SD=1.5,alpha=.05,NP=100,NPSIM=21)
{
dump("powsim","c:\\StatBook\\powsim.r")

if(job==1) #one-sample t-test
{
	ta=qt(1-alpha/2,df=n-1)
	mu=seq(from=mu0-2*SD,to=mu0+2*SD,length=NP)
	NCP=(mu-mu0)*sqrt(n)/SD
	#powTH=pt(-ta,df=n-1,ncp=NCP)+1-pt(ta,df=n-1,ncp=NCP) #produces warnings
	powTH=pt(-ta,df=n-1,ncp=NCP)+pt(ta,df=n-1,ncp=NCP,lower.tail=F) #precision computation
	par(mfrow=c(1,1),mar=c(4,4,3,1))
	plot(mu,powTH,ylim=c(0,1),type="l",lwd=3,xlab="",ylab="")
	mtext(side=1,"m",font=5,cex=1.5,line=2.75)
	mtext(side=2,"Power function",cex=1.5,line=2.75)
	muSIM=seq(from=mu0-2*SD,to=mu0+2*SD,length=NPSIM)
	powSIM=rep(NA,NPSIM)
	for(i in 1:NPSIM)
	{
		Y=matrix(rnorm(nSim*n,mean=muSIM[i],sd=SD),ncol=n)
		avY=rowMeans(Y)
		sigma.hat=sqrt(rowSums((Y-avY)^2)/(n-1))
		Ti=sqrt(n)*(avY-mu0)/sigma.hat
		powSIM[i]=mean(abs(Ti)>ta)
	}
	points(muSIM,powSIM,cex=1.5)
	segments(-100,alpha,100,alpha,col=2)
	paste("n=",n,", SD=",SD,", mu0=",mu0,", alpha=", alpha,sep="") 
	title(paste("n=",n,", SD=",SD,", mu0=",mu0,", alpha=", alpha,sep="")) 
}
if(job==2) #two-sample t-test
{
	ta=qt(1-alpha/2,df=n+m-2)
	DELTA=seq(from=-3*SD,to=3*SD,length=NP)
	mu0=rnorm(1);mu1=mu0+DELTA
	c.efs=DELTA/SD # Cohen effect size
	delta=c.efs/sqrt(1/n+1/m)
	powTH=pt(-ta,df=n+m-2,ncp=delta)+pt(ta,df=n+m-2,ncp=delta,lower.tail=F)
	par(mfrow=c(1,1),mar=c(4,4,3,1))
	plot(DELTA,powTH,ylim=c(0,1),type="l",lwd=3,xlab="",ylab="")
	title(paste("n=",n,", m=",m,", alpha=",alpha,", SD=",SD,", nSim=",nSim,sep="")) 
	mtext(side=1,"D",font=5,cex=1.5,line=2.75)
	mtext(side=2,"Power function",cex=1.5,line=2.75)
	
	DELTAsim=seq(from=-3*SD,to=3*SD,length=NPSIM)
	powSIM=rep(NA,NPSIM)
	for(i in 1:NPSIM)
	{
		X=matrix(rnorm(nSim*n,mean=mu0,sd=SD),ncol=n)
		avX=rowMeans(X);S2X=rowSums((X-avX)^2)
		Y=matrix(rnorm(nSim*m,mean=mu0+DELTAsim[i],sd=SD),ncol=m)
		avY=rowMeans(Y);S2Y=rowSums((Y-avY)^2)
		sigma.hat=sqrt((S2X+S2Y)/(n+m-2))
		Ti=(avX-avY)/sigma.hat/sqrt(1/n+1/m)
		powSIM[i]=mean(abs(Ti)>ta)
	}
	points(DELTAsim,powSIM,cex=1.5)
	segments(-6*SD,alpha,6*SD,alpha,col=2)
	
	#optimal design
	ta=qt(1-0.05/2,df=100-2)
	c.grid=seq(from=0,to=7*SD,length=1000)
	coef=1/sqrt(2/50)
	powTH=pt(-ta,df=100-2,ncp=coef*c.grid)+pt(ta,df=100-2,ncp=coef*c.grid,lower.tail=F)
	a=abs(powTH-.8)
	c.min=c.grid[a==min(a)]
	cat("Minimum detectable effect size, c.min=",c.min,"\n")
	
}
}
