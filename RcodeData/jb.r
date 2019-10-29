jb <-
function(job=1,v=.1,delta=.9,T=60,tau=5,nSim=100000)
{
dump("jb","c:\\StatBook\\jb.r")
if(job==1)
{
	par(mfrow=c(1,2),mar=c(4,4,3,1))
	for(ig in 1:2)
	{
		flag=0;tx=""
		plot(1:T,1:T,ylim=c(1,T*1.01),type="n",xlab="",ylab="")
		legend(1,T,c("James Bond","Enemy"),col=1:2,lty=1:2,lwd=3,bg="gray90",cex=1.25)
		mtext(side=1,"Time, min",line=2.75,cex=1.25)
		mtext(side=2,"Distance, meters",line=2.75,cex=1.25)
		dJB=0
		for(i in 1:tau)
		{
			d1=runif(1,min=1-delta,max=1+delta)
			segments(i,dJB,i+1,dJB+d1,lwd=3)
			dJB=dJB+d1		
		}
		dEN=0
		for(i in (tau+1):T)
		{
			d1=runif(1,min=1-delta,max=1+delta)
			segments(i,dJB,i+1,dJB+d1,lwd=3)
			dJB=dJB+d1		
		
			d1=runif(1,min=1+v-delta,max=1+v+delta)
			segments(i,dEN,i+1,dEN+d1,lwd=3,col=2,lty=2)
			dEN=dEN+d1				
			if(dEN>=dJB) 
			{
				flag=1
				break
			}
		}
		if(!flag) tx="not"
		title(paste("James Bond has",tx,"been caught"))
	}
}	
if(job==2) #vectorized simulations
{
	XJB=matrix(runif(nSim*T,min=1-delta,max=1+delta),ncol=T)
	XJB=apply(XJB,1,cumsum)
	XEN=matrix(runif(nSim*(T-tau),min=1+v-delta,max=1+v+delta),ncol=T-tau)
	XEN=cbind(matrix(0,ncol=tau,nrow=nSim),XEN)
	XEN=apply(XEN,1,cumsum)
	g=colSums(XJB>XEN)
	pr.sim=mean(g==T)
	num=T-(T-tau)*(1+v)
	den=delta/sqrt(3)*sqrt(2*T-tau)
	cat("Probability that JB will not be caught from simulations =",pr.sim)
	pr.appr=pnorm(num/den)
	cat("\nProbability normal approximation =",pr.appr,"\n")	
	
}
if(job==3) # for loop simulations
{
	prnot=rep(1,nSim)
	for(isim in 1:nSim)
	{
		Dt=sum(runif(tau,min=1-delta,max=1+delta))
		De=0
		for(j in tau:T)
		{
			Dt=Dt+runif(1,min=1-delta,max=1+delta)
			De=De+runif(1,min=1-delta+v,max=1+delta+v)
			if(De>=Dt) 
			{
				prnot[isim]=0
				break			
			}
		}	
	
	}
	cat("Probability that JB will not be caught from for loop simulations =",mean(prnot))

}
}
