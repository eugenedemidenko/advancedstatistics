gampois <-
function(lambda=5,alpha=300,nSim=100000)
{
dump("gampois","c:\\StatBook\\gampois.r")
par(mfrow=c(1,2),mar=c(4,4,1,1))
Nmax=24*60 # minutes in a day
talpha=rep(NA,nSim) #array of times
ind=1:Nmax
for(i in 1:nSim)
{
    x=rpois(Nmax,lambda=lambda)
    xsum=cumsum(x) #cumulative sum
    talpha[i]=max(ind[xsum<=alpha]) #time when alpha events occur 
}
talpha=talpha[order(talpha)]
plot(talpha,(1:nSim)/nSim,lwd=2,type="s",xlab="",ylab="")
mtext(side=1,line=2.75,paste("Time until",alpha,"events happen (min)"),cex=1.5)
mtext(side=2,line=2.5,"Probability, cdf",cex=1.5)
x=seq(from=0,to=max(talpha),length=100)
lines(x,pgamma(x,rate=lambda,shape=alpha),lwd=3,lty=2)
legend(58,.15,c("Empirical/simulated cdf","Gamma approximation cdf"),lty=1:2,lwd=3,bg=gray(.9),cex=1.25)
text(50,.85,paste("l =",lambda),font=5,cex=2)


k=seq(from=0,to=3*lambda,length=1000)
cdfP=ppois(k,lambda=lambda)
cdfG=pgamma(lambda,shape=k+1,rate=1,lower.tail=F)
cdfG5=pgamma(lambda+.5,shape=k+1,rate=1,lower.tail=F)
matplot(k,cbind(cdfP,cdfG,cdfG5),type="l",lwd=2,xlab="",ylab="")
mtext(side=1,line=2.75,"k",cex=1.5)
mtext(side=2,line=2.5,"Probability, cdf",cex=1.5)
text(2*lambda,.2,paste("l =",lambda),font=5,cex=2)
legend(0,1,c("Poisson","Gamma","Gamma + 0.5"),lty=1:3,col=1:3,lwd=2,bg=gray(.9),cex=1.25)

}
