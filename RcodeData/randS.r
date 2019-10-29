randS <-
function(nSim=100000,mu=-1,SD=1,lambda=4)
{
dump("randS","c:\\StatBook\\randS.r")
N=rpois(nSim,lambda)
N=N[N>0]
nSim=length(N)
SX=rep(NA,nSim)
cat("Empirical N mean =",mean(N),", theoretical N mean =", lambda/(1-exp(-lambda)))
varN=(lambda+lambda^2)/(1-exp(-lambda))-(lambda/(1-exp(-lambda)))^2
cat("\nEmpirical var =",var(N),", theoretical var =", varN)
for(i in 1:nSim)
	SX[i]=sum(rnorm(N[i],mean=mu,sd=SD))

print(c(lambda*mu,mean(SX)))
cat("Empirical RS mean =",mean(SX),", theoretical RS mean =", lambda/(1-exp(-lambda))*mu)
SX=SX[order(SX)]

NL=1000
x=seq(from=SX[1],to=SX[nSim],length=NL)
t1=(lambda+lambda^2)/(1-exp(-lambda))
t2=lambda/(1-exp(-lambda))
varRS=SD^2*t2+mu^2*(t1-t2^2)
cat("\nEmpirical RS var =",var(SX),", theoretical RS mean =", varRS)
par(mfrow=c(1,1),mar=c(4,4,1,1))
plot(SX,(1:nSim)/nSim,type="s",lwd=5,xlim=c(t2*mu-3*sqrt(varRS),t2*mu+3*sqrt(varRS)),xlab="",ylab="")
mtext(side=1,"s",cex=1.5,line=2.75)
mtext(side=2,"Probability, cdf",cex=1.5,line=2.5)
lines(x,pnorm(x,mean=t2*mu,sd=sqrt(varRS)),lty=2,lwd=3)
cdfT=rep(NA,NL)
n=1:100;pr=dpois(n,lambda)
for(i in 1:NL) cdfT[i]=sum(pnorm(x[i],mean=mu*n,sd=SD*sqrt(n))*pr)/(1-exp(-lambda))
lines(x,cdfT,col="white")
legend(t2*mu-3*sqrt(varRS),1,c("Empirical","Theoretical","Naive"),lty=c(1,1,2),lwd=c(3,1,3),bg=gray(.9),cex=1.5)
text(0,.1,paste("m=",mu,", s=",SD,", l=",lambda,sep=""),font=5,cex=2)
}
