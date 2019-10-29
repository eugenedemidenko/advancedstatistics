robpol <-
function()
{
dump("robpol","c:\\StatBook\\robpol.r")
par(mfrow=c(1,1),mar=c(3.5,3.5,3,1),cex.main=1.25)
x=c(3.5,5.1,10.2,3.5,11.3,8.2,4.1,5.2,8.2,4.9,7.1,8.8,12.5,3.9,2.8,1.7,16.3,12.3,2.6,8.1,9.9,10.3,6.9,7.1,1.9,12.3,9.4,8.9,6.4,11.3)
n=length(x)
lMM=1/mean(x)
prEXP=exp(-lMM*5)
par(mfrow=c(1,2))
x=x[order(x)]
Fv=(1:n)/n
tq=-1/lMM*log(1-Fv)
plot(tq,x,ylim=c(0,max(tq[tq<Inf])),xlab="",ylab="",main="q-q plot for exponential distribution")
mtext(side=1,"Theoretical quantile",cex=1.5,line=2.25)
mtext(side=2,"Empirical quantile",cex=1.5,line=2.25)
plot(x,Fv,type="s",xlab="",ylab="",main="Empirical and theoretical cdfs",col=3,lty=2)
mtext(side=1,"Police arrival, min",cex=1.5,line=2.25)
mtext(side=2,"Probability",cex=1.5,line=2.25)
x5=abs(x-5)
prEMP=max(1-Fv[x5==min(x5)])
xx=seq(from=min(x),to=max(x),length=100)
lines(xx,1-exp(-xx*lMM),col=2)
segments(5,1-prEMP,5,1,col=3,lty=2,lwd=3)
segments(5.2,1-prEXP,5.2,1,lwd=3,col=2)
segments(min(x),0,12.5,1)
text(15.5,.15,paste("Probability that robber will get away:\n",round(prEXP,2),", based on exponential distribution\n",round(prEMP,2),", based on empirical distribution",sep=""),adj=1,font=2)
legend(10,.5,c("Uniform cdf","Exponential cdf","Empirical cdf"),col=1:3,lwd=2,lty=c(1,1,2),cex=1.25,bg="gray90")
#Estimation the probability that time X~R(alpha,beta)
me=mean(x);SD=sd(x)
alpha.MM=me-sqrt(3)*SD
beta.MM=me+sqrt(3)*SD
pr.MM=(beta.MM-5)/(beta.MM-alpha.MM)
cat("alpha.MM=",alpha.MM,", beta.MM =",beta.MM)
cat("\nProb estim if X~R(alpha,beta) =",pr.MM,"\n")
}
