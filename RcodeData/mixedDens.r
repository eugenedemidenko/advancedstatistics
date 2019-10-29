mixedDens <-
function(mu.m=70,sd.m=4,mu.w=64,sd.w=3,p=.5,h=68)
{
dump("mixedDens","c:\\StatBook\\mixedDens.r")
par(mfrow=c(1,1),mar=c(4,4,1,1))
x=seq(from=64-3*sd.w,to=mu.m+3*sd.m,length=100)
f1.m=p*dnorm(x,mean=mu.m,sd=sd.m)
f2.w=(1-p)*dnorm(x,mean=mu.w,sd=sd.w)
f.marg=f1.m+f2.w
matplot(x,cbind(f1.m,f2.w,f.marg),col=1,lty=c(2,3,1),lwd=c(1,1,3),type="l",xlab="Height (in.)",ylab="Density")
legend(72,.082,c("p*f.man","(1-p)*f.woman","Marginal"),lty=c(2,3,1),lwd=c(1,1,3),bg="gray94",cex=1.5)
segments(h,-1,h,1)
pr.m=dnorm(68,mean=mu.m,sd=sd.m)/(dnorm(68,mean=mu.m,sd=sd.m)+dnorm(68,mean=mu.w,sd=sd.w))
text(68+.1,0.01,paste("Pr(man) =",round(pr.m,2)),adj=0)
}
