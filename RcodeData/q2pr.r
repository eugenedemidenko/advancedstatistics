q2pr <-
function(NSIG=100)
{
dump("q2pr","c:\\StatBook\\q2pr.r")
integr=function(y1,beta,sigma)
dnorm((y1-beta)/sigma)*pnorm((beta^2-3/4^(1/3)*abs(y1)^(2/3)-.5)/sigma)/sigma
par(mfrow=c(1,1),mar=c(4,4,1,1))
beta=c(.5,1)
ss=seq(from=.25,to=1,length=NSIG)
pr=matrix(ncol=2,nrow=NSIG)
for(ib in 1:2)
for(i in 1:NSIG)
pr[i,ib]=integrate(integr,beta=beta[ib],sigma=ss[i],low=-Inf,upper=Inf)$value
matplot(ss,pr,type="l",lwd=3,col=1,xlab="",ylab="")
mtext(side=1,"s",line=2.75,cex=2,font=5)
mtext(side=2,"Probability of two local minima",line=2.75,cex=1.5)
legend(.25,.15,c("beta=.5","beta=1"),lwd=3,lty=1:2,cex=1.5,bg="gray95")

pr55=integrate(integr,beta=.5,sigma=.5,low=-Inf,upper=Inf)$value
lines(x=c(-1,.5,.5),y=c(pr55,pr55,-1),lty=2)
pr55
}
