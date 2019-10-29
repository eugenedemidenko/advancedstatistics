fracksim <-
function(mu=3,SD=2,p=2,nSim=1000000)
{
dump("fracksim","c:\\StatBook\\fracksim.r")
par(mfrow=c(1,2))
X=rnorm(nSim,mean=mu,sd=SD)
Z=X-floor(X)
Z=Z[order(Z)]
plot(Z,(1:nSim)/nSim,type="s",ylab="Probability",main="Normal")
z=seq(from=0,to=1,length=100)
pr=rep(0,length(z))
k.min=mu-10*SD;k.max=mu+10*SD
for(k in k.min:k.max) pr=pr+pnorm(z+k,mean=mu,sd=SD)-pnorm(k,mean=mu,sd=SD)
lines(z,pr,col=2)

X=rchisq(nSim,df=p)
Z=X-floor(X)
Z=Z[order(Z)]
plot(Z,(1:nSim)/nSim,type="s",ylab="Probability",main="Chi-square")
z=seq(from=0,to=1,length=100)
pr=rep(0,length(z))
for(k in 0:100) pr=pr+pchisq(z+k,df=p)-pchisq(k,df=p)
lines(z,pr,col=2)

}
