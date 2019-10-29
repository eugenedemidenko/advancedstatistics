benfordN <-
function(nSim=100000,mu=2)
{
dump("benfordN","c:\\StatBook\\benfordN.r")
par(mfrow=c(1,3))
x=seq(from=0,to=500,length=200)
a=10^(-10);b=10^10
hfx=1/(log(b)-log(a))/x
nfx=log10(exp(1))/sqrt(2*pi)/x*exp(-.5*log10(x)^2)
matplot(x,cbind(hfx,nfx),xlim=c(0,500),ylim=c(0,.005),xlab="",ylab="",type="l",col=1,lwd=3)
mtext(side=2,"Density",cex=1.25,line=2.5)
mtext(side=1,"x",cex=1.25,line=2.5)
legend(200,.005,c("Hyperbolic","Lognormal"),lty=1:2,lwd=3,bg=gray(.95),cex=1.25)
title("Hyperbolic and lognormal densities")
d=1:9
bfl=log10((d+1)/d)
plot(d,bfl,axes=F,ylim=c(0,.3),ylab="",xlab="")
mtext(side=2,"Probability",line=2.5,cex=1.25)
mtext(side=1,"d",cex=1.25,line=2.5)
axis(side=1,1:9)
axis(side=2,seq(from=0,to=.3,by=.05))
title("Distribution of the first digit")
#hyperbolic/uniform distribution
Y=runif(nSim,min=log10(a),max=log10(b))
X=10^Y
D=floor(X/10^floor(log10(X)))
prDunif.sim=rep(NA,9)
for(d in 1:9) prDunif.sim[d]=mean(D==d)
lines(1:9,prDunif.sim)
k=(-1000):1000
prDunifF=rep(NA,9)
for(d in 1:9)
{
	k1=k[k<=log10(b)-log10(d+1) & k>log10(a)-log10(d+1)]
	k2=k[k<log10(b)-log10(d) & k>log10(a)-log10(d)]
	prDunifF[d]=(sum(k1*log(10)+log((d+1)/a))-sum(k2*log(10)+log(d/a)))/(log(b)-log(a))
}
#log10normal/normal distribution
Y=rnorm(nSim)
X=10^Y
D=floor(X/10^floor(log10(X)))
prDnorm.sim=rep(NA,9)
for(d in 1:9) prDnorm.sim[d]=mean(D==d)
lines(1:9,prDnorm.sim)
k=(-1000):1000
prDnormF=rep(NA,9)
for(d in 1:9)
	prDnormF[d]=sum(pnorm(k+log10(d+1))-pnorm(k+log10(d)))

print(cbind(1:9,bfl,prDunif.sim,prDunifF,prDnorm.sim,prDnormF))

d=1:9;bfl=log10((d+1)/d)
sigma=seq(from=1,to=1.5,length=100);ns=length(sigma)
k=(-1000):1000
prDnorm=rep(NA,9)
diff=rep(NA,ns)
for(i in 1:ns)
{
	prDnormF=rep(NA,9)
	for(d in 1:9)
	 prDnorm[d]=sum(pnorm((k+log10(d+1)-mu)/sigma[i])-pnorm((k+log10(d)-mu)/sigma[i]))
	diff[i]=max(abs(bfl-prDnorm))
}
plot(sigma,diff,lwd=3,type="l",xlab="",ylab="")
mtext(side=1,"sigma",cex=1.25,line=2.5)
mtext(side=2,"Maximum absolute difference",cex=1.25,line=2.5)
title("Deviation from Benfords law\nLognormal distribution")

}
