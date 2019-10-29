olsim <-
function(n=10,alpha=-1,beta=1,x=1:n,sigma=2,nSim=10000)
{
dump("olsim","c:\\StatBook\\olsim.r")
par(mfrow=c(1,3))
mx=mean(x);SUMx=var(x)*(n-1)
b.est=a.est=s2.est=tstat=rep(NA,nSim)
for(isim in 1:nSim)
{
	Y=alpha+beta*x+rnorm(n,sd=sigma)
	b.est[isim]=cov(x,Y)*(n-1)/SUMx
	a.est[isim]=mean(Y)-b.est[isim]*mx
	res=Y-a.est[isim]-b.est[isim]*x
	s2.est[isim]=sum(res^2)/(n-2)
	tstat[isim]=(b.est[isim]-beta)/(sqrt(s2.est[isim])/SUMx)
}
par(mfrow=c(2,2))
Fval=(1:nSim)/nSim
a.est=a.est[order(a.est)]
plot(a.est,Fval,type="s",lwd=3,main="Intercept")
a=seq(from=min(a.est),to=max(a.est),length=200)
lines(a,pnorm(a,mean=alpha,sd=sigma*sqrt(sum(x^2)/n/SUMx)),col=2)

b.est=b.est[order(b.est)]
plot(b.est,Fval,type="s",lwd=3,main="Slope")

b=seq(from=min(b.est),to=max(b.est),length=200)
lines(b,pnorm(b,mean=beta,sd=sigma/sqrt(SUMx)),col=2)

LHS=(n-2)*s2.est/sigma^2
LHS=LHS[order(LHS)]
plot(LHS,Fval,type="s",lwd=3,main="Chi-square")
s2=seq(from=min(LHS),to=max(LHS),length=200)
lines(s2,pchisq(s2,df=n-2),col=2)

LHSt=(b.est-beta)/sqrt(s2.est/SUMx)
LHSt=LHSt[order(LHSt)]
plot(LHSt,Fval,type="s",lwd=3,main="T-distribution")
Tval=seq(from=min(LHSt),to=max(LHSt),length=200)
lines(Tval,pt(Tval,df=n-2),col=2)

}
