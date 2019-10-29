simLM <-
function(n=10,sigma=1,beta=c(-1,1,-.5),nSim=10000,k=3)
{
dump("simLM","c:\\StatBook\\simLM.r")
x=1:n
X=cbind(rep(1,n),x,x^2)
iXX=solve(t(X)%*%X)
LX=iXX%*%t(X)
b3=lhs=t3=rep(NA,nSim)
se3=sigma*sqrt(iXX[3,3])
for(isim in 1:nSim)
{
	y=beta[1]+beta[2]*x+beta[3]*x^2+rt(n,df=k)*sigma/sqrt(k/(k-2))
	b=LX%*%y
	b3[isim]=b[3]
	y.hat=X%*%b
	s2=sum((y-y.hat)^2)/(n-3)
	lhs[isim]=(n-3)/sigma^2*s2
	t3[isim]=(b[3]-beta[3])/se3
}
par(mfrow=c(1,3),mar=c(3.5,3.5,3,1),cex.lab=1.5)

b3=b3[order(b3)]
Fe=(1:nSim)/nSim
xx=seq(from=beta[3]-4*se3,to=beta[3]+4*se3,length=200)
plot(b3,Fe,type="s",xlim=c(beta[3]-4*se3,beta[3]+4*se3),lwd=3,ylab="",xlab="",main="The beta-coefficient at the quadratic term")
cx=1.2
mtext(side=1,"Ordered slope estimate",line=2.5,cex=cx)
mtext(side=2,"CDF",line=2.25,cex=cx)
lines(xx,pnorm(xx,mean=beta[3],sd=se3))

lhs=lhs[order(lhs)]
xx=seq(from=0,to=5*n,length=200)
plot(lhs,Fe,type="s",xlim=c(0,5*n),ylab="",xlab="",main="Normalized regression variance",lwd=3)
lines(xx,pchisq(xx,df=n-3))
mtext(side=1,"Ordered slope estimate",line=2.5,cex=cx)
mtext(side=2,"CDF",line=2.25,cex=cx)

t3=t3[order(t3)]
xx=seq(from=-5,to=5,length=200)
plot(t3,Fe,type="s",xlim=c(-5,5),ylab="",xlab="",main="T-statistic of the quadratic term",lwd=3)
lines(xx,pt(xx,df=n-3))
mtext(side=1,"Ordered slope estimate",line=2.5,cex=cx)
mtext(side=2,"CDF",line=2.25,cex=cx)
#mtext(side=3,outer=T,paste("Properties of the OLS coefficient at the quadratic term, n=",n,", sigma=",sigma,", t-df=",k,sep=""),cex=1.5,line=1)

}
