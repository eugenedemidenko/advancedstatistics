smallF <-
function(job=1,n1=3,n2=10,nu=1,alpha=.05,nu.alt=2,nSim=100000)
{
dump("smallF","c:\\StatBook\\smallF.r")

q1q2F=function(n1,n2,alpha,maxit=100,eps=0.0001) #nonequal tail quantiles
{
	k1=(n1-1)/2;k2=(n2-1)/2
	q1=qf(alpha/2,n1-1,n2-1);q2=qf(1-alpha/2,n1-1,n2-1)
	H=matrix(ncol=2,nrow=2)
	d=rep(0,2)
	for(it in 1:maxit)
	{
		H[1,1]=-k1/q1+k1*(k1+k2)/(k2+k1*q1)
		H[1,2]=k1/q2-k1*(k1+k2)/(k2+k1*q2)
		H[2,1]=-df(q1,n1-1,n2-1);H[2,2]=df(q2,n1-1,n2-1)
		
		d[1]=k1*log(q2)-(k1+k2)*log(k2+k1*q2)-k1*log(q1)+(k1+k2)*log(k2+k1*q1)
		d[2]=pf(q2,n1-1,n2-1)-pf(q1,n1-1,n2-1)-(1-alpha)
		delta=solve(H)%*%d
		if(sum(abs(delta))<eps) break
		q1=q1-delta[1];q2=q2-delta[2]	
		#print(c(it,q1,q2,d))
	}
	c(q1,q2)
}

par(mfrow=c(1,2),mar=c(3.5,3.5,3,1),cex.main=1.25)
q1=qf(alpha/2,n1-1,n2-1);q2=qf(1-alpha/2,n1-1,n2-1)
nus=seq(from=.2,to=3,length=200)
powEQT=pf(q2/nus,n1-1,n2-1,lower.tail=F)+pf(q1/nus,n1-1,n2-1)

q1q2NET=q1q2F(n1=n1,n2=n2,alpha=alpha)
powNET=pf(q1q2NET[2]/nus,n1-1,n2-1,lower.tail=F)+pf(q1q2NET[1]/nus,n1-1,n2-1)
matplot(nus,cbind(powNET,powEQT),lwd=3,col=1,lty=1:2,type="l",xlab="",ylab="")	
mtext(side=1,"Alternative variance ratio, nu",cex=1.4,line=2.5)
mtext(side=2,"Power function, probability",cex=1.4,line=2.5)
title("Power functions of equal and unequal-tail F-tests") 
segments(0,alpha,100,alpha,lty=2,col=2)
legend(.5,.21,c("Unequal-tail F-test","Equal-tail F-test"),lty=1:2,lwd=3,bg="gray90",cex=1.4)
#simulations
X1=matrix(rnorm(n1*nSim,sd=sqrt(nu.alt)),ncol=n1)
s2.1.hat=apply(X1,1,var)
X2=matrix(rnorm(n2*nSim,sd=1),ncol=n2)
s2.2.hat=apply(X2,1,var)
q1q2NET=q1q2F(n1=n1,n2=n2,alpha=alpha)
pow.sim=mean(s2.1.hat/s2.2.hat>q1q2NET[2] | s2.1.hat/s2.2.hat<q1q2NET[1])
points(nu.alt,pow.sim,cex=2)

# optimal design

N1=50:125;N2=50:125
pow=matrix(nrow=length(N1),ncol=length(N1))
for(n1 in N1)
for(n2 in N2)
{
	q1q2NET=q1q2F(n1=n1,n2=n2,alpha=alpha)	
	pow[n1-49,n2-49]=powNET=pf(q1q2NET[2]/nu.alt,n1-1,n2-1,lower.tail=F)+pf(q1q2NET[1]/nu.alt,n1-1,n2-1)
}
contour(N1,N2,pow,levels=c(.8,.9),lwd=3,xlab="",ylab="",labcex=1)
title(paste("Optimal sample sizes, n1 + n2 = min for nu.alt = ",nu.alt,sep=""))
mtext(side=1,"Sample size, n1",line=2.5,cex=1.4)
mtext(side=2,"Sample size, n2",line=2.5,cex=1.4)

for(lev in c(.8,.9))
{
	con=contourLines(N1,N2,levels=lev,pow)
	n1=con[[1]]$x;n2=con[[1]]$y
	N=length(n1)
	n11=n1[2:N];n22=n2[2:N]
	sl=abs((n22-n2[1:(N-1)])/(n11-n1[1:(N-1)])+1)
	n1.opt=n1[2:N][sl==min(sl)]
	n2.opt=n2[2:N][sl==min(sl)]
	points(n1.opt,n2.opt,pch=1,cex=2)
	text(n1.opt+3,n2.opt+.5,paste("n1 = ",round(n1.opt),", n2 = ",round(n2.opt),sep=""),adj=0,cex=1.25)
	segments(n1.opt-10,n2.opt+10,n1.opt+10,n2.opt-10,col=2)
}
legend(90,120,c("Power=80%","Power=90%"),lty=1:2,lwd=3,cex=1.4,bg="gray90")

}
