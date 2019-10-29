betaMM.apply <-
function(alpha=2,beta=3,n=100,nSim=10000)
{
dump("betaMM.apply","c:\\StatBook\\betaMM.apply.r")
print(date())
X=matrix(rbeta(n*nSim,alpha,beta),ncol=n)
MM=function(x)
{
	n=length(x)
	K1=mean(log(x));K2=mean(log(1-x))
	alphas=2;betas=3
	xb=mean(x);s2=var(x)
	r=(xb-xb^2-s2)/s2;alphas=xb*r;betas=(1-xb)*r
	der=rep(NA,2);H=matrix(ncol=2,nrow=2)
	for(it in 1:100) #Newton's iteraions
	{
		der[1]=psigamma(alphas)-psigamma(alphas+betas)-K1
		der[2]=psigamma(betas)-psigamma(alphas+betas)-K2
		H[1,1]=trigamma(alphas)-trigamma(alphas+betas)
		H[1,2]=-trigamma(alphas+betas)
		H[2,1]=-trigamma(alphas+betas)
		H[2,2]=trigamma(betas)-trigamma(alphas+betas)
		iH=solve(H)
		delta=iH%*%der
		eps=abs(delta[1])+abs(delta[2])
		if(eps<.0001) break #converged
		alphas=alphas-delta[1];betas=betas-delta[2]
	}
	out=c(alphas,betas,iH[1,1]/n,iH[2,2]/n)
	if(eps>=0.001) out=rep(NA,4)
	return(out)
}
out=t(apply(X,1,FUN=MM))
ab=out[,1:2];hv=out[,3:4]

par(mfrow=c(1,2),mar=c(4,4,3,1))
da=density(ab[,1],from=1,to=3)
plot(da,type="l",lwd=3,xlab="alpha estimate",ylab="Density",main=paste("alpha true=",alpha,", mode=",round(da$x[da$y==max(da$y)],2),sep=""))
db=density(ab[,2],from=2,to=5)
plot(db,type="l",lwd=3,xlab="beta estimate",ylab="Density",main=paste("beta true=",beta,", mode=",round(db$x[db$y==max(db$y)],2),sep=""))
H=matrix(ncol=2,nrow=2)
H[1,1]=trigamma(alpha)-trigamma(alpha+beta)
H[1,2]=-trigamma(alpha+beta)
H[2,1]=-trigamma(alpha+beta)
H[2,2]=trigamma(beta)-trigamma(alpha+beta)
iH=solve(H)/n
print(c(var(ab[,1]),iH[1,1],mean(hv[,1])))
print(c(var(ab[,2]),iH[2,2],mean(hv[,2])))
print(date())
}
