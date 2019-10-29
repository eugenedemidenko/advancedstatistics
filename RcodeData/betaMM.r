betaMM <-
function(alpha=2,beta=3,n=100,nSim=10000)
{
dump("betaMM","c:\\StatBook\\betaMM.r")
print(date())
der=rep(0,2);H=matrix(ncol=2,nrow=2)
ab=hv=matrix(ncol=2,nrow=nSim)
for(i in 1:nSim)
{
	X=rbeta(n,alpha,beta)
	K1=mean(log(X));K2=mean(log(1-X))
	alphas=1;betas=1
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
		if(abs(delta[1])+abs(delta[2])<.001) break #converged
		alphas=alphas-delta[1];betas=betas-delta[2]
		#print(c(it,der,alphas,betas))
	}
	ab[i,1]=alphas;ab[i,2]=betas	
	hv[i,1]=iH[1,1]/n;hv[i,2]=iH[2,2]/n
}
par(mfrow=c(1,2),mar=c(4,4,3,1))
da=density(ab[,1],from=1,to=3)
plot(da,type="l",lwd=3,xlab="alpha estimate",ylab="Density",main=paste("alpha true=",alpha,", mode=",round(da$x[da$y==max(da$y)],2),sep=""))
db=density(ab[,2],from=2,to=5)
plot(db,type="l",lwd=3,xlab="beta estimate",ylab="Density",main=paste("beta true=",beta,", mode=",round(db$x[db$y==max(db$y)],2),sep=""))

H[1,1]=trigamma(alpha)-trigamma(alpha+beta)
H[1,2]=-trigamma(alpha+beta)
H[2,1]=-trigamma(alpha+beta)
H[2,2]=trigamma(beta)-trigamma(alpha+beta)
iH=solve(H)/n
print(c(var(ab[,1]),iH[1,1],mean(hv[,1])))
print(c(var(ab[,2]),iH[2,2],mean(hv[,2])))
print(date())
}
