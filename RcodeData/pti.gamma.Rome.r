pti.gamma.Rome <-
function(p=.75,alpha=3,lambda=2)
{
dump("pti.gamma.Rome","c:\\StatBook\\pti.gamma.Rome.r")
t=qgamma((1-p)/2,shape=alpha,rate=lambda)
T=qgamma((1+p)/2,shape=alpha,rate=lambda)
tT=c(t,T)
M=matrix(ncol=2,nrow=2)
x=seq(0,to=qgamma(.999,shape=alpha,rate=lambda),length=200)
par(mfrow=c(1,1),mar=c(4,4,1,1))
plot(x,dgamma(x,shape=alpha,rate=lambda),lwd=3,type="l",xlab="",ylab="")
mtext(side=1,"Time from the Barbarians first seen to the Rome guards see the smoke, hours",cex=1.5,line=2.75)
mtext(side=2,"Gamma density",cex=1.5,line=2.75)

for(it in 1:10) #Newton's iterations
{
	eq1=(alpha-1)*(log(tT[2])-log(tT[1]))-lambda*(tT[2]-tT[1])
	eq2=pgamma(tT[2],shape=alpha,rate=lambda)-pgamma(tT[1],shape=alpha,rate=lambda)-p
	M[1,1]=-(alpha-1)/tT[1]+lambda;M[1,2]=(alpha-1)/tT[2]-lambda
	M[2,1]=-dgamma(tT[1],shape=alpha,rate=lambda);M[2,2]=dgamma(tT[2],shape=alpha,rate=lambda)
	delta=solve(M)%*%c(eq1,eq2)
	if(max(abs(delta))<0.0001) break
	tT=tT-delta

}
segments(tT[1],-1,tT[1],dgamma(tT[1],shape=alpha,rate=lambda))
segments(tT[2],-1,tT[2],dgamma(tT[2],shape=alpha,rate=lambda))
return(tT)
}
