powlinmod <-
function(N01=TRUE,alpha=0.05,b0=-1,tau=.6,s2=2,sdx=1,sdu=.5,roxu=.7,nSim=100000)
{
dump("powlinmod","c:\\StatBook\\powlinmod.r")
par(mfrow=c(1,2),mar=c(4,4.5,3.5,1),cex.lab=1.5,cex.main=1.5)
Nb=50 # number of grid values for b.alt 
k=0
leg=c("topleft","bottomright")
for(n in c(10,50))
{
	if(N01) tzcrit=qnorm(1-alpha/2) else tzcrit=qt(1-alpha/2,df=n-2)
	k=k+1
	X=rnorm(n*nSim,sd=sdx);Z=rnorm(n*nSim)
	h=roxu/sdx/sqrt(1-roxu^2);U=(Z+h*X)/sqrt(h^2*sdx^2+1)
	v2=sdx^2*(1-roxu^2)
	b.alt=seq(from=b0-1,to=b0+1,length=Nb)
	powWA=powLR=powSC=rep(NA,Nb)
	for(i in 1:Nb)
	{
		Y=b.alt[i]*X+tau*U+rnorm(n*nSim,sd=sqrt(s2))
		X=matrix(X,ncol=n);U=matrix(U,ncol=n);Y=matrix(Y,ncol=n)	
		c11=rowSums(X^2);c12=rowSums(X*U);c22=rowSums(U^2)
		yx=rowSums(X*Y);yu=rowSums(U*Y)
		disc=c11*c22-c12^2
		b.hat=(yx*c22-yu*c12)/disc;tau.hat=(-yx*c12+yu*c11)/disc
		res=Y-b.hat*X-tau.hat*U
		Smin=rowSums(res^2)
		s2.hat=Smin/n 
		#s2.hat=Smin/(n-2) #unbiased sigma.hat^2
		#Wald
		WA=(b.hat-b0)/sqrt(s2.hat/v2/n)
		powWA[i]=mean(abs(WA)>tzcrit)	
		# LR
		tau0=rowSums((Y-b0*X)*U)/rowSums(U^2)
		res0=Y-b0*X-tau0*U
		S0=rowSums(res0^2)
		LR=sign(b.hat-b0)*sqrt(n*log(S0/Smin))
		powLR[i]=mean(abs(LR)>tzcrit)	
		#score
		SC=rowSums(res0*X)/sqrt(v2*s2.hat*n)
		powSC[i]=mean(abs(SC)>tzcrit)	
	}
	matplot(b.alt,cbind(powWA,powLR,powSC),lwd=2,col=1,lty=1:3,type="l",ylim=c(0,.8),xlab="Alternative slope",ylab="Power")
	segments(-100,alpha,100,alpha,col=5)	
	legend(leg[k],c("Wald","LR","Score"),col=1,lty=1:3,lwd=2,bg="gray90",cex=1.25)
	title(paste("N01=",N01,", n=",n,sep=""))
}	
}
