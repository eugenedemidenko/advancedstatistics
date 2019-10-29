linpower <-
function(n=10,m=3,k=2,NL=30,beta=c(1,.5,-.5),sigma=2,alpha=.05,nSim=10000,st=3)
{
dump("linpower","c:\\StatBook\\linpower.r")
set.seed(st)
C=matrix(c(0,1,1,0,-1,0),nrow=k);a=C%*%beta

X=cbind(rep(1,n),1:n,(1:n)^2)
XX=t(X)%*%X;iXX=solve(XX);ix=iXX%*%t(X)
cx=C%*%iXX%*%t(C)
icx=solve(cx)

d=runif(m,min=-1,max=1)
Cd=C%*%d
qff=qf(1-alpha,df1=k,df2=n-m)
Fobs=rep(NA,nSim)
lambda=powEMP=powT=seq(from=0,to=30,length=NL)
for(i in 1:NL)
{
	nu=as.numeric(sigma*sqrt(lambda[i]/(t(Cd)%*%icx%*%Cd)))
	beta.alt=beta+nu*d
	for(isim in 1:nSim)
	{
		y=X%*%beta.alt+rnorm(n,sd=sigma)
		bh=ix%*%y
		br=bh+iXX%*%t(C)%*%icx%*%(a-C%*%bh)
		S0=sum((y-X%*%br)^2)
		Smin=sum((y-X%*%bh)^2)
		Fobs[isim]=(S0-Smin)/k/Smin*(n-m)	
	}
	powEMP[i]=mean(Fobs>qff)	
	powT[i]=1-pf(qff,df1=k,df2=n-m,ncp=lambda[i])
}
par(mfrow=c(1,1),mar=c(4,4,1,1))
plot(lambda,powT,type="l",ylim=c(0,1),xlab="",ylab="")
mtext(side=1,"l",font=5,cex=2,line=2.75)
mtext(side=2,"Power",cex=1.5,line=2.5)
segments(-1,alpha,lambda[NL],alpha,lty=2)
text(20,alpha+.03,paste("a =",alpha),cex=1.5,font=5)
points(lambda,powEMP)

}
