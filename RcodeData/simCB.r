simCB <-
function(job=1,n=10,beta=c(1,1,-.05),sigma=1,NX=100,alpha=.05,sk=3,nSim=10000,xmin=-20,xmax=30)
{
dump("simCB","c:\\StatBook\\simCB.r")
set.seed(sk)
X=cbind(rep(1,n),1:n,(1:n)^2)
m=ncol(X)
F1=qf(1-alpha,df1=m,df2=n-m)
t1=qt(1-alpha/2,df=n-m)
iXX=solve(t(X)%*%X)
iXXx=iXX%*%t(X)

if(job==1)
{
	y=X%*%beta+rnorm(n,sd=sigma)
	b=iXXx%*%y
	s=sqrt(sum((y-X%*%b)^2)/(n-m))
	x=seq(from=0,to=n+6,length=NX)
	lb=lu=r=lbIND=luIND=rep(NA,NX)
	for(i in 1:NX)
	{
		xv=c(1,x[i],x[i]^2)
		sex=s*sqrt(m)*sqrt(F1*t(xv)%*%iXX%*%xv)
		lb[i]=sum(xv*b)-sex;lu[i]=sum(xv*b)+sex
		sexIND=s*t1*sqrt(t(xv)%*%iXX%*%xv)
		lbIND[i]=sum(xv*b)-sexIND;luIND[i]=sum(xv*b)+sexIND
		r[i]=sum(xv*b)
	}
	ll=c(lu,lb)
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	x12=range(x);y12=range(ll)
	iss=seq(from=NX,to=1,by=-1)
	plot(1:n,y,xlim=x12,ylim=y12,xlab="",ylab="")	
	mtext(side=1,"x",cex=1.5,line=2.75)
	mtext(side=2,"y",cex=1.5,line=2.75)
	polygon(x=c(x12[1],x,x[iss]),y=c(lu[1],lu,lb[iss]),col="gray94")
	points(1:n,y)
	lines(x,r,lwd=3)
	lines(x,lb);lines(x,lu)
	lines(x,lbIND,lty=2);lines(x,luIND,lty=2)
	legend(0,20,c("Point CI","Simultaneous CI"),lty=c(2,1),cex=1.5,bg="gray90")
}
if(job==2)
{
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	m=1:6;NM=length(m)
	n=c(10,50);LN=length(n)
	rat=matrix(ncol=LN,nrow=NM)
	for(i in 1:LN)
	rat[,i]=sqrt(m*qf(1-alpha,df1=m,df2=n[i]-m))/qt(1-alpha/2,df=n[i]-m)
	matplot(m,rat,col=1,lwd=3,type="l",xlab="",ylab="")
	points(m,rat[,1],pch=16,cex=2);points(m,rat[,2],pch=1,cex=2)
	mtext(side=1,"m",cex=1.5,line=2.75)
	mtext(side=2,"Ratio of widths",cex=1.5,line=2.5)
	legend(1,2.1,c("n = 10","n = 50"),lty=1:2,pch=c(16,1),lwd=3,cex=1.75,bg="gray95")
}
if(job==3) # simulations
{
	x=seq(from=xmin,to=xmax,length=NX)
	Xpr=cbind(rep(1,NX),x,x^2)
	A=Xpr%*%iXX
	bx=sqrt(m*rowSums(A*Xpr)*F1)
	reg.true=Xpr%*%beta
	pr=rep(0,nSim)
	for(isim in 1:nSim)
	{
		y=X%*%beta+rnorm(n,sd=sigma)
		b=iXXx%*%y
		reg=Xpr%*%b
		s=sqrt(sum((y-X%*%b)^2)/(n-m))
		sex=s*bx
		lb=reg-sex;lu=reg+sex
		ind=sum(lb>reg.true)+sum(lu<reg.true)
		if(ind>0) pr[isim]=1	
	}
	print(1-mean(pr))
}

}
