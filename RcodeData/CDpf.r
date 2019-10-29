CDpf <-
function(job=1,alpha=.05,N=200)
{
dump("CDpf","C:\\StatBook\\CDpf.r")
dat=read.csv("c:\\StatBook\\CDpf.csv")
y=log(dat$Y);k=log(dat$K);l=log(dat$L)
n=length(y)
slm=summary(lm(y~k+l))
print(slm)
outLM=slm$coefficients
if(job==1) # CI and CR for alpha & beta
{
	ci=as.data.frame(matrix(ncol=2,nrow=2),row.names=c("alpha","beta"))
	print(paste(100*(1-alpha),"% CI:",sep=""))
	names(ci)=c("lower limit","upper limit")
	qta=qt(1-alpha/2,df=n-3)
	ci[1,1]=outLM[2,1]-qta*outLM[2,2];ci[1,2]=outLM[2,1]+qta*outLM[2,2]
	ci[2,1]=outLM[3,1]-qta*outLM[3,2];ci[2,2]=outLM[3,1]+qta*outLM[3,2]
	print(ci)
	ixs=solve(vcov(slm)[2:3,2:3])
	agr=seq(from=.1,to=1.1,length=N);bgr=seq(from=-.3,to=1,length=N)
	ragr=rep(agr-outLM[2,1],N);rbgr=rep(bgr-outLM[3,1],each=N)
	lhs=matrix((ragr^2*ixs[1,1]+2*ragr*rbgr*ixs[1,2]+rbgr^2*ixs[2,2])/2,ncol=N)
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	co=contour(agr,bgr,lhs,levels=qf(1-alpha,df1=2,df2=n-3),lwd=5)
	points(outLM[2,1],outLM[3,1],cex=1.5)
	segments(ci[1,1],-10,ci[1,1],10);segments(ci[1,2],-10,ci[1,2],10)
	segments(-10,ci[2,1],10,ci[2,1]);segments(-10,ci[2,2],10,ci[2,2])
	mtext(side=1,"a",cex=2,line=2.5,font=5)
	mtext(side=2,"b",cex=2,line=2.5,font=5)	
	r.kl=cor(k,l)
	X=cbind(rep(1,n),k,l);iXX=solve(t(X)%*%X)
	r.ab=iXX[2,3]/sqrt(iXX[2,2]*iXX[3,3])
	text(.4,-.22,paste("correlation (k,l) =",round(r.kl,5),"\ncorrelation (a,b) =",round(r.ab,5)),pos=4,cex=1.5)
}
if(job==2)
{
	ynew=y-l
	x2=k-l
	#method 1
	so=summary(lm(ynew~l+x2))$coefficients
	print(so)
	pv1=so[2,4]
	print(paste("pv1=",pv1))
	#method 2
	a=1;cv=c(0,1,1)
	tobs=(a-sum(cv*outLM[,1]))/sqrt(t(cv%*%vcov(slm)%*%cv))
	pv2=2*(1-pt(abs(tobs),df=n-3))
	print(paste("pv2=",pv2))
	#method 3
	Fobs=(a-sum(cv*outLM[,1]))^2/t(cv%*%vcov(slm)%*%cv)
	pv3=1-pf(Fobs,df1=1,df2=n-3)
	print(paste("pv3=",pv3))
	#method 4
	S0=sum(lm(ynew~x2)$residuals^2)
	Fobs=(S0-slm$sigma^2*(n-3))/slm$sigma^2
	pv4=1-pf(Fobs,df1=1,df2=n-3)
	print(paste("pv4=",pv4))	
	#method 5
	suFULL=lm(y~l+k)
	suRESTR=lm(y~offset(l)+x2)
	outA=anova(suFULL,suRESTR)
	pv5=(outA$"Pr(>F)")[2]	
	print(paste("pv5=",pv5))	
	
	par(mfrow=c(1,1),mar=c(4,4,1,1))
	so=summary(lm(ynew~x2))$coefficients
	a=so[2,1];SEa=so[2,2]
	mc=(1-a)*dat$K-a*dat$L
	semc=qt(1-alpha/2,df=n-2)*(dat$K+dat$L)*SEa
	YL=mc-semc;YU=mc+semc
	matplot(1:n,cbind(YL,YU),col=1,lty=1,type="l",xlab="",ylab="")
	mtext(side=1,"Year",cex=1.5,line=2.75)
	mtext(side=2,"Optimal condition value",cex=1.25,line=2.75)
	ii=seq(from=n,to=1,by=-1)
	xp=c(1,1:n,ii,1)
	yp=c(YL[1],YU,YL[ii],YL[1])
	polygon(x=xp,y=yp,col="gray90")
	lines(1:n,mc,lwd=2)
	points(1:n,mc+semc);points(1:n,mc-semc)
	segments(1,0,n,0,lwd=3)
	
}

}
