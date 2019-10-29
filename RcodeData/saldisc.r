saldisc <-
function(job=1,m=600,n=500,delta=2,ss=6,LSD=17)
{
dump("saldisc","c:\\StatBook\\saldisc.r")
set.seed(ss)
chol.drug=rnorm(n=n,mean=-delta,sd=LSD)
chol.placebo=rnorm(n=m,mean=0,sd=LSD)
dat=as.data.frame(cbind(round(c(chol.placebo,chol.drug),1),c(rep(0,m),rep(1,n))))
names(dat)=c("chol.perc","treatm")
write.csv(dat,"c:\\StatBook\\saldisc.csv",row.names=F)

dat=read.csv("c:\\StatBook\\saldisc.csv")
chol.drug=dat$chol.perc[dat$treatm==1];n=length(chol.drug)
chol.placebo=dat$chol.perc[dat$treatm==0];m=length(chol.placebo)

muX=mean(chol.drug);muY=mean(chol.placebo)
sig.hat=sqrt((var(chol.drug)*(n-1)+var(chol.placebo)*(m-1))/(m+n-2))
print("sig.hat:");print(sig.hat)
print("Mean % difference for the drug and placebo:");print(c(muX,muY))
print(t.test(chol.drug,chol.placebo,var.equal=T,alternative="less"))
pval=pt((muX-muY)/sig.hat/sqrt(1/n+1/m),df=n+m-2)
print(paste("p-value drug<placebo:",pval))
dval=pnorm((muX-muY)/sig.hat*sqrt(1/2));bval=1-dval
print(paste("d-value drug<placebo:",dval))

x=c(chol.drug,chol.placebo)
x=x[order(x)];nx=length(x)
xx=seq(from=min(x),to=max(x),length=200)
dnw=dnorm(xx,mean=muX,sd=sig.hat)
dw=dnorm(xx,mean=muY,sd=sig.hat)
if(job==1)
{
	par(mfrow=c(1,2))
	matplot(xx,cbind(dw,dnw),type="l",col=1,lty=1:2,lwd=3,axes=F,xlim=c(-55,55),ylim=c(0,.025),xlab="",ylab="")
	ss=seq(from=-55,to=55,by=10)
	axis(side=1,ss)
	axis(side=2,seq(from=0,to=.025,by=.005))
	mtext(side=3,paste("p-value = ",signif(pval,3),sep=""),cex=1.5,xlab="",ylab="")
	mtext(side=1,"% difference from the baseline",cex=1.3,line=3)
	mtext(side=2,"Normal density",cex=1.3,line=2.5)
	rug(chol.drug,ticksize=.075)
	rug(chol.placebo,ticksize=.05)
	segments(muY,max(dw),muY,-1,lty=1)
	segments(muX,max(dnw),muX,-1,lty=2)
	legend(-55,.007,c(paste("Placebo =",round(muY,2),"%",sep=""),paste("Drug = ",round(muX,2),"%",sep="")),lty=1:2,lwd=3,col=1,cex=1.5,bg="gray90") 
	sens=fp=rep(0,nx)
	AUC=0
	for(i in 1:nx)
	{
		sens[i]=sum(chol.drug<x[i])/n 
		fp[i]=sum(chol.placebo<x[i])/m
		if(i>1) AUC=AUC+(fp[i]-fp[i-1])*sens[i]
	}
	plot(fp,sens,type="s",axes=F,lwd=3,xlab="",ylab="")
	fp.2n=pnorm((xx-muY)/sig.hat);sens.2n=pnorm((xx-muX)/sig.hat)
	lines(fp.2n,sens.2n,col=2,lwd=3)
	segments(0,0,1,1,lty=2)
	axis(side=3,at=seq(from=0,to=1,length=length(ss)),lab=ss)
	axis(side=1,at=seq(from=0,to=1,length=6))
	axis(side=2,at=seq(from=0,to=1,length=6))
	text(.7,.2,paste("AUC = b-value = ",round(bval*100),"%",sep=""),cex=1.5)
	text(.2,.7,paste("d-value = ",round(dval*100),"%",sep=""),cex=1.5)
	#text(.2,.7,paste("d-value = ",round(dval*100),"%",sep=""),cex=1.5)
	mtext(side=1,"False positive",cex=1.3,line=3)
	mtext(side=2,"Sensitivity",cex=1.3,line=2.5)
	
	legend(0,1,c("Empirical ROC curve","Binormal ROC curve"),col=c(1,2),lwd=3,cex=1.25,bg="gray90")
}
if(job==2)
{
	par(mfrow=c(1,1),mar=c(4,4,1,1),cex.lab=1.25)
	ra=range(c(chol.placebo,chol.drug))
	plot(1,1,type="n",xlim=ra,ylim=ra,xlab="",ylab="")
	mtext(side=1,"% difference in placebo group",cex=1.5,line=2.8)
	mtext(side=2,"% difference in the drug group",cex=1.5,line=2.75)
	for(i in 1:m) points(rep(chol.placebo[i],n),chol.drug)
	segments(-100,-100,100,100,col=2,lwd=3)
	
	sens=fp=rep(0,nx)
	AUC=0
	for(i in 1:nx)
	{
		sens[i]=sum(chol.drug<x[i])/n 
		fp[i]=sum(chol.placebo<x[i])/m
		if(i>1) AUC=AUC+(fp[i]-fp[i-1])*sens[i]
	}
	text(5,-30,paste("Nonparametric b-value=",round(AUC*100),"%",sep=""),cex=1.5,col=3)
	text(-30,5,paste("Nonparametric d-value=",round(100-AUC*100),"%",sep=""),cex=1.5,col=3)
	text(-70,-40,paste("p-value =",signif(pval,2)),adj=0,cex=1.4,srt=90)
	text(-10,-73,"b-value = proportion of points below the diagonal",cex=1.4)
	text(-10,52,"d-value = proportion of points above the diagonal",cex=1.4)
}
}
