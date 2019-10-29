qolS <-
function(n=50,mu=50,sigmaQ=20,sigma.eps=2,alpha=1,lambda=3,ss=3)
{
# quality of life (QoL) versus survival
dump("qolS","c:\\StatBook\\qolS.r")
#set.seed(ss)
#Q=matrix(nrow=n,ncol=100)
#T=rep(0,n)
#id=ttL=QOL=NULL
#for(i in 1:n)
#{
#	Q0=rnorm(1,mean=mu,sd=sigmaQ)
#	T[i]=rpois(1,lambda=lambda^(2*Q0/mu))
#	tt=0:T[i]
#	qi=Q[i,1:(T[i]+1)]=Q0-alpha*tt+rnorm(T[i]+1,mean=0,sd=sigma.eps)
#	lines(tt,qi)
#	id=c(id,rep(i,T[i]+1))
#	ttL=c(ttL,tt)
#	QOL=c(QOL,qi)
#}
#QOL=round(QOL)
#xd=data.frame(cbind(id,ttL,QOL))
#write.csv(xd,"c:\\StatBook\\QoL.csv",row.names=F)
d=read.csv("c:\\StatBook\\QoL.csv",header=T)
id=d[,1];tt=d[,2];Q=d[,3]
par(mfrow=c(1,1),mar=c(4,4,1,1))
plot(1,1,xlim=c(0,20),ylim=c(0,100),type="n",xlab="",ylab="")
mtext(side=1,"Time after cancer diagnosis until death, years",cex=1.5,line=2.75)
mtext(side=2,"Quality of Life (QoL)",cex=1.5,line=2.5)
uid=unique(id);nuid=length(uid)
D=matrix(0,ncol=nuid,nrow=length(id))
for(i in 1:nuid)
{
	ti=tt[id==uid[i]]
	qi=Q[id==uid[i]]
	qi=qi[order(ti)];ti=ti[order(ti)]
	lines(ti,qi)
	D[id==uid[i],i]=1
}

o=lm(QOL~ttL,data=d)
print(summary(o))
a=coef(o)
x=0:20
lines(x,a[1]+a[2]*x,lwd=3,lty=2)

oF=lm(Q~D+tt-1)
a=coef(oF)
a0=mean(a[1:nuid]);a1=a[nuid+1]
print(summary(oF))
lines(x,a0+a1*x,lwd=3)

legend(7,18,c("Standard linear regression","Patient-specific intercept term regression"),lty=c(2,1),lwd=3,cex=1.25,bg=gray(.95))
}
