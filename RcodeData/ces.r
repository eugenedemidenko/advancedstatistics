ces <-
function(ss=4)
{
dump("ces","c:\\StatBook\\ces.r")
da=read.csv("c:\\StatBook\\CES.csv",header=T,stringsAsFactors=F)
K=da$K;L=da$L;Y=da$Y
# Starting values
ngrid=100
ag=seq(from=.01,to=.99,length=ngrid)
rog=seq(from=.001,to=3,length=ngrid)
RSSmin=10^10
for(i in 1:ngrid)
for(j in 1:ngrid)
{
	yp=(ag[i]*K^rog[j]+(1-ag[i])*L^rog[j])^(1/rog[j])
	A.est=sum(Y*yp)/sum(yp^2)
	RSSij=sum((Y-A.est*yp)^2)
	if(RSSij<RSSmin)
	{
		RSSmin=RSSij
		aopt=ag[i];ropt=rog[j]
		#print(c(RSSmin,A.est,aopt,ropt))
	}
}
print(paste("Starting values:",A.est,aopt,ropt))
#CES function
out=nls(Y~A*(alpha*K^ro+(1-alpha)*L^ro)^(1/ro),start=c(A=A.est,alpha=aopt,ro=ropt))
print(summary(out))
a=coef(out)
RSS.CES=sum(summary(out)$residuals^2)
ng=50
kg=seq(from=round(min(K))-1,to=round(max(K))+1,length=ng)
lg=seq(from=round(min(L))-1,to=round(max(L))+1,length=ng)
cesf=matrix(ncol=ng,nrow=ng)
for(i in 1:ng)
for(j in 1:ng)
cesf[i,j]=a[1]*(a[2]*kg[i]^a[3]+(1-a[2])*lg[j]^a[3])^(1/a[3])
par(mfrow=c(1,1),mar=c(2,0,0,0))
p=persp(kg,lg,cesf,theta=30,phi=40,r=10,ticktype="detailed",xlab="K",ylab="L",zlab="Y")
kl=trans3d(K,L,Y,pmat=p)
Yp=Y-summary(out)$residuals
kl0=trans3d(K,L,Yp,pmat=p)
segments(kl$x,kl$y,kl0$x,kl0$y,lwd=2)
points(kl$x,kl$y,pch=16,cex=1,col=2)
points(kl0$x,kl0$y,pch=4,cex=1,col=3)
# LRT, Cobb-Douglas function
out=nls(Y~A*K^alpha*L^(1-alpha),start=c(A=A.est,alpha=aopt))
print(summary(out))
RSS.CD=sum(summary(out)$residuals^2)
LHS=length(Y)*log(RSS.CD/RSS.CES)
pv=1-pchisq(LHS,df=1)
paste("LRT p-value=",pv)    
}
