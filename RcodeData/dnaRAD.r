dnaRAD <-
function()
{
dump("dnaRAD","c:\\StatBook\\dnaRAD.r")
da=read.csv("c:\\StatBook\\dnaRAD.csv",stringsAsFactors=F)
Surv=da[,2];log10Surv=log10(Surv)
rad=da[,1];n=length(rad)
par(mfrow=c(1,1),mar=c(4,4,1,1))
plot(rad,log10Surv,ylim=c(1,4),axes=F,xlab="",ylab="")
mtext(side=1,"Radiation given, Gy",cex=1.5,line=2.75)
mtext(side=2,"Survival, hours",cex=1.5,line=2.75)
axis(side=1,at=seq(from=0,to=24,by=2))
axis(side=2,at=1:4,labels=c("1","10","100","1000"),srt=90)
#return()
rT=1:23;ntr=length(rT)
ssMIN=10^10
for(i in 1:ntr)
{
	d=rep(0,n);d[rad>rT[i]]=1
	x1=-rad;x2=-pmax(rad-rT[i],0)
	o=lm(log10Surv~x1+x2)
	SS=sum(o$residuals^2)
	if(SS<ssMIN)
	{
		oOPT=o
		trOPT=rT[i]
		ssMIN=SS	
	}	
}
a=coef(oOPT)
print(summary(oOPT))
print(trOPT)
x=seq(from=0,to=24,length=200);nx=length(x)
#y=a[1]-a[2]*x-a[3]*pmax(x-trOPT,0)
b=c(a,trOPT)
RSS.MIN=10^10 # our own iterations
for(it in 1:10)
{	
	x2=pmax(rad-b[4],0)
	f=b[1]-b[2]*rad-b[3]*x2
	J=cbind(rep(1,n),-rad,x2,b[3]*(rad>b[4]))
	de=t(J)%*%(log10Surv-f)
	iJJ=solve(t(J)%*%J)
	b.new=b+iJJ%*%de	
	x2=pmax(rad-b.new[4],0)
	f=b.new[1]-b.new[2]*rad-b.new[3]*x2
	RSS=sum((log10Surv-f)^2)
	if(RSS>RSS.MIN) break
	b=b.new
	RSS.MIN=RSS
}
s=sqrt(RSS.MIN/(n-4))
seb=s*sqrt(diag(iJJ))
out=as.data.frame(cbind(b,seb,2*(1-pnorm(abs(b/seb)))))
names(out)=c("beta-est","SE","P-value");row.names(out)=c("b1","b2","b3","b4")
print(out)
y=b[1]-b[2]*x-b[3]*pmax(x-b[4],0)
lines(x,y,lwd=2)
}
