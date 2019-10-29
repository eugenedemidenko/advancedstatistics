marathon <-
function()
{
dump("marathon","c:\\StatBook\\marathon.r")
da=matrix(scan("c:\\StatBook\\marathonWR2.txt",what=""),ncol=2,byrow=T)
year=as.numeric(da[,2])
ti=as.character(da[,1])
ti=120+as.numeric(substring(ti,1,2))+as.numeric(substring(ti,4,6))/60
par(mfrow=c(1,1))
plot(year,ti,type="b",xlim=c(1900,2050),ylim=c(110,180),xlab="Year",ylab="Marathon world record min",axes=F,lwd=3,col=3)
polygon(x=c(1900,1900,2050,2050,1900),y=c(110,180,180,110,110),col="gray90")
points(year,ti,type="b",col=2,lwd=2)
ys=seq(from=1900,to=2050,by=10);ns=length(ys)
for(y in ys) segments(y,rep(110,ns),y,rep(180,ns),col="white")
ms=seq(from=110,to=180,by=5);ns=length(ms)
for(m in ms) segments(rep(1900,ns),m,rep(2050,ns),col="white")
axis(side=1,seq(from=1900,to=2050,by=10),cex=.75)
axis(side=2,seq(from=110,to=180,by=5),srt=90)
# starting values
SS=10^10
a3.s=seq(from=0,to=1,length=100)
for(i in 1:100)
{
    x=exp(-a3.s[i]*(year-1900))
    lmi=lm(ti~x)
    si=summary(lmi)$sigma
    if(si<SS)
    {
        a1.0=coef(lmi)[1];a2.0=coef(lmi)[2]
        a3.0=a3.s[i];SS=si
    }
}
print(c(SS,a1.0,a2.0,a3.0))
out=nls(ti~a1+a2*exp(-a3*(year-1900)),start=list(a1=a1.0,a2=a2.0,a3=a3.0))
print(summary(out));a=coef(out)
title(paste("Marathon World record=",round(a[1],2),"+",round(a[2],2),"*exp(-",round(a[3],4),"*(Year-1900))",sep=""))
x=1900:2050;yf=a[1]+a[2]*exp(-a[3]*(x-1900))
lines(x,yf,col=3,lwd=3);segments(1000,a[1],2050,a[1])
text(1940,a[1]+2,paste("Absolute marathon world record =",round(a[1],2),"minutes"))
}
