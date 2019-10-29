salMW <-
function(n=300,job=1,w=.05)
{
dump("salMW","c:\\StatBook\\salMW.r")
d=read.csv("c:\\StatBook\\salary.csv",header=T)
dptm=d[,1];sexC=d[,2];year=d[,3];y=d[,4]
d1=d2=d3=d4=rep(0,n)
d1[dptm=="Sales"]=1
d2[dptm=="IT"]=1
d3[dptm=="Finance"]=1
d4[dptm=="Production"]=1
LY=log(y)

m=matrix(c(1,1,2,2,2,2,2,2,2,2),ncol=5)
layout(m)
par(mar=c(4,4,3,1))

boxplot(list(y[sexC==0],y[sexC==1]),names=c("Females","Males"),cex=1.5)
mtext(side=2,"Salary, $1000",line=2.5,font=2)
pv=t.test(y[sexC==0],y[sexC==1])[[3]]
print(pv)
mtext(paste("P-value=",round(pv,4)),line=.5,font=2)
plot(year,y,"n",xlab="",ylab="")
mtext(side=1,"Years at work",cex=1.25,line=2.75)
mtext(side=3,"Salary for males and females versus years at work",cex=1.25,line=.5,font=2)
points(year[sexC==0]-w,y[sexC==0],pch=1,cex=1.5)
points(year[sexC==1]+w,y[sexC==1],pch=2,cex=1.5)
o=lm(LY~sexC+year);a=coef(o)
print(summary(o))

o=lm(LY~sexC+year+d1+d2+d3)
print(summary(o))
a=coef(o)
x=1:20
lines(x,exp(a[1]+a[2]+a[3]*x),lty=2,lwd=3)
lines(x,exp(a[1]+a[3]*x),lty=1,lwd=3)
legend(13.2,134,c("Female","Male"),pch=1:2,lty=1:2,lwd=3,cex=1.5)

o=lm(LY~sexC+year+dptm)
so=summary(o)
print(so)
ta=so$coefficients
vd=2*so$sigma^2+ta[,2]^2
bvalue=pt(abs(ta[2,1])/sqrt(vd[2]),df=o$df.residual)
cat("B-value of sexC = ",round(100*bvalue),"%\n",sep="")


o=lm(LY~sexC+year+d4)
so=summary(o)
print(so)

}
