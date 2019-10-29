obesegene <-
function()
{
dump("obesegene","c:\\StatBook\\obesegene.r")
da=read.csv("c:\\StatBook\\obesegene.csv",header=T)
G1=da$G1;G2=da$G2;age=da$age;sex=da$sex;BMIP=da$BMIP;BMI=da$BMI
m=matrix(c(1,1,1,1,2,2,2,2,2,2),ncol=5)
layout(m);par(mar=c(4,4,3,1),cex.main=2)
plot(density(BMI),type="l",lwd=3,xlab="",ylab="",main="Gaussian kernel densities")
rug(BMI,side=1);rug(BMIP,side=3)
mtext(side=1,"BMI observations",cex=1.25,line=2.75)
mtext(side=2,"Density value",cex=1.25,line=2.5)
lines(density(BMIP),lwd=3,lty=2)
legend(30,.055,c("BMI children (bottom)","BMI parents (top)"),lty=1:2,lwd=3,cex=1.5)

plot(BMIP,BMI,xlim=c(min(BMIP),66),xlab="",ylab="",main="BMI children versus BMI parents with and without genetics")
mtext(side=1,"BMI parents",cex=1.25,line=2.75)
mtext(side=2,"BMI children",cex=1.25,line=2.75)


LBMI=log(BMI);LBMIP=log(BMIP)
o=lm(LBMI~G1+G2+age+sex+LBMIP)
print(summary(o))

oR=lm(LBMI~age+sex+LBMIP)
print(summary(oR));R2=summary(oR)$r.squared


oF=lm(LBMI~G1+age+sex+LBMIP+I(G2*LBMIP))
print(summary(oF))
R2F=summary(oF)$r.squared
R2G=(R2F-R2)/(1-R2)
print(paste("Generalized coefficient of determination =",round(R2G,3)))
a=coef(oF)
x=range(LBMIP)
x=seq(from=x[1],to=x[2],length=100)
lines(exp(x),exp(a[1]+a[2]+a[3]*60+(a[5]+a[6])*x),lwd=3,col=2);text(60.3,exp(a[1]+a[2]+a[3]*60+(a[5]+a[6])*x[100]),"Bad genes",adj=0,cex=1.5)
lines(exp(x),exp(a[1]+a[3]*60+a[5]*x),lwd=3,col=3);text(60.3,exp(a[1]+a[3]*60+a[5]*x[100]),"Good genes",adj=0,cex=1.5)

}
