housepr <-
function()
{
dump("housepr","c:\\StatBook\\housepr.r")
dat=read.csv("c:\\StatBook\\houseprice.csv",header=T)
AB=dat[,1];pr=dat$price;ft=dat$footage
prA=pr[AB=="A"];prB=pr[AB=="B"]
ftA=ft[AB=="A"];ftB=ft[AB=="B"]
par(mfrow=c(1,2),mar=c(4,4,3,1),cex.main=1.5)
cl=c("gray50","gray80")

plot(ftA,prA,cex=.75,pch=16,col=cl[1],xlab="",ylab="",main="Separate regressions")
points(ftB,prB,cex=.75,pch=16,col=cl[2])
d=density(ftA);lines(d$x,670-10^5*d$y,lwd=2,col=cl[1])
d=density(ftB);lines(d$x,670-10^5*d$y,lwd=2,col=cl[2])
d=density(prA);lines(7000-10^5*d$y,d$x,lwd=2,col=cl[1])
d=density(prB);lines(7000-10^5*d$y,d$x,lwd=2,col=cl[2])
mtext(side=1,"House square footage",cex=1.5,line=2.75)
mtext(side=2,"House price, thousand dollars",cex=1.5,line=2.75)
xx=1000:7000
oA=lm(prA~ftA);a=coef(oA)
lines(xx,a[1]+a[2]*xx,col=cl[1],lwd=3)
oB=lm(prB~ftB);a=coef(oB)
lines(xx,a[1]+a[2]*xx,col=cl[2],lwd=3)
legend(4000,200,c("Area A","Area B"),col=cl,lwd=3,pch=16,cex=1.5)

plot(ftA,prA,cex=.75,pch=16,col=cl[1],xlab="",ylab="",main="Parallel regressions")
points(ftB,prB,cex=.75,pch=16,col=cl[2])
d=density(ftA);lines(d$x,670-10^5*d$y,lwd=2,col=cl[1])
d=density(ftB);lines(d$x,670-10^5*d$y,lwd=2,col=cl[2])
d=density(prA);lines(7000-10^5*d$y,d$x,lwd=2,col=cl[1])
d=density(prB);lines(7000-10^5*d$y,d$x,lwd=2,col=cl[2])
mtext(side=1,"House square footage",cex=1.5,line=2.75)
mtext(side=2,"House price, thousand dollars",cex=1.5,line=2.75)
x=c(ftA,ftB);y=c(prA,prB)
d=c(rep(1,length(ftA)),rep(0,length(ftB)))
o=lm(price~footage+AB,data=dat);a=coef(o)
print(summary(o))
lines(xx,a[1]+a[2]*xx+a[3],col=cl[1],lwd=3)
lines(xx,a[1]+a[2]*xx,col=cl[2],lwd=3)
text(1000,500,paste("Difference =",round(a[3])),cex=1.5,adj=0,font=4)


}
