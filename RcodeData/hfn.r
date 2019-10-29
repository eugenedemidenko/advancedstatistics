hfn <-
function(job=0,st=58)
{
dump("hfn","c:\\StatBook\\hfn.r")
da=read.csv("c:\\StatBook\\HeightFootNose.csv",header=T)
par(mfrow=c(1,3),mar=c(3.5,3.5,3,1))
plot(da$Foot,da$Height,xlab="",ylab="")
title(paste("Height versus length of foot, R =",round(cor(da$Foot,da$Height),2)))
mtext(side=1,"Foot, inches",cex=1.25,line=2.5)
mtext(side=2,"Height, inches",cex=1.25,line=2.25)
print(summary(lm(Height~Foot,data=da)))
abline(lsfit(x=da$Foot,y=da$Height),lwd=3)

plot(da$Nose,da$Height,xlab="",ylab="")
title(paste("Height versus length of nose, R =",round(cor(da$Nose,da$Height),2)))
mtext(side=1,"Nose, inches",cex=1.25,line=2.5)
mtext(side=2,"Height, inches",cex=1.25,line=2.25)
print(summary(lm(Height~Nose,data=da)))
abline(lsfit(x=da$Nose,y=da$Height),lwd=3)

plot(da$Nose,da$Foot,xlab="",ylab="")
title(paste("Height versus length of nose, R =",round(cor(da$Nose,da$Foot),2)))
mtext(side=1,"Foot, inches",cex=1.25,line=2.5)
mtext(side=2,"Nose, inches",cex=1.25,line=2.25)
print(summary(lm(Nose~Foot,data=da)))
abline(lsfit(x=da$Nose,y=da$Foot),lwd=3)

print(summary(lm(Height~Foot+Nose,data=da)))
print(c(sd(da$Height),sd(da$Foot),sd(da$Nose)))

#Interpreting the intercept

f=da$Foot-mean(da$Foot);n=da$Nose-mean(da$Nose)
h=da$Height
print(summary(lm(h~f+n,data=da)))

}
