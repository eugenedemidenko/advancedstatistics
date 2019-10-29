leftright <-
function(n=173,st=6)
{
dump("leftright","c:\\StatBook\\leftright.r")
par(mfrow=c(1,2),mar=c(4,4,3,1),cex.main=2)
#set.seed(st)
#trueL=rnorm(n=n,mean=46,sd=2)
#right=trueL+rnorm(n=n,mean=0,sd=.5)
#left=trueL+rnorm(n=n,mean=0,sd=.5)
#write.csv(x=round(cbind(left,right),1),"c:\\StatBook\\leftright.csv",row.names=F)
d=read.csv("c:\\StatBook\\leftright.csv",header=T)
right=d$right;left=d$left

plot(left,right,xlab="",ylab="",main="Right arm versus left arm")
mtext(side=1,"Left arm, cm",cex=1.75,line=2.75)
mtext(side=2,"Right arm, cm",cex=1.75,line=2.5)
abline(lsfit(y=right,x=left),lwd=3)
o=lm(right~left)
text(42,50,paste("Slope =",round(coef(o)[2],3)),adj=0,cex=1.75)
print(summary(o))
print(summary(lm(right~left-1)))

plot(right,left,xlab="",ylab="",main="Left arm versus right arm")
mtext(side=1,"Right arm, cm",cex=1.75,line=2.75)
mtext(side=2,"Left arm, cm",cex=1.75,line=2.5)
abline(lsfit(y=left,x=right),lwd=3)
o=lm(left~right)
text(42,50,paste("Slope =",round(coef(o)[2],3)),adj=0,cex=1.75)
print(summary(o))
print(summary(lm(left~right-1)))


t.test(x=right,y=left, paired=T)
}
