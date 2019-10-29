SCcancer <-
function()
{
dump("SCcancer","c:\\StatBook\\SCcancer.r")
y=read.csv(file="c:\\StatBook\\DeathYears.csv",header=F)[,1]
y=y[order(y)]
n=length(y)
Fv=seq(from=1,to=0,length=n)
par(mfrow=c(1,1),mar=c(4,4,1,1))
plot(y,Fv,type="s",xlab="Time to death, years",ylab="Probability",lwd=4)
aa=abs(Fv-.5)
ymed=mean(y[aa==min(aa)])
lines(x=c(-1,ymed,ymed),y=c(.5,.5,-1))
theta=mean(y)
lines(y,exp(-y/theta),col=2)
text(9,.5,paste("Median survival =",round(ymed,1),"years"),cex=1.5)
}
