eppendorf <-
function()
{
dump("eppendorf","c:\\StatBook\\eppendorf.r")
x=scan("c:\\StatBook\\eppendorf.txt");n=length(x)
par(mfrow=c(1,1),mar=c(4.25,4.1,1,1),cex.lab=1.5)
hist(x,xlab="Rat brain PtO2, mm Hg",pr=T,main="",col="gray94")
segments(x,rep(-1,n),x,rep(.002,n))
yd=density(x)
lines(yd$x,yd$y,lwd=3)
yd5=density(x,bw=5)
lines(yd5$x,yd5$y,lwd=3,lty=2)
dfL=paste("Default bw=",round(yd$bw,1),sep="")
legend(42,.034,c(dfL,"bw=5"),lwd=3,lty=1:2,cex=1.25)
}
