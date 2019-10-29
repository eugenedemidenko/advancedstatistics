nile <-
function(yc=1898)
{
dump("nile","c:\\StatBook\\nile.r")
d=read.csv("c:\\StatBook\\NileFlow.csv")
year=d[,2];flow=d[,3]
par(mfrow=c(1,1),mar=c(3.5,3.5,1,.1))
plot(year,flow,type="b",axes=F,xlab="",ylab="",ylim=c(450,1350),lwd=2)
mtext(side=1,"Years",cex=1.5,line=2.3)
mtext(side=2,"Nile flow",cex=1.5,line=2.3)
mtext(side=3,"Annual volume of the Nile River discharged at Aswan",cex=1.5,font=2,line=-.5)
axis(side=1,c(seq(from=1871,to=1970,by=10),1971))
axis(side=2,seq(from=450,to=1350,by=100))
segments(yc,400,yc,1400,lty=2)
n=length(year)
d=rep(0,n);d[year<=yc]=1 # dummy variable
# After yc flow is constant, before yc flow has a trend
o=lm(flow~I(d*year)) 
print(summary(o))
# The null model: the flow is constant
o=lm(flow~d)
print(summary(o))
fp=o$fitted.values
lines(year,fp,lwd=3)
text(yc+.5,1250,paste("Changepoint =",yc),cex=1.25,font=4,adj=0)
# The overspecified model
o=lm(flow~d+I(d*year))
print(summary(o))
}
