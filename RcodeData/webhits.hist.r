webhits.hist <-
function()
{
dump("webhits.hist","c:\\statbook\\webhits.hist.r")
y<-scan("c:\\statbook\\comwebhits.dat")
n=length(y)
par(mfrow=c(1,2),mar=c(4,4,2,.1))
hist(y,xlim=c(0,24),xlab="Time of the website hits, h",main="Default histogram",col="gray95")
segments(y,rep(-1,n),y,rep(1,n))
br=seq(from=5,to=24,by=1)
nbins=length(br)
hist(y,xlim=c(5,25),breaks=br,probability=T,xlab="Time of the website hits, h",ylab="Probability",main=paste("Hour histogram with",nbins,"bins"),col="gray95")
segments(y,rep(-1,n),y,rep(.007,n))
print(sum(hist(y)$density))
}
