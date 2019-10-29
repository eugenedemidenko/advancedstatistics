QQmurder <-
function()
{
dump("QQmurder","c:\\statbook\\QQmurder.r")
par(mfrow=c(1,2),mar=c(4,4,3,1),cex.main=1.3)
da=matrix(scan("c:\\statbook\\wikmurdr.txt",what=""),byrow=T,ncol=4)
N=nrow(da)
nam.state=da[,1]
n.state=as.numeric(da[,2])
murd.state=as.numeric(da[,3])
rate=murd.state/n.state*1000000
nam.state=nam.state[order(rate)]
rate=rate[order(rate)]
plot((1:N)/N,rate,xlab="Theoretical quantile of the R(0,1) distribution",ylab="Murders per million",main=paste("Q-q plot for murder rates in",N,"states"))
text(.99,rate[N],nam.state[N],pos=2)
N1=N-1
plot((1:N1)/N1,rate[1:N1],xlab="Theoretical quantile of the R(0,1) distribution",ylab="Murders per million",main=paste("Q-q plot for murder rates in",N1,"states"))
}
