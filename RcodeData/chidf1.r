chidf1 <-
function(mu=1)
{
dump("chidf1","c:\\StatBook\\chidf1.r")
x=seq(from=0,to=5,length=100)
Fs=pnorm(sqrt(x)-mu)-pnorm(-sqrt(x)-mu)
pc=pchisq(q=x,df=1,ncp=mu^2)
matplot(x,cbind(Fs,pc),col=1,type="l",lty=1)
cbind(Fs,pc)
}
