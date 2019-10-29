cauchy.theta <-
function(theta=1,n=50,itMAX=10,ss=3)
{
dump("cauchy.theta","c:\\StatBook\\cauchy.theta.r")
set.seed(ss)
Y=theta+rcauchy(n)
mY=median(Y)
th.NR=th.FS=th.EFS=rep(mY,itMAX)
LL=-n*log(pi)-sum(log(1+(Y-mY)^2))
LL.NR=LL.FS=LL.EFS=rep(LL,itMAX)
for(it in 2:itMAX)
{
	num=sum((Y-th.NR[it-1])/(1+(Y-th.NR[it-1])^2))
	den=sum((Y-th.NR[it-1]^2-1)/(1+(Y-th.NR[it-1])^2)^2)
	th.NR[it]=th.NR[it-1]-num/den
	LL.NR[it]=-n*log(pi)-sum(log(1+(Y-th.NR[it])^2))
	
	num=sum((Y-th.FS[it-1])/(1+(Y-th.FS[it-1])^2))
	th.FS[it]=th.FS[it-1]+4/n*num
	LL.FS[it]=-n*log(pi)-sum(log(1+(Y-th.FS[it])^2))
	
	der=(Y-th.EFS[it-1])/(1+(Y-th.EFS[it-1])^2)
	num=sum(der);den=sum(der^2)
	th.EFS[it]=th.EFS[it-1]+.5*num/den
	LL.EFS[it]=-n*log(pi)-sum(log(1+(Y-th.EFS[it])^2))
}
print(cbind(LL.NR,LL.FS,LL.EFS))
print(cbind(th.NR,th.FS,th.EFS))
par(mfrow=c(1,2),mar=c(4,4,1,1))
matplot(1:itMAX,cbind(LL.NR,LL.FS,LL.EFS),col=1,type="l",lwd=3,xlab="",ylab="")
mtext(side=1,"Iteration index",line=2.75,cex=1.25)
mtext(side=2,"Log-likelihood",line=2.75,cex=1.25)
legend(6,-128.52,c("NR","FS","EFS"),lty=1:3,lwd=3,cex=1.25,bg="gray90")

matplot(1:itMAX,cbind(th.NR,th.FS,th.EFS),col=1,type="l",lwd=3,xlab="",ylab="")
mtext(side=1,"Iteration index",line=2.75,cex=1.25)
mtext(side=2,"Theta estimate",line=2.75,cex=1.25)
legend(6,1.27,c("NR","FS","EFS"),lty=1:3,lwd=3,cex=1.25,bg="gray90")


}
