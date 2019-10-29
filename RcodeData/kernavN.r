kernavN <-
function(n=20,h=.1)
{
dump("kernavN","c:\\StatBook\\kernavN.r")
par(mfrow=c(1,1),mar=c(4,4,1,1))
x=rnorm(n=n,mean=1,sd=2)
low=min(x)-1;up=max(x)+1
plot(x,x,type="n",xlim=c(low,up),ylim=c(0,1),xlab="Data, x",ylab="Density")
rug(x,col=3,lwd=3)
xx=seq(from=low,to=up,length=500)
N=length(xx)
kentd=rep(0,N)
for(i in 1:n) 
{
	di=dnorm(xx,mean=x[i],sd=h)
	kentd=kentd+di/n
	lines(xx,di)
}
lines(xx,kentd,lwd=3,col=2)
dbi=density(x)
print(cbind(dbi$x,dbi$y,n.density.my(x.data=x,x=dbi$x,h=dbi$bw)))
lines(dbi,col=4,lwd=3)
legend(low,1,c("Actual data","The i-th density",paste("The kernel density, h=",h,sep=""),paste("The kernel built-in density with h=",round(dbi$bw,2),sep="")),lty=1,lwd=c(3,1,3,3),col=c(3,1,2,4),bg="gray96")
}
