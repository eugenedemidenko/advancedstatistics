pmaxmin <-
function(n=10,N=100000)
{
dump("pmaxmin","c:\\StatBook\\pmaxmin.r")
X=matrix(runif(n*N),ncol=n)
Xn=apply(X,1,max)
X1=apply(X,1,min)
Y=Xn-X1
Y=Y[order(Y)]
plot(Y,(1:N)/N,type="l",xlab="max-min",ylab="Probability",lty=2)
title(paste("Two cdfs, n=",n,", N=",N,sep=""))
xx=seq(from=Y[1],to=Y[N],length=100)
lines(xx,pbeta(xx,n-1,2))
legend(.3,1,c("Empirical cdf","Theoretical B(n-1,2) cdf"),lty=c(2,1))
}
