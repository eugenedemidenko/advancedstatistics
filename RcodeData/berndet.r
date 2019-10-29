berndet <-
function(prob=.5,nExp=10000,eps=0.0000000001)
{
dump("berndet","c:\\StatBook\\berndet.r")
deti=rep(NA,nExp)
pr=rep(NA,9)
for(n in 2:10)
{
	for(ix in 1:nExp)
	{
		mel=sample(x=c(0,1),size=n^2,replace=T,prob=c(prob,1-prob))
		A=matrix(mel,ncol=n,nrow=n)
		deti[ix]=det(A)
	}
	pr[n-1]=mean(abs(deti)<eps)
}
plot(2:10,pr,type="b",xlab="Matrix order, n",ylab="Probability det=0")
title(paste("Probability that det=0 for a Bernoulli randon matrix nxn\nprob =",prob))
}
