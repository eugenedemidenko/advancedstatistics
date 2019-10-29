ciumax <-
function(theta=1,n=5,lambda=.95,nSim=1000000)
{
	dump("ciumax","c:\\StatBook\\ciumax.r")
	X=matrix(runif(nSim*n,min=0,max=theta),ncol=n)
	Xmax=apply(X,1,max)
	mean(theta>Xmax/lambda^(1/n))
}
