n.density.my <-
function(x.data,x,h)
{
	dump("n.density.my","c:\\StatBook\\n.density.my.r")
	n=length(x.data);m=length(x)
	dif=matrix(rep(x,times=n)-rep(x.data,each=m),nrow=m)
	my.d=rowMeans(dnorm(dif,mean=0,sd=h))
	my.d
}
