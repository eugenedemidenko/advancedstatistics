bufprobSA <-
function(prop.given,Nsim=1000)
{
dump("bufprobSA","c:\\StatBook\\bufprobSA.r")
prop.est=function(ro,prop.given,N)
{
	nc=runif(N);ang=pi*runif(N)
	y0=-ro/2*sin(ang)+nc;y1=ro/2*sin(ang)+nc
	pr=mean(y0<0 | y0>1 | y1<0 | y1>1)			
	return(pr-prop.given)
}

ro.ML=uniroot(f=prop.est,interval=c(0.01,10),prop.given=prop.given,N=Nsim)$root
return(ro.ML)
}
