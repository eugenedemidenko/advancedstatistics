filelXY <-
function(mu=3,nExp=100000,a=10,nbins=100)
{
    dump("filelXY","c:\\StatBook\\filelXY.r")
    XYdens=function(z,mu)
    {
        e1=exp(-mu^2*z^2/2/(z^2+1))
        e2=mu/sqrt(2*pi)/(z^2+1)^1.5
        e3=2*pnorm(mu/sqrt(z^2+1))-1
        e4=exp(-mu^2/2)/pi/(z^2+1)
        return(e1*e2*e3+e4)
    }
    par(mfrow=c(1,2),mar=c(4,4,3,1))
    X=rnorm(nExp);Y=rnorm(nExp,mean=mu)
    r=X/Y;r=r[order(r)];r=r[abs(r)<a]
    z=seq(from=min(r),to=max(r),length=100)
    hist(r,nclass=nbins,probability=T,xlab="simulated X/Y",main="")
    title(paste("Histogram with nbins=",nbins,sep=""))
    lines(z,XYdens(z,mu=mu),lwd=3,col=2)
    zi=seq(from=-a,to=a,length=20);
	cdf=rep(NA,20)
    n=length(r)
    for(i in 1:20)
	cdf[i]=integrate(XYdens,mu=mu,lower=-2,upper=zi[i])$value    
    plot(r,(1:n)/n,type="s",xlab="simulated X/Y",ylab="Probability",main=paste("cdf of X/Y with a=",a,sep=""))
    points(zi,cdf)    
}
