genmixN <-
function(mu1=1,mu2=2,p=.3,N=1000)
    {
       dump("genmixN","c:\\StatBook\\genmixN.r")
       x=c(rnorm(N*p,mean=mu1,sd=1),rnorm(N*(1-p),mean=mu2,sd=1))
       x=x[order(x)]
       NN=length(x)
       Fv=(1:N)/N
       plot(x,Fv,type="s")
       xf=seq(from=x[1],to=x[N],length=100)
       lines(xf,p*pnorm(xf,mean=mu1,sd=1)+(1-p)*pnorm(xf,mean=mu2,sd=1),col=2)
       legend(x[1],1,c("Empirical cdf","Theoretical mixture cdf"),col=1:2,lty=1)
    }
