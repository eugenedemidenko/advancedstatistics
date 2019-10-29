cancgr <-
function(p=.501,y0=100,T=365,nSim=100)
    {
dump("cancgr","c:\\StatBook\\cancgr.r")
    t=1:T
    Y=matrix(0,nrow=T+1,ncol=nSim)
    for(isim in 1:nSim)
    {
     Y[1,isim]=y0
     for(t in 1:T)
     Y[t+1,isim]=2*rbinom(n=1,size=Y[t,isim],prob=p)
    }
    matplot(0:T,Y,type="s",col=1,lty=1,xlab="",ylab="")
    mtext(side=1,"Time, days",line=2.75,cex=2)
    mtext(side=2,"Tumor size",line=2.5,cex=2)
    lines(0:T,y0*(2*p)^(0:T),col=2,lwd=5)
    segments(-1,1000,T+1,1000,lty=2,lwd=3)
    text(50,1080,"Death",font=2,cex=1.5)
    }
