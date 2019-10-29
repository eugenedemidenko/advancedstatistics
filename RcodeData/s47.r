s47 <-
function(K=200,N=1000,d=.05)
{
    dump("s47","c:\\StatBook\\s47.r")
    par(mfrow=c(1,1),mar=c(.5,.5,3.25,.5))
    dN=round(d*N)
    M=matrix(0,ncol=N,nrow=N)
    plot((-dN):(N+dN),(-dN):(N+dN),type="n",axes=F,xlab="",ylab="")
    IN=1:N
    for(i in 1:K)
    {
        x=runif(min=0,max=N,n=1);y=runif(min=0,max=N,n=1)
        polygon(x=c(x-dN,x-dN,x+dN,x+dN,x-dN),y=c(y-dN,y+dN,y+dN,y-dN,y-dN),
                            col=7,lwd=2) 
        x1=max(1,x-dN);x2=min(N,x+dN)
        y1=max(1,y-dN);y2=min(N,y+dN)
        M[IN>=x1 & IN<=x2,IN>=y1 & IN<=y2]=1
    }
    lines(x=c(0,0,N,N,0),y=c(0,N,N,0,0),lwd=4,col=2)
    cov.pr=round(sum(M)/N^2*100,1)
    title(paste(K," throws of random squares of length ",d," 
          on the unit square\ncoverage=",cov.pr,"%",sep=""))
}
