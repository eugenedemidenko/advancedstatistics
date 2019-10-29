dn2 <-
function(n=100,muX=1,sdX=2,muY=1,sdY=2)
    {
     dump("dn2","c:\\statbook\\dn2.r")
     x<-seq(from=muX-3*sdX,to=muX+3*sdX,length=n)
     y<-seq(from=muY-3*sdY,to=muY+3*sdY,length=n)
     zx=rep((x-muX)/sdX,times=n);zy=rep((y-muY)/sdY,each=n)
     par(mfrow=c(1,3),mar=c(3,3,3,1))
     xreg=seq(from=muX-3*sdX,to=muX+3*sdX,length=100)
     for(ro in c(-.7,0,.7))
     {
        coef=1/(2*pi)/sdX/sdY/sqrt(1-ro^2)
        qz=zx^2-2*ro*zx*zy+zy^2
        fxy=matrix(coef*exp(-qz/2/(1-ro^2)),ncol=n,nrow=n)
        contour(x,y,fxy,main=paste("ro =",ro))
        yreg=muY+ro*sdY/sdX*(xreg-muX)
        lines(xreg,yreg,lwd=3)
             }
    }
