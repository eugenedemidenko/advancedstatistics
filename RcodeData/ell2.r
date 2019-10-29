ell2 <-
function(ro=.7,const=1)
{
dump("ell2","c:\\StatBook\\ell2.r")
par(mfrow=c(1,1),mar=c(2,2,1,2))
zex=sqrt(const/(1-ro^2))
z1=seq(from=-zex,to=zex,length=200)
DI=const-(1-ro^2)*z1^2
z2Pl=ro*z1+sqrt(DI);z2Mi=ro*z1-sqrt(DI)
matplot(z1,cbind(z2Pl,z2Mi),type="l",col=1,lty=1,lwd=3,
                         axes=F,xlab="",ylab="")
segments(0,-10,0,10);segments(-10,0,10,0)
lines(x=c(-zex,-zex,zex,zex,-zex),y=c(-zex,zex,zex,-zex,-zex))
segments(-10,-10,10,10,lty=2,lwd=2)
segments(-10,10,10,-10,lty=2,lwd=2)
segments(-zex,-ro*zex,zex,ro*zex,lwd=2)
points(-zex,-ro*zex,pch=16,cex=2);points(zex,ro*zex,pch=16,cex=2)
segments(-ro*zex,-zex,ro*zex,zex,lwd=2,lty=3)
points(-ro*zex,-zex,pch=1,cex=2);points(ro*zex,zex,pch=1,cex=2)
maxEL=sqrt(const/(1-ro)/2);points(maxEL,maxEL,pch=3,cex=2)
points(-maxEL,-maxEL,pch=3,cex=2)
minEL=sqrt(const/(1+ro)/2);points(-minEL,minEL,pch=5,cex=2)
points(minEL,-minEL,pch=5,cex=2)

}
